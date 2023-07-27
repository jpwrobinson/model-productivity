############################################################################
##                                                                        ##
## Functions to extract traits from Fishbase (https://www.fishbase.se)    ##
## to compute micronutrient content following                             ##
## Hicks et al. (2019) https://www.nature.com/articles/s41586-019-1592-6  ##
## E. MAIRE - LEC - e.maire@lancaster.ac.uk                               ##
## Version: May 2021                                                      ##
##                                                                        ##
############################################################################
library(rfishbase)
library(curl)
library(rvest)

# helper scraping function
scraplinks <- function(url){
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    # Extract the link text
    link_ <- webpage %>%
      rvest::html_nodes("a") %>%
      rvest::html_text()
    return(tibble(link = link_, url = url_))
  }

#Taxonomy
getTaxo <- function(sp,tax){
  
  #Check Names
  Species <- sp
  Species_corrected <- rep("NA",length(sp))
  SpecCode <- rep("NA",length(sp))
  
  for(k in 1:length(Species)){
    test <- validate_names(Species[k])
    print(paste(k, ':', Species[k])) ## print species name for debugging
    if(length(test)==1){
      Species_corrected[k] <- test
      SpecCode[k] <- as.numeric(unique(rfishbase::species(test,fields="SpecCode")$SpecCode))
    }else{
      Species_corrected[k] <- test[k]
      SpecCode[k] <- as.numeric(unique(rfishbase::species(test[k],fields="SpecCode")$SpecCode))
      next
    }
    
    ## validate names can return NA for species in species table, so adding in speccode for these:
    if(is.na(test)){
      Species_corrected[k] <- Species[k]
      SpecCode[k] <- species(Species[k])$SpecCode
    }
    
  }#end of k
  
  correctNames <- data.frame(Species,Species_corrected,SpecCode)

  sp_data<-left_join(correctNames, 
                     as.data.frame(tax) %>% 
                       select(all_of(Genus, Family, Order, Class, Species_corrected)), by = 'Species_corrected')
  
  return(sp_data)
  
}#end of function getTaxo

###################################################
#EnvTemp  
getEnvTemp <- function(sp_data) {
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  EnvTemp <- rep("Unknown",length(sp))
  EnvTempLevel <- rep("species",length(sp))
  
  for(i in 1:length(sp)){
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    html <- readLines(urly)
    
    #reduce html to environment section
    env <- grep("Environment", html)
    
    short <- html[seq(env[1],env[1]+10,1)]
    
    #Identify good section and extract the environment
    tro <- grep("Tropical", short)
    subt <- grep("Subtropical", short)
    deep <- grep("Deep-water", short)
    polar <- grep("Polar", short)
    temp <- grep("Temperate", short)
    boreal <- grep("Boreal", short)
    
    if(length(tro)==1){ EnvTemp[i] <- "tropical" }
    if(length(subt)==1){ EnvTemp[i] <- "subtropical" }
    if(length(polar)==1){ EnvTemp[i] <- "polar_deep" }
    if(length(deep)==1){ EnvTemp[i] <- "polar_deep" }
    if(length(temp)==1){ EnvTemp[i] <- "temperate" }
    if(length(boreal)==1){ EnvTemp[i] <- "boreal" }
    
  } #end of i
  
  en <- data.frame(sp,EnvTemp,EnvTempLevel)
  names(en)[1] <- "Species_corrected"
  
  res <- merge(sp_data,en,by="Species_corrected",all.x=T) 
  
  return(res)  
  
}#end of function

################################################
#Trophic level
getTrophicLevel<-function(sp_data){
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  trophic_level <- vector()
  trophic_levelLevel <- rep("species",length(sp))
  
  for(i in 1:length(sp)) {
    cat("i",i,"\n")
    # Grab species page
    
    urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    
    #Extract URLs and Links
    glinks <- scraplinks(urly)
    idlink <- which(glinks$link == "Life-history tool")
    
    url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
    
    html <- read_html(url)
    
    #Convert to text
    tr_text <- html %>%
      html_nodes("tr") %>%
      html_text()
    
    #Identify trophic level section and extract tl value
    id <- grep("Trophic", tr_text)
    
    if(length(id)==0){
      trophic_level[i] <- NA
      trophic_levelLevel[i] <- NA
    }else{
      #Delete multiples spaces etc.
      tlraw <- gsub("\r|\n|\t", " ", tr_text[id])
      tlclean <- gsub("\\s+", " ", str_trim(tlraw))
      tlval <- strsplit(tlclean," ")[[1]]
      
      #Save trophic level value
      trophic_level[i] <- as.numeric(tlval[3])
    }
    
  }#end of i
  
  
  #Complete missing values from rfishbase (tl = 0 or NA)
  #1-Check if tl is available at species level and complete with genus and family level
  nas <- c(which(is.na(trophic_level)==T),which(trophic_level==0))
  
  if(length(nas>0)){
    
    for(k in 1:length(nas)){
      
      genus <- word(sp[nas[k]],1)
      fish <- species_list(Genus = genus)
      
      a <- ecology(fish, fields = c("DietTroph"))
      a <- as.matrix(a)
      
      if(is.na(mean(as.numeric(a),na.rm=T))==F) {  
        trophic_level[nas[k]] <- round(mean(as.numeric(a),na.rm=T),2)
        trophic_levelLevel[nas[k]] <- "genus"
      }else{
        fam <- sp_data$Family[which(sp_data$Species_corrected==sp[nas[k]])]
        fish <- species_list(Family = fam)
        a <- ecology(fish, fields = c("DietTroph"))
        a <- as.matrix(a)
        trophic_level[nas[k]] <- round(mean(as.numeric(a),na.rm=T),2)
        trophic_levelLevel[nas[k]] <- "family"
      } #end of else
      
      
    } #end of k
    
    tl <- data.frame(sp,trophic_level,trophic_levelLevel)
    names(tl)[1] <- "Species_corrected"
    
    res <- merge(sp_data,tl,by="Species_corrected",all.x=T)
    
  }else{
    
    tl <- data.frame(sp,trophic_level,trophic_levelLevel)
    names(tl)[1] <- "Species_corrected"
    
    res <- merge(sp_data,tl,by="Species_corrected",all.x=T) 
    
  } 
  
  return(res) 
  
}#end of getTrophicLevel

###################################################
#Body Shape
getBodyShape <- function(sp_data){
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  BodyShape <- vector()
  BodyShapeLevel <- rep("species",length(sp))
  
  for(i in 1:length(sp)){
    cat("i",i,"\n")
    body <- species(sp[i],fields=c("BodyShapeI"))
    BodyShape[i] <- as.character(as.data.frame(body))
  }#end of i
  
  #Complete missing values for body shape
  nas <- which(is.na(BodyShape)==T)
  
  for(l in 1:length(nas)){
    
    genus <- word(sp[nas[l]],1)
    fish <- species_list(Genus = genus)
    
    a <- species(fish, fields = c("BodyShapeI"))
    x = tolower(a$BodyShapeI)
    x = names(which.max(table(x)))
    
    if (is.null(x)==TRUE){
      BodyShape[nas[l]] <- "Unknown"
      BodyShapeLevel[nas[l]] <- "Unknown"
    }else{
      BodyShape[nas[l]] <- x
      BodyShapeLevel[nas[l]] <- "genus"
    }
  }#end of l
  
  BodyShape[grep("short", BodyShape)] <- "short_deep"
  BodyShape[grep("fusiform", BodyShape)] <- "fusiform"
  BodyShape[grep("elongated", BodyShape)] <- "elongate"
  
  bs <- data.frame(sp,BodyShape,BodyShapeLevel)
  names(bs)[1] <- "Species_corrected"
  
  res <- merge(sp_data,bs,by="Species_corrected",all.x=T) 
  
  return(res)
}#end of function


###################################################
#Max Depth (web scraping)
getMaxDepth <- function(sp_data) {
  
  #Delete missing species and create empty vector to fill in
  
  sp <- unique(sp_data$Species_corrected)
  
  DepthRangeDeep <- vector()
  DepthRangeDeepLevel <- rep("species",length(sp))
  
  for(i in 1:length(sp)){
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Convert to text
    depth_text <- read_html(urly) %>% 
      html_nodes(".smallSpace") %>%
      html_text()
    
    #Identify depth range section and extract max depth value if provided
    depth <- grep("depth range", depth_text)
    if(length(depth)>=1){ #Usually mentionned once but for sure the right information is described in the first text part
      depthraw <- gsub("\r|\n|\t", " ", depth_text[depth[1]])
      depthclean <- gsub("\\s+", " ", str_trim(depthraw))
      depthind <- strsplit(depthclean," ")[[1]]
      range <- which(depthind=="range")
      #maxd <- as.numeric(depthind[range+3])
      
      DepthRangeDeep[i] <- as.numeric(depthind[range+3])
    }else{
      DepthRangeDeep[i] <- NA
      DepthRangeDeepLevel[i] <- "higher"
    }
  } #end of i
  
  #Complete missing values 
  #1-Check if tl is available at species level and complete with genus and family level
  nas <- which(is.na(DepthRangeDeep)==T)
  
  if(length(nas>0)){
    
    for(k in 1:length(nas)){
      
      genus <- word(sp[nas[k]],1)
      fish <- species_list(Genus = genus)
      
      a <- species(fish, fields = c("DepthRangeDeep"))
      a <- as.matrix(a)
      
      if(is.na(mean(as.numeric(a),na.rm=T))==F) {  
        DepthRangeDeep[nas[k]] <- round(mean(as.numeric(a),na.rm=T),2)
        DepthRangeDeepLevel[nas[k]] <- "genus"
      }else{
        fam <- sp_data$Family[which(sp_data$Species_corrected==sp[nas[k]])]
        fish <- species_list(Family = fam)
        a <- species(fish, fields = c("DepthRangeDeep"))
        a <- as.matrix(a)
        DepthRangeDeep[nas[k]] <- round(mean(as.numeric(a),na.rm=T),2)
        DepthRangeDeepLevel[nas[k]] <- "family"
      } #end of else
      
    } #end of k
    
    Maxdepth <- data.frame(sp,DepthRangeDeep,DepthRangeDeepLevel)
    names(Maxdepth)[1] <- "Species_corrected"
    
    res <- merge(sp_data,Maxdepth,by="Species_corrected",all.x=T)
    
  }else{
    
    Maxdepth <- data.frame(sp,DepthRangeDeep,DepthRangeDeepLevel)
    names(Maxdepth)[1] <- "Species_corrected"
    
    res <- merge(sp_data,Maxdepth,by="Species_corrected",all.x=T)
    
    return(res)
  }  
  
}#end of function


###################################################
#DemersPelag (web scraping)

getDemersPelag <- function(sp_data) {
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  DemersPelagLevel <- rep("species",length(sp))
  DemersPelag <- vector()
  
  for(i in 1:length(sp)){
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Convert to text
    dempel_text <- read_html(urly) %>% 
      html_nodes(".smallSpace") %>%
      html_text()
    
    #Identify Environment section and extract habitat if provided
    dempel.m <- grep("Marine", dempel_text)
    dempel.f <- grep("brackish", dempel_text)
    dempel <- unique(c(dempel.m,dempel.f))
    if(length(dempel)>=1){ #Usually mentioned once but for sure the right information is described in the first text part
      dempelraw <- gsub("\r|\n|\t", "", dempel_text[dempel[1]])
      dempelint <- gsub("\\;", "", str_trim(dempelraw))
      dempelclean <- gsub("\\s+", " ", dempelint)
      dempelind <- strsplit(dempelclean," ")[[1]]
      
      #Extract the environment
      pel <- grep("pel", dempelind)
      dem <- grep("dem", dempelind)
      reef <- grep("reef", dempelind)
      
      uniq <- unique(c(pel,dem,reef))
      
      if(length(uniq)==1){
        DemersPelag[i] <-  str_replace(dempelind[uniq],"[[.]]", "") 
      }else{
        DemersPelag[i] <- NA
        DemersPelagLevel[i] <- "higher"
      }
      
    }else{
      DemersPelag[i] <- NA
      DemersPelagLevel[i] <- "higher"
    }
  } #end of i
  
  #Clean outputs 
  DemersPelag <-  str_replace(DemersPelag,"[[,]]", "") 
  
  DP <- data.frame(sp,DemersPelag,DemersPelagLevel)
  names(DP)[1] <- "Species_corrected"
  
  res <- merge(sp_data,DP,by="Species_corrected",all.x=T)
  
  return(res)
  
}#end of getDemersPelag

################################################
#getFeedingPathway

getFeedingPathway<-function(sp_data, Diet, Food){
  
  #Delete missing species and create empty vector to fill in
  
  sp <- unique(sp_data$Species_corrected)
  
  #Get info from the 'Diet' and 'Food items' sections
  Feeding_path_Diet <- vector()
  Feeding_pathLevel_Diet <- rep("species",length(sp))
  
  Feeding_path_Food <- vector()
  Feeding_pathLevel_Food <- rep("species",length(sp))
  
  for(i in 1:length(sp)) {
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Extract URLs and Links
    glinks <- scraplinks(urly)
    idlinkDiet <- which(glinks$link == "Diet")
    idlinkFood <- which(glinks$link == "Food items")
    
    # Run on Food items
    if(length(idlinkFood)==1){ 
      
      idlink <- idlinkFood  #open the Food Items section (more specific)
      
      # fix url leading ".." #Correction in December 2020 (J. Robinson, LEC)
      #url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
      idurl<-str_replace_all(glinks$url[idlink], '\\.\\.', '')
      url <- paste("https://www.fishbase.se",idurl,sep="")
      
      html <- read_html(url)
      
      tab <- rvest::html_table(html)[[1]]
      
      Foodshort <- unique(Food[,c("Food_I","Feeding_pathway")])
      feedpath <- merge(tab,Foodshort,by.x="Food I",by.y="Food_I",all.x=T)
      
      #Identity Adult Diet
      feedpathA <- feedpath[grep("adult", feedpath$`Predator Stage`),]
      
      x = names(sort(table(feedpathA$Feeding_pathway),decreasing=TRUE)[1])
      
      if(length(x)==1){
        Feeding_path_Food[i] <- x
        Feeding_pathLevel_Food[i] <- "species"
      }else{
        Feeding_path_Food[i] <- NA
        Feeding_pathLevel_Food[i] <- "higher"
      }
      
    }else{
      Feeding_path_Food[i] <- NA
      Feeding_pathLevel_Food[i] <- "higher"
    } #end of idlinkFood  
    
    
    # Run on Diet
    if(length(idlinkDiet)==1){ 
      
      idlink <- idlinkDiet  #open the Diet section
      
      # fix url leading ".." #Correction in December 2020 (J. Robinson, LEC)
      #url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
      idurl<-str_replace_all(glinks$url[idlink], '\\.\\.', '')
      url <- paste("https://www.fishbase.se",idurl,sep="")
      
      html <- read_html(url)
      
      tab <- rvest::html_table(html)[[1]]
      
      Dietshort <- unique(Diet[,c("Functional_group_I","Feeding_pathway")])
      feedpath <- merge(tab,Dietshort,by.x="Main Food",by.y="Functional_group_I",all.x=T)
      
      #Identity Adult Diet
      feedpathA <- feedpath[grep("adult", feedpath$`Predator Life Stage`),]
      
      if(dim(feedpathA)[1]>0){
        stat <- feedpathA %>%
          group_by(Feeding_pathway) %>%
          summarise(food = sum(Percent))
        
        #total percentage must be >50
        control <- stat$food[which.max(stat$food)]
        if(control>50){
          x = stat$Feeding_pathway[which.max(stat$food)] 
        }else{
          x <- NA
        }
        
        if(length(x)==1){
          Feeding_path_Diet[i] <- x
          Feeding_pathLevel_Diet[i] <- "species"
        }else{
          Feeding_path_Diet[i] <- NA
          Feeding_pathLevel_Diet[i] <- "higher"
        }
      }else{
        Feeding_path_Diet[i] <- NA
        Feeding_pathLevel_Diet[i] <- "higher"
      }
      
    }else{
      Feeding_path_Diet[i] <- NA
      Feeding_pathLevel_Diet[i] <- "higher"
    } #end of idlinkDiet
    
    #No data available
    if(length(idlinkDiet)==0 & length(idlinkFood)==0){
      
      Feeding_path_Diet[i] <- NA
      Feeding_pathLevel_Diet[i] <- "higher"
      
      Feeding_path_Food[i] <- NA
      Feeding_pathLevel_Food[i] <- "higher"
    }
    
  }#end of i
  
  feedP <- data.frame(sp,Feeding_path_Diet,Feeding_pathLevel_Diet,Feeding_path_Food,Feeding_pathLevel_Food)
  names(feedP)[1] <- "Species_corrected"
  
  #Compute final feeding pathway variable + Level and prioritize 'Diet' because more accurate if available
  feedP$Feeding_path <- feedP$Feeding_path_Diet
  feedP$Feeding_pathLevel <- feedP$Feeding_pathLevel_Diet
  
  #But if Feeding_path_Diet == NA replace by Feeding_path_Food
  feedP$Feeding_path[which(is.na(feedP$Feeding_path_Diet)==T)] <- feedP$Feeding_path_Food[which(is.na(feedP$Feeding_path_Diet)==T)] 
  feedP$Feeding_pathLevel[which(is.na(feedP$Feeding_path_Diet)==T)] <- feedP$Feeding_pathLevel_Food[which(is.na(feedP$Feeding_path_Diet)==T)]
  
  #Fill in missing values with genus and family level information ONLY!
  dat <- merge(sp_data,feedP,by="Species_corrected",all.x=T)
  nas <- which(is.na(dat$Feeding_path)==T)
  
  for(n in 1:length(nas)){
    genus <- dat$Genus[nas[n]]
    val <- dat$Feeding_path[which(dat$Genus==genus)]
    value <- names(sort(table(as.factor(val[-which(is.na(val)==T)])),decreasing=TRUE)[1])
    
    if(is.null(value)==TRUE){
      fam <- dat$Family[nas[n]]
      val <- dat$Feeding_path[which(dat$Family==fam)]
      value <- names(sort(table(as.factor(val[-which(is.na(val)==T)])),decreasing=TRUE)[1])
      if (is.null(value)==TRUE){
        dat$Feeding_path[nas[n]] <- NA
        dat$Feeding_pathLevel[nas[n]] <- "higher" # users can attribute a value 
      }else{
        dat$Feeding_path[nas[n]] <- value
        dat$Feeding_pathLevel[nas[n]] <- "family"
      }
    }else{
      dat$Feeding_path[nas[n]] <- value
      dat$Feeding_pathLevel[nas[n]] <- "genus"
    }
    
  }#end of n
  
  #Make sure NAs are consistent
  if(length(which(is.na(dat$Feeding_path)==T))>length(which(is.na(dat$Feeding_pathLevel)==T))){
    dat$Feeding_pathLevel[which(is.na(dat$Feeding_path)==T)] <- NA
  }
  
  #Create final file
  res <- dat %>%
    select(Species_corrected,Species,Genus,Family,Order,Class,Feeding_path,Feeding_pathLevel)
  
  return(res) 
  
}#end of getFeedingPathway

################################################
#getMainFood

getMainFood<-function(sp_data, Diet, Food){
  
  #Delete missing species and create empty vector to fill in
  
  sp <- unique(sp_data$Species_corrected)
  
  #Get info from the 'Diet' and 'Food items' sections
  main_diet <- vector()
  main_diet_pathway <- vector()
  main_dietLevel <- rep("species",length(sp))
  
  main_food <- vector()
  main_food_pathway <- vector()
  main_foodLevel <- rep("species",length(sp))
  
  for(i in 1:length(sp)) {
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Extract URLs and Links
    glinks <- scraplinks(urly)
    idlinkDiet <- which(glinks$link == "Diet")
    idlinkFood <- which(glinks$link == "Food items")
    
    # Run on Food items
    if(length(idlinkFood)==1){ 
      
      idlink <- idlinkFood  #open the Food Items section (more specific)
      
      # fix url leading ".." #Correction in December 2020 (J. Robinson, LEC)
      #url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
      idurl<-str_replace_all(glinks$url[idlink], '\\.\\.', '')
      url <- paste("https://www.fishbase.se",idurl,sep="")
      
      html <- read_html(url)
      
      tab <- rvest::html_table(html)[[1]]
      
      #Identity Adult Diet
      feedpathA <- tab[grep("adult", tab$`Predator Stage`),]
      
      x = names(sort(table(feedpathA$`Food I`),decreasing=TRUE)[1])
      
      if(length(x)==1){
        main_food[i] <- x
        main_food_pathway[i] <- unique(Food$Feeding_pathway[which(Food$Food_I==x)])
        main_foodLevel[i] <- "species"
      }else{
        main_food[i] <- NA
        main_food_pathway[i] <- NA
        main_foodLevel[i] <- "higher"
      }
      
    }else{
      main_food[i] <- NA
      main_food_pathway[i] <- NA
      main_foodLevel[i] <- "higher"
    } #end of idlinkFood  
    
    
    # Run on Diet
    if(length(idlinkDiet)==1){ 
      
      idlink <- idlinkDiet  #open the Diet section
      
      # fix url leading ".." #Correction in December 2020 (J. Robinson, LEC)
      #url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
      idurl<-str_replace_all(glinks$url[idlink], '\\.\\.', '')
      url <- paste("https://www.fishbase.se",idurl,sep="")
      
      html <- read_html(url)
      
      tab <- rvest::html_table(html)[[1]]
      
      #Identity Adult Diet
      feedpathA <- tab[grep("adult", tab$`Predator Life Stage`),]
      
      if(dim(feedpathA)[1]>0){
        stat <- feedpathA %>%
          group_by(`Main Food`) %>%
          summarise(food = mean(Percent))
        
        #total percentage must be >50
        control <- stat$food[which.max(stat$food)]
        if(control>50){
          x = stat$`Main Food`[which.max(stat$food)] 
        }else{
          x <- NA
        }
        
        if(is.na(x)==F){
          main_diet[i] <- x
          main_diet_pathway[i] <- unique(Diet$Feeding_pathway[which(Diet$Functional_group_I==x)])
          main_dietLevel[i] <- "species"
        }else{
          main_diet[i] <- NA
          main_diet_pathway[i] <- NA
          main_dietLevel[i] <- "higher"
        }
      }else{
        main_diet[i] <- NA
        main_diet_pathway[i] <- NA
        main_dietLevel[i] <- "higher"
      }
      
    }else{
      main_diet[i] <- NA
      main_diet_pathway[i] <- NA
      main_dietLevel[i] <- "higher"
    } #end of idlinkDiet
    
    #No data available
    if(length(idlinkDiet)==0 & length(idlinkFood)==0){
      
      main_diet[i] <- NA
      main_diet_pathway[i] <- NA
      main_dietLevel[i] <- "higher"
      
      main_food[i] <- NA
      main_food_pathway[i] <- NA
      main_foodLevel[i] <- "higher"
    }
    
  }#end of i
  
  feedP <- data.frame(sp,main_food,main_foodLevel,main_food_pathway,main_diet,main_dietLevel,main_diet_pathway)
  names(feedP)[1] <- "Species_corrected"
  
  #Compute final feeding pathway variable + Level and prioritize 'Diet' because more accurate if available
  feedP$Feeding_path <- feedP$main_diet_pathway
  feedP$Feeding_pathLevel <- feedP$main_dietLevel
  
  #But if main_diet_pathway == NA replace by main_food_pathway
  feedP$Feeding_path[which(is.na(feedP$main_diet_pathway)==T)] <- feedP$main_food_pathway[which(is.na(feedP$main_diet_pathway)==T)] 
  feedP$Feeding_pathLevel[which(is.na(feedP$main_diet_pathway)==T)] <- feedP$main_foodLevel[which(is.na(feedP$main_diet_pathway)==T)]
  
  #Create final file
  res <- feedP
  
  return(res) 
  
}#end of getMainFood


###################################################
#environment: marine, freshwater, mixed (web scraping)

getEnvironment <- function(sp_data) {
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  marine <- vector()
  brackish <- vector()
  freshwater <- vector()
  
  for(i in 1:length(sp)){
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Convert to text
    full_text <- read_html(urly) %>% 
      html_nodes(".smallSpace") %>%
      html_text()
    
    #Identify Environment section and extract habitat if provided
    env <- gsub("\r|\n|\t", "", full_text[2])
    envlower <- tolower(env)
    
    env.m <- grep("marine", envlower)
    env.b <- grep("brackish", envlower)
    env.f <- grep("freshwater", envlower)
    
    if(length(env.m)==1) {marine[i] <- env.m} else {marine[i] <- 0}
    if(length(env.b)==1) {brackish[i] <- env.b} else {brackish[i] <- 0}
    if(length(env.f)==1) {freshwater[i] <- env.f} else {freshwater[i] <- 0}
    
  }#end of i
  
  #Summarise
  dat <- data.frame(sp,marine,freshwater,brackish)
  dat$sum <- apply(dat[,c("marine","freshwater","brackish")],1,sum)
  dat$environment <- rep(NA,nrow(dat))
  dat$EnvironmentLevel <- rep("species",nrow(dat))
  
  env <- c("marine","freshwater","brackish")
  
  for(j in 1:nrow(dat)){
    #no data
    if(dat$sum[j]==0){
      dat$Environment[j] <- "unknown"
      dat$EnvironmentLevel[j] <- "higher"
    }
    
    #Species-level data
    if(dat$sum[j]==1){
      dat$Environment[j] <- env[which(dat[j,c("marine","freshwater","brackish")]==1)]
    }else{
      dat$Environment[j] <- "mixed"
    }
  }
  
  final <- unique(dat %>% rename(Species_corrected = sp) %>%
                    select(Species_corrected,Environment,EnvironmentLevel))
  
  res <- merge(sp_data,final,by="Species_corrected",all.x=T)
  
  return(res) 
  
}#end of getEnvironment

#################################################
#Lmax
getLmax<-function(sp_data){
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  lmax <- vector()
  lmaxLevel <- rep("species",length(sp))
  lmaxtype <- vector()
  
  for(i in 1:length(sp)) {
    cat("i",i,"\n")
    # Grab species page
  
    urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    
    #Extract URLs and Links
    glinks <- scraplinks(urly)
    idlink <- which(glinks$link == "Life-history tool")
    
    url <- paste("https://www.fishbase.se",glinks$url[idlink],sep="")
    
    html <- read_html(url)
    
    #Extract Lmax
    value <- html %>%
      html_nodes("tr")
    
    id <- grep("Lmax", value)
    
    if(length(id)==0){
      lmax[i] <- NA
      lmaxLevel[i] <- NA
      lmaxtype[i] <- 'unknown'
    }else{
      h <- value[id]
      lmax[i] <- as.numeric(html_nodes(h, "input[type='text']") %>% html_attr("value"))
      
      #Extract type if drop-down menu
      x <- gsub(">", ">\n", h)
      x <- paste0(x, collapse = "")
      x <- unlist(strsplit(x, "\n"))
      
      ## Extract selected options ----
      test <- grep("option.*selected", x)
      
      # if(length(test)==1){
      #   type <- x[test]
      #   type <- gsub("\"", "", regmatches(type, regexpr("\"[A-Z]{2}\"", type)))
      #   
      # }else{
        #Extract type if text (fixed) value
        l_text <- value %>%
          html_text()
        
        #Identify Lmax section and extract type
        id <- grep("Lmax", l_text)
        
        #Delete multiples spaces etc.
        lraw <- gsub("\r|\n|\t", " ", l_text[id])
        lclean <- gsub("\\s+", " ", str_trim(lraw))
        lval <- strsplit(lclean," ")[[1]]
        
        type <- lval[length(lval)]
      }
      
      #Save
      lmaxtype[i] <- type
    # }
    
    
  }#end of i
  
  len <- data.frame(sp,lmax,lmaxtype,lmaxLevel)
  names(len)[1] <- "Species_corrected"
  
  res <- merge(sp_data,len,by="Species_corrected",all.x=T) 
  
  return(res) 
  
}#end of getLmax

################################################
#getBiology

getBiology <- function(sp_data) {
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  #BiologyLevel <- rep("species",length(sp))
  Biology <- vector()
  
  for(i in 1:length(sp)){
    
    # Grab species page
    cat("i",i,"\n")
    
    if( is.na(word(sp[i],3))==F){
      urly = paste('https://www.fishbase.se/summary/',word(sp[i],1),"-",word(sp[i],2),"+",word(sp[i],3),'.html',sep="")
    }else{
      urly = paste('https://www.fishbase.se/summary/',str_replace_all(sp[i]," ", "-"),'.html',sep="")
    }
    
    #Convert to text
    biol_text <- read_html(urly) %>% 
      html_nodes(".smallSpace") %>%
      html_text()
    
    #Identify Environment section and extract habitat if provided
    biol.p <- unique(c(grep("open water", biol_text), grep("midwater", biol_text)))
    biol.b <- unique(c(grep("benthopelagic", biol_text), grep("Benthopelagic", biol_text),grep("seaward", biol_text),grep("outer reef",biol_text)))
    
    if( (length(biol.p)==0 & length(biol.b)==0) ) { Biology[i] <- "Bnth" } #default benthic
    if( (length(biol.b)>=1) ) { Biology[i] <- "BtPl" } # 
    if( (length(biol.p)>=1) ) { Biology[i] <- "Pelg" } # 
    
  } #end of i
  
  #Clean outputs 
  bio <- data.frame(sp,Biology)
  names(bio)[1] <- "Species_corrected"
  
  res <- merge(sp_data,bio,by="Species_corrected",all.x=T)
  
  return(res)
  
}#end of getBiology

################################################
#getVerticalPosition

getVerticalPosition <- function(sp_data,gaspar) {
  
  #Delete missing species and create empty vector to fill in
  sp <- unique(sp_data$Species_corrected)
  
  VerticalPos <- vector()
  
  for(i in 1:length(sp)){
    test <- which(gaspar$FullSpecies==sp[i])
    if(length(test)==1){
      VerticalPos[i]<-gaspar$Level_water[test]
    }else{
      VerticalPos[i] <- 'unknown'
    }
    
  }#end of i
  
  #Clean outputs 
  pos <- data.frame(sp,VerticalPos)
  names(pos)[1] <- "Species_corrected"
  pos$VerticalPos <- fct_recode(VerticalPos, "benthic" = "Bottom",
                                "benthopelagic" = "Low",
                                "pelagic"="High")
  
  res <- merge(sp_data,pos,by="Species_corrected",all.x=T)
  
  return(res)
  
}# end of getVerticalPosition


#END
#############################################################################
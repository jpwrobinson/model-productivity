library(rfishprod)
library(tidyverse)

force<-read.csv(file = 'data/force_with_lmax.csv')

bon<-readxl::read_excel('data/Species_Bonaire.xlsx') %>% 
    mutate(Species = recode(Species, 'Aulostomus maculataus' = 'Aulostomus maculatus',
                            'Neoiphon marianus' = 'Neoniphon marianus')) %>% 
    left_join(force, by = 'Species') %>% 
    mutate(trophic_group=as.factor(FunctionalGrp))

##--------------------------------------------------##
# 1. Need to update db (rfishprod) with our diet cats
##--------------------------------------------------##
## QUERY ALICE 1
## assign db species with functional group scheme in FORCE
db<-db %>% mutate(trophic_group = recode(Diet, 'InvMob' = 'Invertivore',
                                         'InvSes' = 'Invertivore',
                                         'Plktiv' = 'Planktivore',
                                         'Omnivr' = 'Piscivore',
                                         'HerDet' = 'Herbivore',
                                         'HerMac' = 'Herbivore',
                                         'FisCep' = 'Piscivore'),
                  lmax=MaxSizeTL) %>% 
    droplevels()


# QUERY ALICE 2 - need site lat lon
##--------------------------------------------------##
## 2. add SST (see morais_sst_script.R)
##--------------------------------------------------##
# sst<-read.csv('data/env/sst_mean_extracted.csv')
bon$sstmean<-28.4 #placeholder


##--------------------------------------------------##
# 3. Predicting Kmax, the standardised VBGF parameter 
#  (Recommendation: use 100s to 1000s iterations) 
##--------------------------------------------------##

bon$trophic_group<-factor(bon$trophic_group, levels=levels(db$trophic_group))
bon<-droplevels(bon)

# adapt formula from Morais & Bellwood 2018
fmod <- formula (~ sstmean + lmax + trophic_group) 

## check overlap of species
length(unique(bon$Species_corrected[!bon$Species_corrected %in% db$Species])) # 65 sp in WCS not in Morais db
length(unique(bon$Species_corrected)) # 89 sp in WCS - 4 countries
65/89*100 # 73% needing out of sample preds

# fit xgboost model to predict Kmax
uvc <- predKmax (bon,
                   dataset = db,
                   fmod = fmod,
                   niter = 1000,
                   return = 'pred')

# save Kmax predictions
bon <- uvc$pred
hist(bon$Kmax)

##--------------------------------------------------##
#       4. Predicting individual growth
##--------------------------------------------------##

## Now we need the UVC fish obs in here
uvc<-readxl::read_excel('data/Bonaire_fish_data_5plus_fixed.xlsx') %>% 
    mutate(Species = recode(Species, 'Aulostomus maculataus' = 'Aulostomus maculatus',
                            'Neoiphon marianus' = 'Neoniphon marianus')) %>% 
    left_join(bon) %>% 
    janitor::clean_names()

uvc %>% filter(size_cm >= lmax) %>% dim() ## 8 observations
uvc %>% filter(size_cm >= lmax) %>% summarise(sum(abundance)) ## 8 fishes

# These need to be reduced to equal lmax (prod= exactly 0) 
# OR 0.1cm below lmax (tiny prod values)
uvc$size2 <- ifelse(uvc$size_cm >= uvc$lmax, uvc$lmax-0.1, uvc$size_cm)

# # Positioning fish in their growth trajectory 
# # i.e. what's the size they're supposed to have on the next day? 
uvc$L.1day <- with (uvc, applyVBGF (Lmeas = size2,
                                        Lmax = lmax,
                                        Kmax = kmax))

head(uvc %>% select(size2, L.1day)) #each fish has grown a tiny amount (in length).

#Calculate age estimates:
## 4. productivity equation
lplus<-function(lmax, Kmax, age, days=1/365){lmax*(1 - exp(-Kmax*(age+days)))} ## Renato approach, VBGF growth based on age

# estimate age of each fish (eq. 3 in Depczynski et al. 2007)
age_est<-function(lmax, lcensus, K, l0=0){(1/K)*log(lmax/((1-lcensus)*lmax))}
uvc$age<-age_est(lmax=uvc$lmax, lcensus=uvc$size2/uvc$lmax, K = uvc$kmax)

# convert length to mass
uvc$mass<-uvc$a * uvc$size2 ^ uvc$b

## estimate productivity of each fish
uvc$size_nextday<-with(uvc, 
                       lplus(lmax = lmax, K = kmax, age = age ))
uvc$prod_mass_g<-with(uvc, 
                      somaGain(a = a, b = b, Lmeas = size2, Lmax = lmax, Kmax = kmax))

## estimate natural mortality
uvc$Z<-with(uvc, predM(size2, Kmax = kmax, Lmax = lmax)) ## estimated mortality rate by species
uvc$per_capita_mortality<-with(uvc, somaLoss(Z, size2, t = 1)) ## daily per capita loss from natural mortality

# remove mortality from mass gain 
uvc$prod_mass_g <- uvc$prod_mass_g - uvc$per_capita_mortality
uvc$prod_mass_g <- ifelse(uvc$prod_mass_g < 0, 0, uvc$prod_mass_g)

uvc<-uvc %>% mutate(
    prod_cm_day_perfish = size_nextday - size2,
    prod_g_day = prod_mass_g * abundance,
    prod_g_day_m2 = prod_g_day  / transect_area) ## convert transect to m2


## save and end
write.csv(bon, 'data/bonaire_kmax_pred.csv')
write.csv(uvc, file = 'results/bonaire_productivity.csv')

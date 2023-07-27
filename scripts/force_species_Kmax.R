library(rfishprod)
library(tidyverse)
source('scripts/traits_from_fishbase_functions.R')

force<-readxl::read_excel('data/FORCE_fish_species.xlsx')
length(unique(force$Species)) # 176 species
## fix some species names that don't work with validate_names below
force<-force %>% mutate(Species = recode(Species, 'Aulostomus maculataus' = 'Aulostomus maculatus',
                                         'Cantherhines pulles' = 'Cantherhines pullus',
                                         'Dasyatis americanus' = 'Hypanus americanus',
                                         'Halichoeres bivitattus' = 'Halichoeres bivittatus',
                                         'Neoiphon marianus' = 'Neoniphon marianus',
                                         'Serranus tobacarius' = 'Serranus tabacarius',
                                         'Sphoeroides sphengleri' = 'Sphoeroides spengleri'
))

## QUERY WITH ALICE
force<-force %>% filter(! Species %in% c('Emblemariopsis spp.', 'Gobiidae sp.',
                                         'Hypoplectrus sp.', 'Gobiosoma sp.'))

# check overlap with Renato package db
# **careful** there are multiple rows per unique species, because of location specific size / K values
data(db)
inn<-length(unique(db$Species[db$Species %in% force$Species])) # 37 species in prod db
miss<-length(force$Species[!force$Species %in% db$Species]) ## 140 missing species
paste0(round(miss/length(unique(force$Species))*100, 2), '% of species require out-of-sample Kmax')

## Lmax and diet for all species in Renato, Casey and James dbs
lmax_db<-read.csv('data/species_lmax_diet.csv')
inn<-length(lmax_db$species[lmax_db$species %in% force$Species]) # 37 species in prod lmax_db
miss<-length(force$Species[!force$Species %in% lmax_db$species]) ## 140 missing species
paste0(round(miss/length(unique(force$Species))*100, 2), '% of species require out-of-sample Kmax')


#Get fish Taxonomy from Fishbase 
tax <- load_taxa() %>% 
    filter(!is.na(SpecCode)) %>% 
    filter(!SpecCode %in% c('<p>', '', '.')) %>% 
    collect() %>% 
    mutate(Species_corrected = Species)

tax<-tax[!duplicated(tax),]

## get lmax from fishbase
species_list <- data.frame(Species = sort(force$Species)) # 175 species
species_list<-species_list %>% left_join(tax)

# validate names for incorrect species (n = 27)
incor<-species_list$Species[is.na(species_list$SpecCode)]
sp_data <- getTaxo(sp = incor , tax = tax)
species_list$Species_corrected[is.na(species_list$SpecCode)]<-sp_data$Species_corrected
species_list$Genus[is.na(species_list$SpecCode)]<-sp_data$Genus
species_list$Family[is.na(species_list$SpecCode)]<-sp_data$Family
species_list$SpecCode[is.na(species_list$SpecCode)]<-sp_data$SpecCode

## data checks
species_list[duplicated(species_list),]
species_list %>% filter(is.na(SpecCode))
species_list<- species_list[!duplicated(species_list),]

#Get Lmax 
lmax <- getLmax(species_list)

# compare with previous vals
lmax_db$lmax_fb<-lmax$lmax[match(lmax_db$species, lmax$Species_corrected)]

ggplot(lmax_db %>% filter(!is.na(lmax_fb)), aes(max_size_tl, lmax_fb)) + 
    geom_abline(intercept=0, slope = 1, col='grey50', linetype=5) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = species), size=2) +
    theme_bw()

# query weirdoes
lmax %>% filter(Species_corrected %in% c('Acanthurus bahianus', 'Elagatis bipinnulata', "Melichthys niger"))
# lmax is set to 1 when this is NULL on fishbase. Use previous db value.
# Elagatis and Melichthys values are confident on fishbase.

# reset A. bahianus
lmax$lmax[lmax$Species_corrected=='Acanthurus bahianus']<-lmax_db$max_size_tl[lmax_db$species == 'Acanthurus bahianus']

## match lmax with FORCE 
force$Species_corrected<-species_list$Species_corrected[match(force$Species, species_list$Species)]
force$lmax<-lmax$lmax[match(force$Species_corrected, lmax$Species_corrected)]

write.csv(force, file = 'data/force_with_lmax.csv')

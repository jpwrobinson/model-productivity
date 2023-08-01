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
fishp <- predKmax (bon,
                   dataset = db,
                   fmod = fmod,
                   niter = 1000,
                   return = 'pred')

# save Kmax predictions
bon <- fishp$pred
hist(bon$Kmax)

write.csv(bon, 'data/bonaire_kmax_pred.csv')

## QUERY ALICE 3
## Now we need the UVC fish obs in here
library(rfishprod)
library(tidyverse)

force<-read.csv(file = 'data/force_with_lmax.csv')

##--------------------------------------------------##
# 1. Need to update db (rfishprod) with our diet cats
##--------------------------------------------------##
## QUERY ALICE 1
## assign db species with functional group scheme in FORCE
db<-db %>% mutate(trophic_group = recode(Diet, 'InvMob' = 'invertivore-mobile',
                                         'InvSes' = 'invertivore-sessile',
                                         'Plktiv' = 'planktivore',
                                         'Omnivr' = 'omnivore',
                                         'HerDet' = 'herbivore-detritivore',
                                         'HerMac' = 'herbivore-macroalgae',
                                         'FisCep' = 'piscivore'))


# QUERY ALICE 2
##--------------------------------------------------##
## 2. add SST (see morais_sst_script.R)
##--------------------------------------------------##
sst<-read.csv('data/env/sst_mean_extracted.csv')
fish$sstmean<-sst$sst


##--------------------------------------------------##
# 3. Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) 
##--------------------------------------------------##

# adapt formula from Morais & Bellwood 2018
fmod <- formula (~ sstmean + lmax + trophic_group) 

# fit xgboost model to predict Kmax
fishp <- predKmax (force,
                   dataset = db,
                   fmod = fmod,
                   niter = 1000,
                   return = 'pred')

# save Kmax predictions
force <- fishp$pred
hist(force$Kmax)

## QUERY ALICE 3
## Now we need the UVC fish obs in here
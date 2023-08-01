
uvc<-read.csv(file = 'results/bonaire_productivity.csv')


prod<-uvc %>% group_by(site_id, transect_id, transect_length, reef_name) %>% 
    summarise(biomass_gm2 = sum(biomass_gm2),
              prod_g_day_m2 = sum(prod_g_day_m2),
              prod_g_yr_m2 = sum(prod_g_day_m2 * 365)) %>% 
    group_by(site_id, transect_length, reef_name) %>% 
    summarise(biomass_gm2 = mean(biomass_gm2),
              prod_g_day_m2 = mean(prod_g_day_m2),
              prod_g_yr_m2 = mean(prod_g_yr_m2)) %>% 
    group_by(site_id, reef_name) %>% 
    summarise(biomass_gm2 = sum(biomass_gm2),
              prod_g_day_m2 = sum(prod_g_day_m2),
              prod_g_yr_m2 = sum(prod_g_yr_m2)) 


prod_fg<-uvc %>% group_by(site_id, transect_id, transect_length, reef_name, model_grp) %>% 
    summarise(biomass_gm2 = sum(biomass_gm2),
              prod_g_day_m2 = sum(prod_g_day_m2),
              prod_g_yr_m2 = sum(prod_g_day_m2 * 365)) %>% 
    group_by(site_id, transect_length, reef_name, model_grp) %>% 
    summarise(biomass_gm2 = mean(biomass_gm2),
              prod_g_day_m2 = mean(prod_g_day_m2),
              prod_g_yr_m2 = mean(prod_g_yr_m2)) %>% 
    group_by(site_id, reef_name, model_grp) %>% 
    summarise(biomass_gm2 = sum(biomass_gm2),
              prod_g_day_m2 = sum(prod_g_day_m2),
              prod_g_yr_m2 = sum(prod_g_yr_m2)) %>% 
    pivot_longer(biomass_gm2:prod_g_yr_m2, names_to = 'met', values_to = 'val')

ggplot(prod_fg, 
       aes(reef_name, val, fill=model_grp)) + geom_bar(stat='identity') +
    facet_wrap(~met, scales='free')




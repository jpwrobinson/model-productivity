
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
    pivot_longer(biomass_gm2:prod_g_yr_m2, names_to = 'met', values_to = 'val') %>% 
    filter(met != 'prod_g_day_m2') %>% 
    mutate(source = 'Morais')

# alice mod preds
mod<-readxl::read_excel('results/Bonaire_Model_data_comparison2023.xlsx') %>% 
    janitor::clean_names() %>% 
    rename('Pred_biomass_gm2' = model_pred_biomass_5, 
           'Herb_biomass_gm2' = model_herb_biomass_5,
           'Pred_prod_g_yr_m2' = model_pred_productivity,
           'Herb_prod_g_yr_m2' = model_herb_productivity,
           'reef_name' = 'site_name') %>% 
    select(reef_name:Herb_prod_g_yr_m2) %>% 
    pivot_longer(-reef_name, names_to = 'met', values_to = 'val') %>% 
    mutate(model_grp = str_split_fixed(met, '_', 2)[,1],
           met = str_split_fixed(met, '_', 2)[,2],
           source = 'Rogers')


plotter<-rbind(mod, prod_fg %>% ungroup() %>%  select(names(mod)))

ggplot(plotter, 
       aes(reef_name, val, fill=model_grp)) + geom_bar(stat='identity') +
    facet_grid(met ~ source, scales='free') +
    labs(x = '', y = 'value', fill='')

ggplot(plotter, 
       aes(reef_name, val, fill=model_grp)) + geom_bar(stat='identity', position = 'fill') +
    facet_grid(met ~ source, scales='free') +
    labs(x = '', y = 'proportion %', fill='')




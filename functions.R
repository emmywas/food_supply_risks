#functions.R

#**********************************************************************************************
get_national_percap<-function(nutrient_production,population){
  
  national_percap_data<-nutrient_production %>%
    select(-Area,-pop_total) %>% 
    group_by(UN_CODE,FAO_code,year) %>% 
    summarise(across(prod_sum_kcal:fruitveg_100g_cap_day, ~ sum(.x, na.rm = TRUE))) 
  
  write_csv(national_percap_data,"./data/national_percap_data.csv")
  
  return(national_percap_data)
}

#**********************************************************************************************
#Resilience/diversity calculations

wrangle_diversity_data<-function(nutrient_production){
  
  # Custom function to calculate h-index
  calc_h_index <- function(col){ #column name as input
    col <- col[order(col, decreasing = TRUE)] #order column data from largest to smallest
    tail(which(col >= seq_along(col)), 1) #calculate h-index
  }


  #H-index
  h_index_data<-nutrient_production%>%
    select(FAO_code,year,unique_id,kcal_cap_day) %>% 
    group_by(FAO_code,year,unique_id) %>% 
    summarise_at(vars(kcal_cap_day),list(sum),na.rm=T) %>%
    ungroup() %>% 
    group_by(FAO_code,year) %>% 
    summarize(h_index = calc_h_index(kcal_cap_day))
  
  
  richness<-nutrient_production %>%
    group_by(FAO_code,year,unique_id) %>% 
    summarise_at(vars(kcal_cap_day),list(sum),na.rm=T) %>%
    ungroup() %>% 
    select(FAO_code,year,unique_id) %>% 
    count(FAO_code,year) %>% 
    rename(richness=n)
  
  diversity_data<-h_index_data %>% 
    left_join(richness)
  
  
  return(diversity_data)
  
}
#********************************************************************************************
wrangle_extended_div_data<-function(nutrient_production){
  
  extended_diversity_data<-nutrient_production %>% 
    group_by(FAO_code,year,unique_id) %>% 
    summarise_at(vars(kcal_cap_day:fruitveg_100g_cap_day),list(sum),na.rm=T) %>%
    ungroup() %>% 
    group_by(FAO_code,year) %>% 
    mutate(kcal_rank = rank(-kcal_cap_day, na.last="keep")) %>% 
    mutate(protein_rank = rank(-protein_g_cap_day, na.last="keep")) %>% 
    mutate(fat_rank = rank(-fat_g_cap_day, na.last="keep")) %>% 
    mutate(carb_rank = rank(-carb_g_cap_day, na.last="keep")) %>% 
    mutate(vitA_rank = rank(-vitA_mcg_cap_day, na.last="keep")) %>% 
    mutate(folate_rank = rank(-folate_mcg_cap_day, na.last="keep")) %>%
    mutate(iron_rank = rank(-iron_mg_cap_day, na.last="keep")) %>% 
    mutate(zinc_rank = rank(-zinc_mg_cap_day, na.last="keep")) %>% 
    mutate(calcium_rank = rank(-calcium_mg_cap_day, na.last="keep")) %>%
    mutate(fruitveg_rank = rank(-fruitveg_100g_cap_day, na.last="keep")) %>% 
    mutate(kcal_rank=case_when((kcal_cap_day==0) ~ NA_real_, 
                               TRUE ~ kcal_rank)) %>%  #Zero's should not have a rank
    mutate(protein_rank=case_when((protein_g_cap_day==0) ~ NA_real_, 
                               TRUE ~ protein_rank)) %>% 
    mutate(fat_rank=case_when((fat_g_cap_day==0) ~ NA_real_, 
                                  TRUE ~ fat_rank)) %>% 
    mutate(carb_rank=case_when((carb_g_cap_day==0) ~ NA_real_, 
                              TRUE ~ carb_rank)) %>% 
    mutate(vitA_rank=case_when((vitA_mcg_cap_day==0) ~ NA_real_, 
                               TRUE ~ vitA_rank)) %>% 
    mutate(folate_rank=case_when((folate_mcg_cap_day==0) ~ NA_real_, 
                               TRUE ~ folate_rank)) %>% 
    mutate(iron_rank=case_when((iron_mg_cap_day==0) ~ NA_real_, 
                                 TRUE ~ iron_rank)) %>% 
    mutate(zinc_rank=case_when((zinc_mg_cap_day==0) ~ NA_real_, 
                               TRUE ~ zinc_rank)) %>% 
    mutate(calcium_rank=case_when((calcium_mg_cap_day==0) ~ NA_real_, 
                               TRUE ~ calcium_rank)) %>% 
    mutate(fruitveg_rank=case_when((fruitveg_100g_cap_day==0) ~ NA_real_, 
                                  TRUE ~ fruitveg_rank))
  
  write_csv(extended_diversity_data, "./data/extended_diversity_data.csv")
  
  return(extended_diversity_data)
}
#**********************************************************************************************
make_full_temporal_data<-function(national_percap_data,diversity_data){
  
  #Simple, generic calculation of nutritional limits (no national)
  calorie_limit<-2357
  protein_limit<-(calorie_limit*0.1)/4 #used lower limit in 10-15%, 1g protein is approx 4 kcal
  fat_limit<-(calorie_limit*0.15)/9 #lower limit, 15-30%, 1g fat is approx 9 kcal
  carb_limit<-(calorie_limit*0.55)/4 #lower limit, 55-75%, 1g carb approx 4 kcal
  fruit_limit<-4 #4 due to 100g unit - i.e. limit at 400g
  
  vitA_limit<-434 #Vit A requirement mean for adults equivalent to 434mg RE/day
  folate_limit<-320 #Average across adult age groups average daily requirement 320 mg/day
  iron_limit<-24.24 #Averaged across 18+, males and females, across 5,10,12,15% bioavailability mg/day
  zinc_limit<-7.15 #Averaged across adult age groups and across High, Moderate, Low bioavailability 7.15 mg/day
  calcium_limit <-1000 #Calcium recommended intake for adults of 1000mg/day #NOTE recommended intake vs requirement
  
  #For now no calorie index - only plot continuous
  #calorie_quarter<-calorie_limit*0.25
  #calorie_half<-calorie_limit*0.5
  #calorie_3quarter<-calorie_limit*0.75
  
  nutrient_suff_data<- national_percap_data%>% 
       mutate(protein_suff=case_when((protein_g_cap_day>protein_limit)~1,
                                  (protein_g_cap_day<protein_limit)~0)) %>% 
    mutate(carb_suff=case_when((carb_g_cap_day>carb_limit)~1,
                               (carb_g_cap_day<carb_limit)~0)) %>% 
    mutate(fat_suff=case_when((fat_g_cap_day>fat_limit)~1,
                              (fat_g_cap_day<fat_limit)~0)) %>% 
    mutate(fruit_suff=case_when((fruitveg_100g_cap_day>fruit_limit)~1,
                                (fruitveg_100g_cap_day<fruit_limit)~0))%>%
    mutate(vitA_suff=case_when((vitA_mcg_cap_day>vitA_limit)~1,
                               (vitA_mcg_cap_day<vitA_limit)~0)) %>%
    mutate(folate_suff=case_when((folate_mcg_cap_day>folate_limit)~1,
                                 (folate_mcg_cap_day<folate_limit)~0)) %>%
    mutate(iron_suff=case_when((iron_mg_cap_day>iron_limit)~1,
                               (iron_mg_cap_day<iron_limit)~0)) %>% 
    mutate(zinc_suff=case_when((zinc_mg_cap_day>zinc_limit)~1,
                               (zinc_mg_cap_day<zinc_limit)~0)) %>% 
    mutate(calcium_suff=case_when((calcium_mg_cap_day>calcium_limit)~1,
                                  (calcium_mg_cap_day<calcium_limit)~0)) %>% 
    mutate(num_nutrient_suff = rowSums(across(protein_suff:calcium_suff),na.rm = T)) %>% 
    mutate(protein_percentsuff=as.factor(case_when((protein_g_cap_day<=(protein_limit*0.5))~"0-50",
                                                   (protein_g_cap_day<=protein_limit)~"51-100",
                                                   (protein_g_cap_day<=(protein_limit*1.5))~"101-150",
                                                   (protein_g_cap_day>(protein_limit*1.5))~">150"
    ))) %>% 
    mutate(carb_percentsuff=as.factor(case_when((carb_g_cap_day<=(carb_limit*0.5))~"0-50",
                                                (carb_g_cap_day<=carb_limit)~"51-100",
                                                (carb_g_cap_day<=(carb_limit*1.5))~"101-150",
                                                (carb_g_cap_day>(carb_limit*1.5))~">150"
    ))) %>% 
    mutate(fat_percentsuff=as.factor(case_when((fat_g_cap_day<=(fat_limit*0.5))~"0-50",
                                               (fat_g_cap_day<=fat_limit)~"51-100",
                                               (fat_g_cap_day<=(fat_limit*1.5))~"101-150",
                                               (fat_g_cap_day>(fat_limit*1.5))~">150"
    ))) %>% 
    mutate(fruit_percentsuff=as.factor(case_when((fruitveg_100g_cap_day<=(fruit_limit*0.5))~"0-50",
                                                 (fruitveg_100g_cap_day<=fruit_limit)~"51-100",
                                                 (fruitveg_100g_cap_day<=(fruit_limit*1.5))~"101-150",
                                                 (fruitveg_100g_cap_day>(fruit_limit*1.5))~">150"
    ))) %>% 
    mutate(vitA_percentsuff=as.factor(case_when((vitA_mcg_cap_day<=(vitA_limit*0.5))~"0-50",
                                                (vitA_mcg_cap_day<=vitA_limit)~"51-100",
                                                (vitA_mcg_cap_day<=(vitA_limit*1.5))~"101-150",
                                                (vitA_mcg_cap_day>(vitA_limit*1.5))~">150"
    ))) %>% 
    mutate(folate_percentsuff=as.factor(case_when((folate_mcg_cap_day<=(folate_limit*0.5))~"0-50",
                                                  (folate_mcg_cap_day<=folate_limit)~"51-100",
                                                  (folate_mcg_cap_day<=(folate_limit*1.5))~"101-150",
                                                  (folate_mcg_cap_day>(folate_limit*1.5))~">150"
    ))) %>% 
    mutate(iron_percentsuff=as.factor(case_when((iron_mg_cap_day<=(iron_limit*0.5))~"0-50",
                                                (iron_mg_cap_day<=iron_limit)~"51-100",
                                                (iron_mg_cap_day<=(iron_limit*1.5))~"101-150",
                                                (iron_mg_cap_day>(iron_limit*1.5))~">150"
    ))) %>% 
    mutate(zinc_percentsuff=as.factor(case_when((zinc_mg_cap_day<=(zinc_limit*0.5))~"0-50",
                                                (zinc_mg_cap_day<=zinc_limit)~"51-100",
                                                (zinc_mg_cap_day<=(zinc_limit*1.5))~"101-150",
                                                (zinc_mg_cap_day>(zinc_limit*1.5))~">150"
    ))) %>% 
    mutate(calcium_percentsuff=as.factor(case_when((calcium_mg_cap_day<=(calcium_limit*0.5))~"0-50",
                                                   (calcium_mg_cap_day<=calcium_limit)~"51-100",
                                                   (calcium_mg_cap_day<=(calcium_limit*1.5))~"101-150",
                                                   (calcium_mg_cap_day>(calcium_limit*1.5))~">150"
    ))) %>% 
    mutate(protein_factor=case_when((protein_g_cap_day<=(protein_limit*0.5))~1,
                                    (protein_g_cap_day<=protein_limit)~2,
                                    (protein_g_cap_day<=(protein_limit*1.5))~3,
                                    (protein_g_cap_day>(protein_limit*1.5))~4
    ))%>% 
    mutate(carb_factor=case_when((carb_g_cap_day<=(carb_limit*0.5))~1,
                                 (carb_g_cap_day<=carb_limit)~2,
                                 (carb_g_cap_day<=(carb_limit*1.5))~3,
                                 (carb_g_cap_day>(carb_limit*1.5))~4
    )) %>% 
    mutate(fat_factor=case_when((fat_g_cap_day<=(fat_limit*0.5))~1,
                                (fat_g_cap_day<=fat_limit)~2,
                                (fat_g_cap_day<=(fat_limit*1.5))~3,
                                (fat_g_cap_day>(fat_limit*1.5))~4
    )) %>% 
    mutate(fruit_factor=case_when((fruitveg_100g_cap_day<=(fruit_limit*0.5))~1,
                                  (fruitveg_100g_cap_day<=fruit_limit)~2,
                                  (fruitveg_100g_cap_day<=(fruit_limit*1.5))~3,
                                  (fruitveg_100g_cap_day>(fruit_limit*1.5))~4
    )) %>% 
    mutate(vitA_factor=case_when((vitA_mcg_cap_day<=(vitA_limit*0.5))~1,
                                 (vitA_mcg_cap_day<=vitA_limit)~2,
                                 (vitA_mcg_cap_day<=(vitA_limit*1.5))~3,
                                 (vitA_mcg_cap_day>(vitA_limit*1.5))~4
    )) %>% 
    mutate(folate_factor=case_when((folate_mcg_cap_day<=(folate_limit*0.5))~1,
                                   (folate_mcg_cap_day<=folate_limit)~2,
                                   (folate_mcg_cap_day<=(folate_limit*1.5))~3,
                                   (folate_mcg_cap_day>(folate_limit*1.5))~4
    )) %>% 
    mutate(iron_factor=case_when((iron_mg_cap_day<=(iron_limit*0.5))~1,
                                 (iron_mg_cap_day<=iron_limit)~2,
                                 (iron_mg_cap_day<=(iron_limit*1.5))~3,
                                 (iron_mg_cap_day>(iron_limit*1.5))~4
    )) %>% 
    mutate(zinc_factor=case_when((zinc_mg_cap_day<=(zinc_limit*0.5))~1,
                                 (zinc_mg_cap_day<=zinc_limit)~2,
                                 (zinc_mg_cap_day<=(zinc_limit*1.5))~3,
                                 (zinc_mg_cap_day>(zinc_limit*1.5))~4
    )) %>% 
    mutate(calcium_factor=case_when((calcium_mg_cap_day<=(calcium_limit*0.5))~1,
                                    (calcium_mg_cap_day<=calcium_limit)~2,
                                    (calcium_mg_cap_day<=(calcium_limit*1.5))~3,
                                    (calcium_mg_cap_day>(calcium_limit*1.5))~4
    )) 
  
  
  
  
  #Merge datasets to join self-suff index against h-index
  clean_temporal_data<-nutrient_suff_data %>% 
    ungroup() %>% 
    select(FAO_code,year,kcal_cap_day:calcium_factor) %>% 
    left_join(diversity_data)
  
  temporal_data_clean<-clean_temporal_data %>%
    filter(!is.na(num_nutrient_suff)) %>% 
    filter(!is.na(h_index))
  
  full_temporal_data<-temporal_data_clean %>% 
    ungroup() %>% 
    count(num_nutrient_suff, h_index) %>% 
    right_join(temporal_data_clean)
  
  return(full_temporal_data)
}

#**********************************************************************************************
calc_current<-function(full_temporal_data, country_list){
  
  calorie_limit<-2357
  protein_limit<-(calorie_limit*0.1)/4 #used lower limit in 10-15%, 1g protein is approx 4 kcal
  fat_limit<-(calorie_limit*0.15)/9 #lower limit, 15-30%, 1g fat is approx 9 kcal
  carb_limit<-(calorie_limit*0.55)/4 #lower limit, 55-75%, 1g carb approx 4 kcal
  fruit_limit<-4 #4 due to 100g unit - i.e. limit at 400g
  
  vitA_limit<-434 #Vit A requirement mean for adults equivalent to 434mg RE/day
  folate_limit<-320 #Average across adult age groups average daily requirement 320 mg/day
  iron_limit<-24.24 #Averaged across 18+, males and females, across 5,10,12,15% bioavailability mg/day
  zinc_limit<-7.15 #Averaged across adult age groups and across High, Moderate, Low bioavailability 7.15 mg/day
  calcium_limit <-1000 #Calcium recommended intake for adults of 1000mg/day #NOTE recommended intake vs requirement
  
  current_system<-full_temporal_data %>% 
    filter(year>=2014) %>% 
    group_by(FAO_code) %>% 
    summarise_at(c("h_index","num_nutrient_suff","kcal_cap_day","protein_g_cap_day","carb_g_cap_day",
                   "fat_g_cap_day","fruitveg_100g_cap_day","vitA_mcg_cap_day", "folate_mcg_cap_day",
                   "iron_mg_cap_day","zinc_mg_cap_day","calcium_mg_cap_day","richness")
                 ,mean) %>% 
    mutate(suff_group=as.factor(case_when(num_nutrient_suff>=9~"complete",
                                          num_nutrient_suff<9 & num_nutrient_suff>=6~"high",
                                          num_nutrient_suff<6 & num_nutrient_suff>=3~"medium",
                                          num_nutrient_suff<3 & num_nutrient_suff>=0~"low"))) %>% 
    mutate(div_group=as.factor(case_when(h_index<10 ~"low",
                                         h_index>=10 & h_index <20 ~"medium",
                                         h_index>=20~"high"))) %>% 
    mutate(col_group=as.factor(case_when(div_group=="low" & suff_group=="complete"~1,  
                                         div_group=="medium" & suff_group=="complete"~2,
                                         div_group=="high" & suff_group=="complete"~3,
                                         div_group=="low" & suff_group=="high"~4,
                                         div_group=="medium" & suff_group=="high"~5,
                                         div_group=="high" & suff_group=="high"~6,
                                         div_group=="low" & suff_group=="medium"~7,
                                         div_group=="medium" & suff_group=="medium"~8,
                                         div_group=="high" & suff_group=="medium"~9,
                                         div_group=="low" & suff_group=="low"~10,
                                         div_group=="medium" & suff_group=="low"~11,
                                         div_group=="high" & suff_group=="low"~12))) %>% 
    mutate(protein_percentsuff=as.factor(case_when((protein_g_cap_day<=(protein_limit*0.5))~"0-50",
                                                   (protein_g_cap_day<=protein_limit)~"51-100",
                                                   (protein_g_cap_day<=(protein_limit*1.5))~"101-150",
                                                   (protein_g_cap_day>(protein_limit*1.5))~">150"))) %>%
    mutate(protein_percentsuff=fct_relevel(protein_percentsuff,
                                           levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(carb_percentsuff=as.factor(case_when((carb_g_cap_day<=(carb_limit*0.5))~"0-50",
                                                (carb_g_cap_day<=carb_limit)~"51-100",
                                                (carb_g_cap_day<=(carb_limit*1.5))~"101-150",
                                                (carb_g_cap_day>(carb_limit*1.5))~">150"
    ))) %>% 
    mutate(carb_percentsuff=fct_relevel(carb_percentsuff,
                                        levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(fat_percentsuff=as.factor(case_when((fat_g_cap_day<=(fat_limit*0.5))~"0-50",
                                               (fat_g_cap_day<=fat_limit)~"51-100",
                                               (fat_g_cap_day<=(fat_limit*1.5))~"101-150",
                                               (fat_g_cap_day>(fat_limit*1.5))~">150"
    ))) %>%
    mutate(fat_percentsuff=fct_relevel(fat_percentsuff,
                                       levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(fruit_percentsuff=as.factor(case_when((fruitveg_100g_cap_day<=(fruit_limit*0.5))~"0-50",
                                                 (fruitveg_100g_cap_day<=fruit_limit)~"51-100",
                                                 (fruitveg_100g_cap_day<=(fruit_limit*1.5))~"101-150",
                                                 (fruitveg_100g_cap_day>(fruit_limit*1.5))~">150"
    ))) %>%
    mutate(fruit_percentsuff=fct_relevel(fruit_percentsuff,
                                         levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(vitA_percentsuff=as.factor(case_when((vitA_mcg_cap_day<=(vitA_limit*0.5))~"0-50",
                                                (vitA_mcg_cap_day<=vitA_limit)~"51-100",
                                                (vitA_mcg_cap_day<=(vitA_limit*1.5))~"101-150",
                                                (vitA_mcg_cap_day>(vitA_limit*1.5))~">150"
    ))) %>% 
    mutate(vitA_percentsuff=fct_relevel(vitA_percentsuff,
                                        levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(folate_percentsuff=as.factor(case_when((folate_mcg_cap_day<=(folate_limit*0.5))~"0-50",
                                                  (folate_mcg_cap_day<=folate_limit)~"51-100",
                                                  (folate_mcg_cap_day<=(folate_limit*1.5))~"101-150",
                                                  (folate_mcg_cap_day>(folate_limit*1.5))~">150"
    ))) %>%
    mutate(folate_percentsuff=fct_relevel(folate_percentsuff,
                                          levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(iron_percentsuff=as.factor(case_when((iron_mg_cap_day<=(iron_limit*0.5))~"0-50",
                                                (iron_mg_cap_day<=iron_limit)~"51-100",
                                                (iron_mg_cap_day<=(iron_limit*1.5))~"101-150",
                                                (iron_mg_cap_day>(iron_limit*1.5))~">150"
    ))) %>% 
    mutate(iron_percentsuff=fct_relevel(iron_percentsuff,
                                        levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(zinc_percentsuff=as.factor(case_when((zinc_mg_cap_day<=(zinc_limit*0.5))~"0-50",
                                                (zinc_mg_cap_day<=zinc_limit)~"51-100",
                                                (zinc_mg_cap_day<=(zinc_limit*1.5))~"101-150",
                                                (zinc_mg_cap_day>(zinc_limit*1.5))~">150"
    ))) %>% 
    mutate(zinc_percentsuff=fct_relevel(zinc_percentsuff,
                                        levels=c("0-50","51-100","101-150",">150"))) %>% 
    mutate(calcium_percentsuff=as.factor(case_when((calcium_mg_cap_day<=(calcium_limit*0.5))~"0-50",
                                                   (calcium_mg_cap_day<=calcium_limit)~"51-100",
                                                   (calcium_mg_cap_day<=(calcium_limit*1.5))~"101-150",
                                                   (calcium_mg_cap_day>(calcium_limit*1.5))~">150"
    ))) %>% 
    mutate(calcium_percentsuff=fct_relevel(calcium_percentsuff,
                                           levels=c("0-50","51-100","101-150",">150"))) %>% 
    filter(FAO_code!=151) #Remove Netherlands Antilles, data is included in constituent Islands
  
  
  return(current_system)
  
}

#*******************************************************************************************
make_fig1<-function(current_system, country_list, population){
  
  
  calorie_limit<-2357
  
  square<-data.frame(x=c(1,1,1,1,1,1,1,1,1,1),
                     y=c(0,1,2,3,4,5,6,7,8,9)) 
  
  population_mini<-population %>% 
    filter(year>=2014) %>% 
    group_by(FAO_code) %>% 
    summarise(pop=mean(pop_total)) %>% 
    left_join(current_system) %>%
    filter(!is.na(col_group)) %>%
    ungroup() %>%
    mutate(y=case_when(num_nutrient_suff>=9 ~ 9,
                       num_nutrient_suff>=8 & num_nutrient_suff<9 ~8, 
                       num_nutrient_suff>=7 & num_nutrient_suff<8 ~7, 
                       num_nutrient_suff>=6 & num_nutrient_suff<7 ~6,
                       num_nutrient_suff>=5 & num_nutrient_suff<6 ~5,
                       num_nutrient_suff>=4 & num_nutrient_suff<5 ~4, 
                       num_nutrient_suff>=3 & num_nutrient_suff<4 ~3,
                       num_nutrient_suff>=2 & num_nutrient_suff<3 ~2,
                       num_nutrient_suff>=1 & num_nutrient_suff<2 ~1, 
                       num_nutrient_suff>=0 & num_nutrient_suff<1 ~0 )) 
    
    population_square_data<-population_mini %>% 
    group_by(y) %>% 
    summarise(pop_groups=sum(pop)) %>%
    mutate(pop_percent=(pop_groups/sum(pop_groups)*100)) %>% 
    left_join(square) %>% 
    mutate(text=as.character(round(pop_percent, digits = 2))) 
    
    no_country_summary<-population_mini %>% 
      count(y) %>% 
      mutate(x=1) %>% 
      mutate(text=as.character(n))
  
  fig1_data<-current_system %>% 
    mutate(cal_percent=(kcal_cap_day/calorie_limit)*100)
  
  fig1A<-ggplot(fig1_data)+
    geom_point(aes(x=cal_percent,y=num_nutrient_suff), size=3)+
    theme_classic()+
    geom_vline(aes(xintercept=100, col="red"))+
    theme(legend.position = "none")+
    scale_x_continuous(n.breaks=6, name= "Average calorie production, % of calorie sufficiency")+
    scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9), name="Average Number of nutrients fulfilled")
  
  fig1B<-ggplot(population_square_data,aes(x=x,y=y,fill=pop_percent))+
    geom_raster()+
    scale_fill_gradientn(colours=c("#F6BDC050","#FF000050"))+
    geom_text(aes(label=text))+
    theme_classic()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9), name="Average Number of nutrients fulfilled")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    xlab("Population (%)")
  
  fig1C<-ggplot(no_country_summary,aes(x=x,y=y))+
    geom_tile(fill='white')+
    geom_text(aes(label=text))+
    theme_classic()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9), name="Average Number of nutrients fulfilled")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    xlab("No. of countries")
  
  
  figure1<-plot_grid(fig1A,fig1B,fig1C, ncol=3,nrow=1, rel_widths = c(4,1,1), labels="AUTO")
  
  #Save as svg to align two facets in inkscape
  ggsave(plot=figure1,filename = "figure_1.svg",path = "./plots", width = 18,height = 12, units="cm")
  
  return(figure1)
  
}
#*******************************************************************************************
#Make figure 2 plot
make_fig2<-function(current_system, population){
  
  custom_palette<-c("complete"="#E0B44Aff",
                    "high"="#1E5631ff",
                    "medium"="#76BA1Bff",
                    "low"="#ACDF87ff"
  )
  
  custom_shapes<-c("complete"=16,
                   "high"=17,
                   "medium"=18,
                   "low"=15
    
  )
  
  panelA_square<-data.frame(y=rep(1,6),
                            x=seq(0,25,5)) 
  
  panelA_square_data<-population %>% 
    filter(year>=2014) %>% 
    group_by(FAO_code) %>% 
    summarise(pop=mean(pop_total)) %>% 
    left_join(current_system) %>%
    filter(!is.na(h_index)) %>%
    ungroup() %>%
    mutate(x=case_when(h_index>=0 & h_index<5 ~ 0,
                       h_index>=5 & h_index<10 ~ 5,
                       h_index>=10 & h_index<15 ~ 10,
                       h_index>=15 & h_index<20 ~ 15,
                       h_index>=20 & h_index<25 ~ 20,
                       h_index>=25  ~ 25)) %>% 
    group_by(x) %>% 
    summarise(pop_groups=sum(pop)) %>%
    mutate(pop_percent=(pop_groups/sum(pop_groups)*100)) %>% 
    left_join(panelA_square) %>% 
    mutate(text=as.character(round(pop_percent, digits = 2)))
  
  
  panelC_square<-data.frame(x=rep(1,9),
                            y=seq(0, 400, 50)) 
  
  #Since one group that does not have datapoints - introduce blank line
  #empty_line<-data.frame(y=350,pop_groups=0,pop_percent=0,x=1,text="0")
  
  panelC_square_data<-population %>% 
    filter(year>=2014) %>% 
    group_by(FAO_code) %>% 
    summarise(pop=mean(pop_total)) %>% 
    left_join(current_system) %>%
    filter(!is.na(richness)) %>%
    ungroup() %>%
    mutate(y=case_when(richness>=0 & richness<50 ~ 0,
                       richness>=50 & richness<100 ~ 50,
                       richness>=100 & richness<150 ~ 100,
                       richness>=150 & richness<200 ~ 150,
                       richness>=200 & richness<250 ~ 200,
                       richness>=250 & richness<300  ~ 250,
                       richness>=300 & richness<350  ~ 300,
                       richness>=350 & richness<400  ~ 350,
                       richness>=400 & richness<450  ~ 400)) %>% 
    group_by(y) %>% 
    summarise(pop_groups=sum(pop)) %>%
    mutate(pop_percent=(pop_groups/sum(pop_groups)*100)) %>% 
    left_join(panelC_square) %>% 
    mutate(text=as.character(round(pop_percent, digits = 2)))# %>% 
  #rbind(empty_line)
  
  fig2A<-ggplot(panelA_square_data,aes(x=x,y=y,fill=pop_percent))+
    geom_raster(hjust=1)+
    scale_fill_gradientn(colours=c("#F6BDC050","#FF000050"))+
    geom_text(aes(label=text), nudge_x = 2.5)+
    theme_classic()+
    theme(legend.position = "none")+
    scale_x_continuous(breaks=c(0,5,10,15,20,25,30), name= "Average Production Diversity")+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ylab("Population (%)")
  
  fig2B<-ggplot(current_system)+
    geom_point(aes(x=h_index, y=richness, col=suff_group,shape=suff_group), size=3)+
    scale_shape_manual(values=custom_shapes)+
    scale_color_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none")+
    scale_x_continuous(limits=c(0,30),n.breaks=7,minor_breaks = seq(0, 30, 10), name= "Average Production Diversity")+
    scale_y_continuous(limits=c(0,400),n.breaks=8,minor_breaks = seq(0, 400, 50), name="Taxonomic Richness")
  
  fig2C<-ggplot(panelC_square_data,aes(x=x,y=y,fill=pop_percent))+
    geom_raster(vjust=1)+
    scale_fill_gradientn(colours=c("#F6BDC050","#FF000050"))+
    geom_text(aes(label=text), nudge_y = 25)+
    theme_classic()+
    theme(legend.position = "none")+
    scale_y_continuous(limits=c(0,400),n.breaks=8, name="Taxonomic Richness")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    xlab("Population (%)")
  
  
  top_row<-plot_grid(fig2A,NULL, rel_widths = c(4,1), labels = c("A",""))
  bottom_row<-plot_grid(fig2B,fig2C, rel_widths=c(4,1),labels=c("B","C"))
  
  #Combine in facet
  fig2_facet<-plot_grid(top_row, bottom_row,ncol=1, rel_heights = c(1,4))  
  
  ggsave(plot=fig2_facet,filename = "figure_2.svg",path = "./plots", width = 18,height = 15, units="cm")
  
  return(fig2_facet)
  
}


#*******************************************************************************************
make_fig3<-function(current_system,  country_list){ #population,
  
  sf::sf_use_s2(FALSE) 
  
  custom_palette<-c("high"="#3142BEff",
                    "medium"="#61A8EDff",
                    "low"="#B1DDF9ff"
  )
  
  small_islands<-c("ASM","ATG","BHS","BRB","BMU","ABW","SLB","BRN","CPV",
                   "CYM","COM","COK","DMA","FRO","FLK","FJI","PYF",
                   "KIR","GRD","GLP","GUM","MHL","MDV","MTQ","MUS","FSM",
                   "NRU","NCL","VUT","NIU","PLW","REU","SHN","KNA","LCA",
                   "SPM","VCT","STP","SYC","TKL","TON","TTO","TCA",
                   "TUV","VGB","VIR","WLF","WSM","AIA","CHA","MYT")
  
  #World map -fig1C
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  current_country<-current_system %>% 
    left_join(country_list) %>% 
    mutate(ISO_3_CODE=case_when(FAO_code==41~"CHN", #Add China mainland
                                TRUE~ISO_3_CODE)) 
  clean_world<-world %>% 
    select(iso_a3,geometry) %>% 
    rename(ISO_3_CODE=iso_a3) 

  
  a_data<-current_country %>% 
    filter(suff_group=="complete")
  
  a_world<-clean_world %>% 
    left_join(a_data)
  
  a_points<-a_world %>% 
    filter(ISO_3_CODE %in% small_islands) %>% 
    na.omit() %>% 
    sf::st_centroid(geometry)
  
  b_data<-current_country %>% 
    filter(suff_group=="high")
  
  b_world<-clean_world %>% 
    left_join(b_data)
  
  b_points<-b_world %>% 
    filter(ISO_3_CODE %in% small_islands) %>% 
    na.omit() %>% 
    sf::st_centroid(geometry)
  
  c_data<-current_country %>% 
    filter(suff_group=="medium")
  
  c_world<-clean_world %>% 
    left_join(c_data)
  
  c_points<-c_world %>% 
    filter(ISO_3_CODE %in% small_islands) %>% 
    na.omit() %>% 
    sf::st_centroid(geometry)
  
  d_data<-current_country %>% 
    filter(suff_group=="low")
  
  d_world<-clean_world %>% 
    left_join(d_data)
  
  d_points<-d_world %>% 
    filter(ISO_3_CODE %in% small_islands) %>% 
    na.omit() %>% 
    sf::st_centroid(geometry)
  
  legend<-ggplot(data = a_world) +
    geom_sf(aes(fill = div_group))+
    scale_fill_manual(values=custom_palette, na.value = "white")+
    theme_classic()+
    theme(legend.position = "top") 
  
  fig3_A<-ggplot(data = a_world) +
    geom_sf(aes(fill = div_group))+
    geom_point(data=a_points, aes(col=div_group, size=3, geometry = geometry),
               stat = "sf_coordinates")+
    scale_fill_manual(values=custom_palette, na.value = "white")+
    scale_color_manual(values=custom_palette, na.value = "white")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),axis.title.y = element_blank()) 

  
  fig3_B<-ggplot(data = b_world) +
    geom_sf(aes(fill = div_group))+
    geom_point(data=b_points, aes(col=div_group, size=3, geometry = geometry),
               stat = "sf_coordinates")+
    scale_fill_manual(values=custom_palette, na.value = "white")+
    scale_color_manual(values=custom_palette, na.value = "white")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),axis.title.y = element_blank()) 
  
  fig3_C<-ggplot(data = c_world) +
    geom_sf(aes(fill = div_group))+
    geom_point(data=c_points, aes(col=div_group, size=3, geometry = geometry),
               stat = "sf_coordinates")+
    scale_fill_manual(values=custom_palette, na.value = "white")+
    scale_color_manual(values=custom_palette, na.value = "white")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),axis.title.y = element_blank())
  
  fig3_D<-ggplot(data = d_world) +
    geom_sf(aes(fill = div_group))+
    geom_point(data=d_points, aes(col=div_group, size=3, geometry = geometry),
      stat = "sf_coordinates")+
    scale_fill_manual(values=custom_palette, na.value = "white")+
    scale_color_manual(values=custom_palette, na.value = "white")+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),axis.title.y = element_blank())

  
  #Combine in facet
  
  #top_row<-plot_grid(fig3_A,fig3_B, labels=c("A","B"),label_size = 12)
  
  suff_labels<-c("Complete","High","Medium","Low")
  
  fig3_facet<-plot_grid(fig3_A,fig3_B, fig3_C, fig3_D,
                        labels = suff_labels,
                        hjust = 0, label_x = 0.01, 
                        rel_widths = c(1, 1),
                        ncol=2)  
  
  #Save as svg to make final cosmetic edits in inkscape
  #Combine and Add one mutual legend in nice way. (ugly in print version directly) 
  ggsave(plot=fig3_A,filename = "figure_3A.svg",path = "./plots", width = 18,height = 12, units="cm")
  ggsave(plot=fig3_B,filename = "figure_3B.svg",path = "./plots", width = 18,height = 12, units="cm")
  ggsave(plot=fig3_C,filename = "figure_3C.svg",path = "./plots", width = 18,height = 12, units="cm")
  ggsave(plot=fig3_D,filename = "figure_3D.svg",path = "./plots", width = 18,height = 12, units="cm")
  ggsave(plot=legend,filename = "figure_3_legend.svg",path = "./plots", width = 18,height = 3, units="cm")
  ggsave(plot=fig3_facet,filename = "figure_3_facet.svg",path = "./plots", width = 27,height = 19, units="cm")
  
  return(fig3_facet)
}

#**********************************************************************************************

make_SMfig1<-function(current_system, country_list){
  
  SM2_data<-current_system %>% 
    left_join(country_list) %>% 
    mutate(ISO_3_CODE=case_when(FAO_code==41~"CHN", #Add China mainland
                                TRUE~ISO_3_CODE))
  
  #World maps
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  current_world<-world %>% 
    select(iso_a3,geometry) %>% 
    rename(ISO_3_CODE=iso_a3) %>% 
    left_join(SM2_data)%>% 
    filter(!is.na(num_nutrient_suff))  
  
  
  custom_palette<-c("0-50"="#E6A157FF",
                    "51-100"="#A35638FF",
                    "101-150"="#519872FF",
                    ">150"="#A4B494FF")
  
  legend_map<-ggplot(data = current_world) +
    geom_sf(aes(fill = fruit_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()
  
  legend<-get_legend(legend_map)
  
  SM_map_protein<-ggplot(data = current_world) +
    geom_sf(aes(fill = protein_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(fill = "Percent sufficiency")
  
  SM_map_carb<-ggplot(data = current_world) +
    geom_sf(aes(fill = carb_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(fill = "Percent sufficiency")
  
  SM_map_fat<-ggplot(data = current_world) +
    geom_sf(aes(fill = fat_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_fruit<-ggplot(data = current_world) +
    geom_sf(aes(fill = fruit_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_vitA<-ggplot(data = current_world) +
    geom_sf(aes(fill = vitA_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_folate<-ggplot(data = current_world) +
    geom_sf(aes(fill = folate_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_iron<-ggplot(data = current_world) +
    geom_sf(aes(fill = iron_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_zinc<-ggplot(data = current_world) +
    geom_sf(aes(fill = zinc_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  SM_map_calcium<-ggplot(data = current_world) +
    geom_sf(aes(fill = calcium_percentsuff))+
    scale_fill_manual(values=custom_palette)+
    theme_bw()+
    theme(legend.position = "none") 
  
  nutrient_labels<-c("Protein","Carbohydrates","Fat","Fruit and Vegetables","Vitamin A",
                     "Folate","Iron","Zinc","Calcium")
  
  
  fig1_SM<-plot_grid(SM_map_protein,SM_map_carb, SM_map_fat,
                                   SM_map_fruit,SM_map_vitA, SM_map_folate,
                                   SM_map_iron,SM_map_zinc, SM_map_calcium,
                                  legend,
                                   labels = nutrient_labels,
                                   hjust = 0, label_x = 0.01, 
                                   rel_widths = c(1, 1),ncol=2)
  
  ggsave(plot=fig1_SM,filename = "figure_1SM.png",path = "./plots", width = 18,height = 26, units="cm")
  
  return(fig1_SM)
  
}
#****************************************************************************
create_app_data<-function(national_percap_data,country_list, nutrient_production, nutrient_ID_list, extended_diversity_data){
  
  calorie_limit<-2357
  protein_limit<-(calorie_limit*0.1)/4  #used lower limit in 10-15%, 1g protein is approx 4 kcal
  fat_limit<-(calorie_limit*0.15)/9   #lower limit, 15-30%, 1g fat is approx 9 kcal
  carb_limit<-(calorie_limit*0.55)/4 #lower limit, 55-75%, 1g carb approx 4 kcal
  fruit_limit<-4 #4 due to 100g unit - i.e. limit at 400g
  vitA_limit<-434 #Vit A requirement mean for adults equivalent to 434mg RE/day
  folate_limit<-320 #Average across adult age groups average daily requirement 320 mg/day
  iron_limit<-24.24 #Averaged across 18+, males and females, across 5,10,12,15% bioavailability mg/day
  zinc_limit<-7.15 #Averaged across adult age groups and across High, Moderate, Low bioavailability 7.15 mg/day
  calcium_limit <-1000 #Calcium recommended intake for adults of 1000mg/day #NOTE recommended intake vs requirement
  
  barplot_data<-national_percap_data %>%
    mutate(kcal_suff_perc=(kcal_cap_day/calorie_limit)*100) %>%
    mutate(protein_suff_perc=(protein_g_cap_day/protein_limit)*100) %>%
    mutate(fat_suff_perc=(fat_g_cap_day/fat_limit)*100) %>%
    mutate(carb_suff_perc=(carb_g_cap_day/carb_limit)*100) %>%
    mutate(vitaminA_suff_perc=(vitA_mcg_cap_day/vitA_limit)*100) %>%
    mutate(folate_suff_perc=(folate_mcg_cap_day/folate_limit)*100) %>%
    mutate(iron_suff_perc=(iron_mg_cap_day/iron_limit)*100) %>%
    mutate(zinc_suff_perc=(zinc_mg_cap_day/zinc_limit)*100) %>%
    mutate(calcium_suff_perc=(calcium_mg_cap_day/calcium_limit)*100) %>%
    mutate(fruitveg_suff_perc=(fruitveg_100g_cap_day/fruit_limit)*100) %>%
    select(UN_CODE,year,kcal_suff_perc:fruitveg_suff_perc) %>%
    pivot_longer(cols=kcal_suff_perc:fruitveg_suff_perc, names_to = "nutrient",values_to = "percentage", names_pattern = "(.*)_suff_perc") %>%
    left_join(country_list)
  
  write_csv(barplot_data,"./app_data/barplot_data.csv")
  
  circle_plot_data<-nutrient_production %>%
    left_join(country_list) %>%
    select(country_name,year, prod_source,kcal_cap_day:fruitveg_100g_cap_day) %>%
    group_by(country_name,year, prod_source) %>%
    summarise_at(vars(kcal_cap_day:fruitveg_100g_cap_day),sum, na.rm=T) %>%
    pivot_longer(cols = kcal_cap_day:fruitveg_100g_cap_day, names_to="variable", values_to = "value") %>%
    mutate(nutrient=case_when(variable=="kcal_cap_day"~"kcal",
                              variable=="protein_g_cap_day"~"protein",
                              variable=="fat_g_cap_day"~"fat",
                              variable=="carb_g_cap_day"~"carb",
                              variable=="vitA_mcg_cap_day"~"vitaminA",
                              variable=="folate_mcg_cap_day"~"folate",
                              variable=="iron_mg_cap_day"~"iron",
                              variable=="zinc_mg_cap_day"~"zinc",
                              variable=="calcium_mg_cap_day"~"calcium", 
                              variable=="fruitveg_100g_cap_day"~"fruitveg")) 
  
  write_csv(circle_plot_data,"./app_data/circle_plot_data.csv")
  
  table_data<-nutrient_production %>%
    left_join(country_list) %>%
    left_join(nutrient_ID_list) %>% 
    select(country_name,year,prod_source, species,Item, kcal_cap_day:fruitveg_100g_cap_day)
  
  write_csv(table_data,"./app_data/table_data.csv")
  
  diversity_data<-extended_diversity_data %>% 
    left_join(country_list)
  
  write_csv(diversity_data,"./app_data/diversity_data.csv")
  
  return(paste("All app data saved"))
  
}


#******************************************************
#Added wheat analysis & tables

make_wheat_analysis<-function(wheat_data,current_system,country_list){
  

clean_wheat_yearly<-wheat_data %>% 
  rename(FAO_code=`Partner Country Code (FAO)`) %>% 
  filter(Element=="Export Quantity") %>% 
  filter(Year<=2018) %>% 
  mutate(Year=as.factor(Year)) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  mutate(FAO_code=as.numeric(FAO_code)) %>% 
  group_by(FAO_code,Year) %>% 
  summarise(wheat_total=sum(Value,na.rm=T))

clean_wheat_year_sum<-clean_wheat_yearly%>%
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(total_tonnes=sum(wheat_total,na.rm = T))
  
clean_wheat<-clean_wheat_yearly %>% 
  left_join(clean_wheat_year_sum) %>% 
  mutate(frac=wheat_total/total_tonnes) %>% 
  ungroup() %>% 
  group_by(FAO_code) %>% 
  summarise(frac_mean=mean(frac,na.rm=T)) %>% 
  left_join(current_system) %>% 
  left_join(country_list) %>% 
  select(FAO_code,country_name,frac_mean,suff_group,num_nutrient_suff)

return(clean_wheat)

}
#*******************************************************
#Wheat diversity

check_wheat_diversity<-function(extended_diversity_data,current_system, country_list){
  
  check_wheat<-extended_diversity_data %>% 
    filter(year>=2014) %>% 
    filter(unique_id==37) %>%  #wheat
    group_by(FAO_code) %>% 
    summarise_at(vars(kcal_rank,kcal_cap_day),mean) %>% 
    rename(wheat_kcal_cap_day=kcal_cap_day) %>% 
    left_join(current_system)
  
  wheat_no1<-check_wheat %>% 
    filter(kcal_rank==1) %>% 
    mutate(wheat_percent=round((wheat_kcal_cap_day/kcal_cap_day)*100)) %>%
    left_join(country_list) %>% 
    select(country_name,h_index,div_group,num_nutrient_suff,suff_group,wheat_percent) %>% 
    arrange(desc(wheat_percent))
  
  return(wheat_no1)
  
  write_csv(wheat_no1,"wheat_table.csv")
}

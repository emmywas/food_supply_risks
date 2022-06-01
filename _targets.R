#make.R script

library(targets)
source("./R_files/functions.R") # defines functions

tar_option_set(packages = c("tidyverse", "rnaturalearth","rnaturalearthdata","cowplot","sf"))


list(
  
  tar_target(
    nutrient_production ,
    read_csv("./data/nutrient_production.csv")
  ),
  
  tar_target(
    country_list, 
    read_csv("./data/country_list.csv")
  ), 
  
  tar_target(
    population, 
    read_csv("./data/population.csv")
  ), 
  
  tar_target(
    nutrient_ID_list, 
    read.csv("./data/nutrient_ID_list.csv")
  ), 
  

  tar_target(
    wheat_data,
    readxl::read_excel("./data/wheat_data.xls")
  ),
  
  tar_target(
    national_percap_data,
    get_national_percap(nutrient_production,population)
  ), 

  tar_target(
    diversity_data,
    wrangle_diversity_data(nutrient_production)
  ),
  
  tar_target(
    extended_diversity_data,
    wrangle_extended_div_data(nutrient_production)
  ),
  
  tar_target(
    full_temporal_data,
    make_full_temporal_data(national_percap_data,diversity_data)
  ), 
  
  tar_target(
    current_system, 
    calc_current(full_temporal_data,country_list)
  ), 
  
  tar_target(
    clean_wheat,
    make_wheat_analysis(wheat_data,current_system,country_list)
  ),

  tar_target(
    fig1,
    make_fig1(current_system, country_list, population)
  ), 
  
  tar_target(
    fig2, 
    make_fig2(current_system, population)
  ), 
  
  tar_target(
    fig3,
    make_fig3(current_system, country_list) 
  ), 
  
  
  tar_target(
    fig1SM,
    make_SMfig1(current_system, country_list)
  ), 
  
  tar_target(
    app_data,
    create_app_data(national_percap_data,country_list, nutrient_production, nutrient_ID_list, extended_diversity_data)
  ), 
  
  
  tar_target(
    wheat_table,
    check_wheat_diversity(extended_diversity_data,current_system, country_list)
  )
  
)

#***************************************
#to run: tar_make()
#to check: tar_visnetwork()
#to load: tar_load(target_name)
#to delete cache of specific target: tar_delete("target_name")

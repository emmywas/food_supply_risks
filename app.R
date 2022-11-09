#Interactive data tool - shiny app

#***************************************************
#Load packages
library(tidyverse)
library(shiny)
library(shinydashboard)


#***************************************************
#Load and set up data 

barplot_data<-read_csv("./app_data/barplot_data.csv")
circle_plot_data<-read_csv("./app_data/circle_plot_data.csv")
diversity_data<-read_csv("./app_data/diversity_data.csv")
table_data<-read_csv("./app_data/table_data.csv")

country_choices<-barplot_data%>% 
  select(country_name) %>% 
  distinct() %>% 
  as.vector()

nutrient_choices<-c("Calories"="kcal",
                    "Protein"="protein",
                    "Fat"="fat",
                    "Carbohydrates"="carb", 
                    "Vitamin A"="vitaminA",
                    "Folate"="folate",
                    "Iron"="iron",
                    "Zinc"="zinc",
                    "Calcium"="calcium",
                    "Fruits and vegetables"="fruitveg")

circle_plot_palette<-c(
  "livestock"="#ffcc99FF",
  "agriculture"="#ffff80FF",
  "aquaculture"="#6699ffFF",
  "fisheries"="#0066ffFF"
)

#***************************************************


#############
#App Code ----
############



#UI CODE#
ui<-navbarPage(title="Food System Risks",
               
               #----------------------------------------
               tabPanel(title="Home", 
                        
                        # The Article
                        fluidRow(
                          column(3),
                          column(6,
                                 shiny::HTML("<br><br><center> <h1>The Article</h1> </center><br>"),
                                 shiny::HTML("<h5>This is an interactive database tool that is a complement to the article 
                                             Wassénius et al., 20XX, Diverse supply risks in the global food system: 
                                             A global analysis of national self-sufficiency and diversity. DOI: XXXXXXX </h5>")
                          ),
                          column(3)
                        ),
                        
                        fluidRow(
                          
                          style = "height:50px;"),
                        
                        # PAGE BREAK
                        tags$hr(),
                        
                        # The data
                        fluidRow(
                          column(3),
                          column(6,
                                 shiny::HTML("<br><br><center> <h1>The Data</h1> </center><br>"),
                                 shiny::HTML("<h5>The data used in this tool can be found in the repository at https://doi.org/10.7910/DVN/VQW92D. The methodology to create this dataset can be found in the complementary paper,
                                             especially in supplementary materials. The orginial datasources are FAOSTAT for terrestrial
                                             production data, FishStatJ for aquatic production data and the GENuS database (Smith et al. 2016)
                                             for nutritonal data.</h5>")
                          ),
                          column(3)
                        ),
                        
                        fluidRow(
                          
                          style = "height:50px;"),
                       
                        
                        # PAGE BREAK
                        tags$hr(), 
                        
                        # How to use tool
                        fluidRow(
                          column(3),
                          column(6,
                                 shiny::HTML("<br><br><center> <h1>How to use this tool</h1> </center><br>"),
                                 shiny::HTML("<h5>To explore the nutritional self-sufficiency and diversity of national food systems, 
                                             click on the tab (at the top) labelled Data Tool. Here you will find several plots and a
                                             table giving details on the food system data. You can select a year, country and nutrient
                                             you wish to dig deeper into.</h5>")
                          ),
                          column(3)
                        ),
                        
                        fluidRow(
                          
                          style = "height:50px;"),
                        
                        
                        # PAGE BREAK
                        tags$hr()
                        
                        
                        
               ), #close of Home tabpanel
               
               #----------------------------------------
               tabPanel(title="Data tool",
                        titlePanel("Diverse supply risks in the global food system"),
                        br(),
                        "Here you can investigate the level of self-sufficiency and the level of diversity of the production system.",
                        br(),
                        "Select the country, year and nutrient you want to look at:",
                        br(),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("year","Select Year:",
                                        min=1961,max=2018,value=2018, sep = ""),
                            selectInput("country", "Select Country:", 
                                        choices=country_choices),
                            selectInput("nutrient","Select Nutrient or Calories:", choices=nutrient_choices),
                            width = 2
                          ), #close of Sidebar Panel
                          mainPanel(
                            fluidRow(
                            plotOutput("bar_plot")
                            ), 
                            fluidRow(
                              column(
                                width=6, 
                                plotOutput("circle_plot")
                              ), 
                              column(
                                width=6, 
                                plotOutput("diversity_plot")
                              )
                            ),
                            fluidRow(
                              dataTableOutput("table")
                            )
                          ) #close Main Panel
                        ) #close sidebar Layout
               )#close of  Datatool tabpanel
               
) #close of Navbar & UI


#***************************************************

#SERVER CODE#
server <- function(input, output,session) {
  
  reactive_barplot_data<-reactive({
    barplot_data %>% 
      filter(country_name==input$country) %>% 
      filter(year==input$year) %>% 
      mutate(highlight=case_when(nutrient==input$nutrient~"1", 
                                 nutrient!=input$nutrient~"0"))
  })
  
  
  output$bar_plot<-renderPlot({
    ggplot(reactive_barplot_data(), aes(x=nutrient,y=percentage)) +
      geom_bar(stat = "identity", aes(fill=highlight))+
      theme_classic()+
      scale_fill_manual(values=c("1"="darkred","0"="lightgrey"))+
      geom_hline(yintercept = 100, col="red")+
      geom_hline(yintercept= 50, col="red", linetype=2)+
      geom_hline(yintercept = 150, col="red", linetype=2)+
      theme(legend.position = "none")+
      scale_y_continuous(name="Percentage sufficiency")+
      scale_x_discrete(name="Nutrients and Calories")
    
  })
  
  
  reactive_circle_plot_data<-reactive({
    circle_plot_data%>% 
      filter(country_name==input$country) %>% 
      filter(year==input$year) %>% 
      filter(nutrient==input$nutrient)
  })
  
  
  output$circle_plot<-renderPlot({
    ggplot(reactive_circle_plot_data(), aes(x="", y=value, fill=prod_source)) +
      geom_bar(stat="identity", width=1, color="white") +
      scale_fill_manual(values=circle_plot_palette)+
      coord_polar("y", start=0) +
      theme_void()+
      labs(fill = "Production Sector")
  })
  
  
  
  reactive_diversity_data<-reactive({
      diversity_data %>% 
      filter(country_name==input$country) %>% 
      filter(year==input$year)
  })
  
  calories_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=kcal_rank,y=kcal_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (kcal/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  protein_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=protein_rank,y=protein_g_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (grams protein/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  fat_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=fat_rank,y=fat_g_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (grams fat/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  carb_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=carb_rank,y=carb_g_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (grams carbohydrates/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  vitA_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=vitA_rank,y=vitA_mcg_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Mcg Vitamin A/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  folate_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=folate_rank,y=folate_mcg_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Mcg Folate/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  iron_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=iron_rank,y=iron_mg_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Mg Iron/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  zinc_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=zinc_rank,y=zinc_mg_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Mg Zinc/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  calcium_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=calcium_rank,y=calcium_mcg_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Mcg Calcium/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  fruitveg_plot<-reactive({
    ggplot(reactive_diversity_data(), aes(x=fruitveg_rank,y=fruitveg_100g_cap_day))+
      geom_point()+
      geom_line()+
      theme_bw()+
      scale_y_continuous(name="Domestic production (Fruit and vegetable 100g/cap/day)")+
      scale_x_continuous(name="Taxanomic Richness (ranked by domestic production)")
  })
  
  diversity_plot_Input <- reactive({
    switch(input$nutrient,
           "kcal" = calories_plot(),
           "protein" = protein_plot(),
           "fat" = fat_plot(),
           "carb" = carb_plot(), 
           "vitaminA" = vitA_plot(),
           "folate" = folate_plot(), 
           "iron" = iron_plot(), 
           "zinc" = zinc_plot(), 
           "calcium" = calcium_plot(), 
           "fruitveg" = fruitveg_plot()
    )
  })  

  
  output$diversity_plot<-renderPlot({
   diversity_plot_Input()
  })
  
  reactive_table_data<-reactive({
    table_data%>% 
      filter(country_name==input$country) %>% 
      filter(year==input$year)
  })
  
  output$table<-renderDataTable(reactive_table_data())
  
} #Close of server


#***************************************************
shinyApp(server = server, ui = ui)



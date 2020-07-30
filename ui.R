#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(flexdashboard)
library(plotly)
library(ggrepel)
library(grid)
library(gridExtra)
library(countrycode)
library(shinydashboard)
library(viridis)

# The list of valid books
continent <- list("All"="All",
                  "Africa" = "Africa",
              "Asia" = "Asia",
              "Europe" = "Europe",
              "North America" = "North America",
              "Oceania" = "Oceania",
              "South America" = "South America")


xinp <- list("Death Count"="Total_deaths",
             "Confirmed cases"="Total_Cases",
             "Recovered"="Total_recovered",
             "Testing"="TotalTests")

yinp <- list("Confirmed cases"="Total_Cases",
             "Death Count"="Total_deaths",
             "Recovered"="Total_recovered",
             "Testing"="TotalTests")

histInp <- list("Death over time"= "death",
                "Cases over time"="confirmed",
                "Recovery over time"="recovered")


# Manish Input files
gd<-read.csv("./data/WHO-COVID-19-global-data.csv")
region<-unique(gd$WHO_region)
Ctrs <- unique(gd$Country)

dfMP<-read.csv("./data/owid-covid-data .csv",na.strings = "")
na.omit(dfMP)

# Nitin Input
##Nitin Dataframes
nk_df_indicator <- read.csv('./data/Covid_Indicators.csv')
country_choices <- list("Afghanistan"="Afghanistan",
                        "Argentina"="Argentina",
                        "Austria"="Austria",
                        "Canada"="Canada",
                        "Chile"="Chile",
                        "China"="China",
                        "Czech Republic"="Czech Republic",
                        "India"="India",
                        "Germany"="Germany",
                        "New Zealand"="New Zealand",
                        "United Arab Emirates"="United Arab Emirates",
                        "Australia"="Australia",
                        "Belgium"="Belgium",
                        "Brazil"="Brazil",
                        "United Kingdom"="United Kingdom",
                        "United States"="United States")



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("darkly"),
    navbarPage(
        title=div(img(src="Covid-bg.png", align="top",height = 40, width = 50), "Covid Data Analysis"),
        tabPanel("Introductory Analysis",
                 sidebarPanel(
                     tags$h3("Input for Comparitive Analysis"),
                     selectInput(inputId = "cont","Select continent:",continent,multiple = TRUE),
                     actionButton(inputId = "Run","Run"),
                     hr(),
                     selectInput(inputId = "xinp","Choose measure on X-axis :",xinp),
                     selectInput(inputId = "yinp","Choose measure on y-axis:",yinp),
                     hr(),
                     h5(textOutput("ACount")),
                     h5(textOutput("DCount")),
                     tags$h3("Input for Time series analysis"),
                     selectInput(inputId = "histInp","Select feature to analyze",histInp)
                     
                 ), # sidebarPanel
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Comparitive Analysis", 
                                  fluidRow(
                                      column(12, plotOutput("cd",height = "600px"))
                                      
                                      )
                                  
                                ),
                     tabPanel("Time series Analysis", plotOutput("ct"))
                     )  
                     
                 ) # mainPanel
                 
        ), # 
        
        tabPanel("Covid response", 
                 sidebarLayout(
                     sidebarPanel(
                         tags$h3("Inputs for lockdown pattern analysis"),
                         selectInput(inputId = "selects", choices = region,
                                     label = "Select Continent", multiple = FALSE),
                         hr(),
                         tags$h3("Input for health care infra analysis"),
                         radioButtons("rdmanish", "Select Country type:",
                                      c("Tier-1" = "T1",
                                        "Tier-2" = "T2",
                                        "Tier-3" = "T3")),
                         sliderInput(inputId = "range", "Top Countries:",
                                     min = 0, max = 50,value=10),
                         actionButton(inputId = "Manish2","Update")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Lockdown pattern", 
                                      fluidRow(
                                          column(12, plotOutput("PlotManish"))
                                          
                                      )
                                      
                             ),
                             tabPanel("Health Care infra",
                                      plotOutput("PlotManish2",height=800,width=700),
                                      
                                      )
                         )
                         
                     )
                 )
                 
                 
                 
                 ),
        
        tabPanel("Economic stability and indicators", 
                 sidebarPanel(
                     tags$h3("Input for Economic Stimulus"),
                     selectInput(inputId = "cont1","Select continent:",continent),
                     radioButtons("Top", "Top Countries:",
                                  c("Top 10 Countries"="Top 10 Countries",
                                    "Top 5 Countries" = "Top 5 Countries",
                                    "All" ="All")),
                     actionButton(inputId = "Run1","Run"),
                     hr(),
                     tags$h3("Input for Covid Indicators"),
                     selectInput(inputId = "country","Select Country:",country_choices, multiple = TRUE)
                     
                 ), # sidebarPanel
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Announced economic stimulus spending", 
                                  fluidRow(
                                      column(12, plotOutput("dist1",height = "600px"))
                                      
                                  )
                                  
                         ),
                         tabPanel("Covid Indicators Response", plotOutput("dist2"))
                     )  
                     
                 )
        )
        
    ) # navbarPage
    
    
)
)



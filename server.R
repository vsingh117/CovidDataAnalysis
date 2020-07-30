#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(png)
library(gganimate)
library(hrbrthemes)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Vijay Input data
  df_case_death <- read.csv('./data/Src_covid.csv')
  vj_panel2 <- read.csv('./data/Covid-TimeSeries.csv')
  vj_panel2$day <- as.Date(vj_panel2$day, format = "%d-%m-%y")
  
  # Manish input data
  df1MP<-read.csv("./data/OxCGR_all.csv")
  gd<-read.csv("./data/WHO-COVID-19-global-data.csv")
  
  dfMP<-read.csv("./data/owid-covid-data .csv",na.strings = "")
  na.omit(dfMP)
  
  ## Nitin Dataframes
  nk_df_amount <- read.csv('./data/covid_amount.csv')
  nk_df_indicator <- read.csv('./data/Covid_Indicators.csv')
  
  # Manish code ends
  
  freq <- eventReactive(input$Run,
                        # Change when the "Rerun" button is pressed
                        {
                          withProgress({
                            setProgress(message = "Processing corpus...")
                            if(input$cont=='All')
                            {
                              df_csdt_filter <-  df_case_death
                            } else {
                              df_csdt_filter <- df_case_death %>% filter(Continent %in% input$cont)
                            }
                            
                            return(df_csdt_filter)
                            
                          })
                          
                        })
  
  vj_dfagr <- eventReactive(input$Run,
                            {
                              df_csdt_filter <- freq()
                              df_agr <- df_csdt_filter %>%
                                select(Continent,Total_deaths,Total_recovered,Total_Cases,
                                       TotalTests) %>% 
                                mutate(
                                  Deaths = sum(Total_deaths,na.rm = T),
                                  Cases = sum(Total_Cases,na.rm = T),
                                  Recovery = sum(Total_recovered,na.rm = T),
                                  Test = sum(TotalTests,na.rm = T)
                                ) %>% select(Continent,Cases,Deaths,Recovery,Test)
                              
                              df_agr <- distinct(df_agr,Cases,Deaths,Recovery,Test)
                              
                              return(df_agr)
                            })
  
  state_highlight <- c('USA','Brazil','Spain','Russia','Italy','United Kingdom',
                       'Germany','France','China','Turkey','Iran','India','Peru','Canada','Chile',
                       'Saudi Arabia','Mexico','Belgium','Austria','Israel','Ukraine','Egypt',
                       'South Korea','Romania','Kuwait','Japan','Denmark','Panama','Serbia','Maldives',
                       'Burkina Faso','Uruguay','Uganda','Equatorial Guinea','Paraguay','Jordan',
                       'San Marino','Georgia','Central African Republic','Namibia','New Caledonia',
                       'Grenada','Timor-Leste','Laos','Gambia','Curaso','Australia','New Zealand',
                       "South Africa","Nigeria","Ghana","Morocco","Ethiopia")
  
  nk_freq <- eventReactive(input$Run1,
                           # Change when the "Rerun" button is pressed
                           {
                             withProgress({
                               setProgress(message = "Processing corpus...")
                               if(input$cont1=='All' & input$Top=='Top 10 Countries')
                               {
                                 nk_df_amount <- nk_df_amount %>% arrange(desc(sum)) %>% group_by(continent_name) %>% slice(1:10)
                                 
                               } 
                               else if(input$cont1=='All' & input$Top=='Top 5 Countries')
                               {
                                 nk_df_amount <- nk_df_amount %>% arrange(desc(sum)) %>% group_by(continent_name) %>% slice(1:5)
                               }
                               else if(input$cont1=='All' & input$Top=='All')
                               {
                                 nk_df_amount 
                               }
                               else if (input$cont1 != 'All' & input$Top=='All')
                               {
                                 nk_df_amount <- nk_df_amount %>% filter(continent_name == input$cont1)
                               }
                               else if (input$cont1 != 'All' & input$Top=='Top 10 Countries')
                               {
                                 nk_df_amount <- nk_df_amount %>% arrange(desc(sum)) %>% filter(continent_name == input$cont1) %>% slice(1:10)
                                 
                               }
                               else if (input$cont1 != 'All' & input$Top=='Top 5 Countries')
                               {
                                 nk_df_amount <- nk_df_amount %>% arrange(desc(sum)) %>% filter(continent_name == input$cont1) %>% slice(1:5)
                                 
                               }
                               return(nk_df_amount)
                               
                             })
                             
                           })
  
  nk_freq2 <- eventReactive(input$country,
                            # Change when the "Rerun" button is pressed
                            {
                              withProgress({
                                setProgress(message = "Processing corpus...")
                                
                                nk_df_indicator <- nk_df_indicator %>% filter(countryname %in% input$country)
                                
                                
                                return(nk_df_indicator)
                                
                              })
                              
                            })
  
  # Function to plot scatter plot on Mail panel
  output$cd <- renderPlot({
    
    df_csdt_filter <- freq()
    df_csdt_filter <- df_csdt_filter %>%
      mutate(
        label = if_else(Country %in% state_highlight, Country, "")
      )
    
    
    ggplot(data=df_csdt_filter,aes(x=get(input$xinp),y=get(input$yinp),
                                   size=get(input$xinp),color=Continent))+
      geom_point() +
      geom_text_repel(
        aes(label = label),
        color = "black",
        size = 9/.pt, # font size 9 pt
        point.padding = 0.1, 
        box.padding = .6,
        min.segment.length = 0,
        seed = 7654
      ) + theme_bw() +
      theme(axis.title = element_blank() , 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "grey50"))
    
  })
  
  #Plot table on side layout    
  output$table <- renderPlot({ 
    tt3 <- ttheme_minimal(
      core=list(bg_params = list(fill = blues9[1:4], col=NA),
                fg_params=list(fontface=3)),
      colhead=list(fg_params=list(col="navyblue", fontface=4L))
    )
    
    df_csdt_filter <- freq()
    df_agr <- vj_dfagr()
    
    grid.arrange(
      tableGrob(df_agr,theme = tt3,rows = NULL)) 
  },width = "auto", height = "auto",res = 72, bg = 'transparent')
  
  
  ## visual data outputs
  
  output$ACount<-renderText({
    df_csdt_filter <- freq()
    df_agr <- vj_dfagr()
    paste("Number of cases in selected region :: ",df_agr$Cases)
  })
  
  output$DCount<-renderText({
    df_csdt_filter <- freq()
    df_agr <- vj_dfagr()
    paste("Number of deaths in selected region ::",df_agr$Deaths)
  })
  
  # Cases Vs Testing Analysis
  output$ct <- renderPlot({
    ggplot(data = vj_panel2,aes(x=day,y=get(input$histInp),color=region))+
      geom_histogram(bindwidth=50,stat = 'identity') + facet_wrap(~region, scales = "free") +
      theme(axis.title = element_blank() , 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "grey50"))
    
  })
  
  # Manish analysis-1
  output$PlotManish <- renderPlot({
    
    df1MP1<-df1MP %>%
      select(CountryName,	CountryCode,Dates,c6_sh,C6_Flag) %>%
      mutate(restriction = 
               case_when(
                 (c6_sh == 2 | c6_sh > 3)& C6_Flag==1   ~ "National Lockdown",
                 (c6_sh == 2 | c6_sh > 3)& C6_Flag==0   ~ "Local Lockdown",
                 (c6_sh == 1 )& C6_Flag==1  ~ "National Guidelines",
                 (c6_sh == 1 )& C6_Flag==0  ~ "Local Guidelines",
                 
                 TRUE                      ~ "No Guidance"
               )
      )  %>% 
      filter(restriction !="No Guidance")
    
    include <- gd%>%filter(WHO_region=="EURO")
    df1MP2 <-  df1MP1%>%filter(CountryName %in% include$Country)
    
    
    if (input$selects != "Other"){
      
      country<-gd %>% filter(WHO_region==input$selects)
      
      df1MP2 <- df1MP1 %>%
        filter(CountryName %in% country$Country)
    }
    
    else{
      df1MP2 <- df1MP1 %>%
        filter(CountryName %in% input$selects2)
    }
    
    ggplot(df1MP2, aes( ymd(Dates), CountryName, colour = restriction)) + theme_minimal() +
      geom_point(shape=22,size=3) + 
      labs(x="Timeline", y="Country Name")
  })
  
  # Manish plot-2
  
  man_func <- eventReactive(input$Manish2,
                            # Change when the "Rerun" button is pressed
                            {
                              withProgress({ 
                                
                                dfMP <- dfMP  %>% filter(date=='7/27/2020')
                                
                                if(input$rdmanish == "T1")
                                {
                                  df_hbed <- dfMP %>% filter(hospital_beds_per_thousand>3.5)
                                }
                                else if(input$rdmanish == "T2")
                                {
                                  df_hbed <- dfMP %>% filter(hospital_beds_per_thousand>1.5 &
                                                               hospital_beds_per_thousand<=3.5)
                                }
                                else{
                                  df_hbed <- dfMP %>% filter(hospital_beds_per_thousand<=1.5)
                                }
                                
                                if(input$range>0){
                                  df_hbed <- df_hbed %>% arrange(desc(hospital_beds_per_thousand)) %>% 
                                    head(input$range)
                                }
                                else{
                                  df_hbed <- df_hbed %>% input$range
                                }
                                
                                return(df_hbed)
                              })
                            })
  
  output$PlotManish2 <- renderPlot({
    
    df_hbed <- man_func()
    ggplot(df_hbed, aes( x=hospital_beds_per_thousand ,y=reorder(location,hospital_beds_per_thousand),fill=location))  +
      theme_bw() + geom_bar(stat = "identity") +
      geom_text(aes(label=hospital_beds_per_thousand), vjust=1.6, color="black", size=3.5) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "grey50"))+
      labs(x="Hospital beds per thousand", y="Country Name")
  })
  
  
  ###Nitin
  
  output$dist1 <- renderPlot({
    
    nk_df_amount <- nk_freq()
    theme_bluewhite <- function (base_size = 11, base_family = "") {
      theme_bw() %+replace% 
        theme(
          panel.background = element_rect(fill = "#BED3EE"),
          panel.border = element_rect(color = "lightblue", fill = NA),
        )
    }
    
    
    nk_df_amount %>%
      mutate(name = fct_reorder(countryname, sum)) %>%
      ggplot(aes(x=reorder(countryname, +sum), y=sum)) +
      geom_bar(stat="identity", fill="#8B3013", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      theme_bluewhite() +
      facet_wrap(~continent_name, scales='free')  +
      labs(x="Country", y="Monetary value of fiscal stimuli(Million USD)")
    #+    geom_text(aes(label=sum), vjust=-0.3, size=3.0) 
    
    
  })
  
  output$dist2 <- renderPlot({
    nk_df_indicator <- nk_freq2()
    ggplot(nk_df_indicator, aes(x=mdy(date), y=indicator, group=Indicator_Type
                                , color=Indicator_Type)) +
      geom_line() +
      geom_point() + theme_bw() +
      scale_color_viridis(discrete = TRUE) +
      ggtitle("Response on Indicators during Covid-19") +
      #theme_ipsum() +
      labs(x="Date", y="Indicator Index") +
      facet_wrap(~countryname)
    #+    transition_reveal(year)
    
  })
  
  
})   # Shiny server end bracket

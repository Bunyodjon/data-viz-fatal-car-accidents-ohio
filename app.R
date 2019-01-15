library(shiny)
library(dplyr)
library(tidyverse)
library(tidyr)
library(leaflet)
library(knitr)


#setwd("~/Downloads/all_files")

#Working app can be found here: https://bunyod.shinyapps.io/GroupProject/

# Import data relating to accidents and city coordinates data. 
# Data sets have been created using cleaning_code.R
# after importing data set, order DAY_WEEK1, MONTH1, HOUR1 with factor function level option
city_coordinate <- read_csv("Data/city_coordinate.csv")
accident.all <- read.csv("Data/accident_all.csv") %>%
  mutate(DAY_WEEK1 = factor(DAY_WEEK1, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
         MONTH1 = factor(MONTH1, levels=c("January", "Febraury", "March", "April", 
                                          "May", "June", "July", "August",
                                          "September", "October", "November",
                                          "December")),
         HOUR1=factor(HOUR1, levels = c("12:00am", "1:00am", "2:00am", "3:00am", 
                                        "4:00am", "5:00am", "6:00am", "7:00am",
                                        "8:00am", "9:00am", "10:00am", "11:00am",
                                        "12:00pm", "1:00pm","2:00pm","3:00pm",
                                        "4:00pm", "5:00pm", "6:00pm", "7:00pm",
                                        "8:00pm", "9:00pm", "10:00pm", "11:00pm")))

# Build the user interface portion of the Shiny App
ui <- fluidPage(
  navbarPage("Fatal Accident: 2013-2017", windowTitle = "Accident",
             #Create tab called Map: interactive map with the use of the leaflet package and Open Street Map
             tabPanel("Map",
                      # Create flexible sidebar Menu, it moves around. See About page for reference 
                      div(class="outer", 
                          tags$head(includeCSS("./Data/style.css")),  # this part uses style.css
                          leafletOutput("city_map", width="100%", height = "100%"),
                          #This is a sidebar menu
                          absolutePanel(id = "description",
                                        class = "panel panel-default",
                                        fixed = T,
                                        draggable = T,
                                        top = 90,
                                        left = "auto",
                                        right = 20,
                                        bottom = "auto",
                                        width = "25%",
                                        height = "auto",
                                        #Select city variable for y-axis
                                        h2("Traffic Explorer"),
                                        #Create interactive input options for city selection                           
                                        selectInput(inputId = "city1", 
                                                    label = "Select City:",
                                                    choices = c("Cincinnati" = "Cincinnati", 
                                                                "Dayton" = "Dayton", 
                                                                "Columbus" = "Columbus",
                                                                "Cleveland" = "Cleveland",
                                                                "Toledo" = "Toledo",
                                                                "Akron" = "Akron")),
                                        # Create  checkBoxGroup for selecting cause of an accident
                                        checkboxGroupInput(inputId = "cause", 
                                                           label = "Select Cause(s):",
                                                           choices = c("Talking on Cell Phone" = 5, 
                                                                       "Texting on Cell Phone" = 6, 
                                                                       "Eating or Drinking" = 13, 
                                                                       "Smoking Related" = 14, 
                                                                       "Distracted" = 92,
                                                                       "Other" = 1), 
                                                           selected = c(5, 6, 13, 14, 92, 1)),
                                        # Create  checkBoxGroup for selecting weather condition when an accident happened
                                        checkboxGroupInput(inputId = "weather1",
                                                           label = "Select Weather",
                                                           choices = c("Clear", 
                                                                       "Rain", 
                                                                       "Sleet or Hail", 
                                                                       "Snow",
                                                                       "Fog",
                                                                       "Other"
                                                             ),
                                                           selected = c("Clear", "Rain", "Sleet or Hail", "Snow", "Fog", "Other")),
                                        # This includes a legend image at the bottom of the sidebar
                                        div(img(src="legend.png", width="300px", height="75px", align="center"))
                                        
                          ))
                      
             ), 
             # Create a tab called Time Series 
             tabPanel("Time Series",
                      # Create interactive time series-based barplot for use on the second tab
                      sidebarPanel(
                        # title at the top of the menu
                        h4("Fatal Accident Break Down by Time and Weather"),
                      # Create options for selecting time frame which the plot will display            
                        radioButtons(inputId = "time",
                                     label = "Select TimeFrame",
                                     choices = c("Hour"="HOUR1", 
                                                 "Day Week"="DAY_WEEK1", 
                                                 "Month"="MONTH1"),
                                     selected = "HOUR1"),
                      # Create checkbox which provides more detailed breakdown of bars based on weather               
                        checkboxInput(inputId = "weather2",
                                      label = "Group by Weather",
                                      value = TRUE)
                      ),
                      mainPanel(
                        plotOutput("plot")
                      )
             ),
             #Create “About” tab which provides additional information regarding the plots on tabs 1 and 2
             tabPanel("About",
                      includeMarkdown("Data/include.md"))
  )
)
# Build the server portion of the Shiny App
server <- function(input, output) {
  
  # Establishing colors for each of the five weather conditions
  pal <- colorFactor(c("navy", "black", "cyan4", "deeppink", "purple", "red"), domain = c("Rain", "Snow", "Clear", "Fog", "Sleet or Hail", "Other"))
  # Create reactive map plot of Ohio cities
  city_coor <- reactive({
    city_coordinate %>% 
      filter(City %in% input$city1)
  })
  # Filtering map data by location, cause, and weather
  city_data <- reactive({
    accident.all %>% 
      filter((LONGITUD<=-80.60 & LONGITUD >=-84.82) & (LATITUDE >=38.52 & LATITUDE<=41.72)) %>%
      filter(WEATHER2 %in% input$weather1 & MDRDSTRD2 %in% input$cause)
  })
  
  #Creating a map plot with Leaflet package (“Map” tab)
  output$city_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data=city_data(), lng=~LONGITUD, lat=~LATITUDE, 
                       popup="Open Street Map", color=~pal(WEATHER2),
                       radius=6,
                       stroke = FALSE, fillOpacity = 0.8, 
                       clusterOptions = markerClusterOptions()) %>%
      addMarkers(lng=~Lng, lat=~Lat, data =city_coor())
    
  }) 
  
  #Make time series bar plots (‘‘Time Series” tab): filter by Ohio state, group by time and Weather and summarise
  plotdata <- reactive({ 
    accident.all %>%
      filter((LONGITUD<=-80.60 & LONGITUD >=-84.82) & (LATITUDE >=38.52 & LATITUDE<=41.72)) %>%
      group_by_(input$time, "WEATHER2") %>%
      summarise(ACCIDENT_COUNT=n()) %>%
      na.omit(HOUR1)
  })
  
  # Plotting Bar Plot with ggplot. If Group by weather is true, it will gives a breakdown of accidens with weather condition 
  output$plot <- renderPlot({ 
    if (input$weather2) {
        ggplot() + 
        geom_bar(aes_string(x=input$time, y="ACCIDENT_COUNT", fill="WEATHER2"), 
                  stat="identity", width=0.8, data=plotdata(), na.rm = TRUE) +
         theme_classic() +
         labs(y="Number of Fatal Accidents", fill="Weather",
              legend="Weather",
              title="Ohio Fatal Accidents (2013 - 2017)",
              caption="National Highway Traffic Safety Administration (NHTSA) - Fatal Accident, 2013-2017") +
         theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
               axis.title.x = element_blank(),
               plot.title = element_text(size=22, hjust = 0.5),
               axis.title.y = element_text(size=18, margin=margin(t = 0, r = 20, b = 0, l = 0)),
               legend.position = "bottom")
    } else {
      ggplot() + 
        geom_bar(aes_string(x=input$time, y="ACCIDENT_COUNT"), 
                 stat="identity", fill="red", alpha=0.5, width=0.8, data=plotdata(), na.rm = TRUE) +
        theme_classic()+
        labs(y="Number of Fatal Accidents", fill="Weather",
             legend="Weather",
             title="Ohio Fatal Accidents (2013 - 2017)",
             caption="National Highway Traffic Safety Administration (NHTSA) - Fatal Accident, 2013-2017") +
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              plot.title = element_text(size=22, hjust = 0.5),
              axis.title.y = element_text(size=18, margin=margin(t = 0, r = 20, b = 0, l = 0)),
              legend.position = "bottom")
    }

  })
  
  
}

#Create Shiny app object
shinyApp(ui = ui, server = server)

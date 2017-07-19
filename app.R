# Objective of web app ----------------------------------------------------

#User should be presented with a few charts detailing where the best places to go for a beer depending 
#on the time and day they have selected.

# Load packages -----------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(data.table)

# Load data ---------------------------------------------------------------
#source("melb_data.R")
#cafe_final <- read_csv("cafe_final.csv")
#pedestrian_final <- read_csv("pedestrian_final.csv")
#bbq_final <- read_csv("bbq_final.csv")
#bike_share_final <- read_csv("bike_share_final.csv")
#drinking_fountain_final <- read_csv("drinking_fountain_final.csv")
       

# Prep data ---------------------------------------------------------------

cafe_final_filtered <- filter(cafe_final,
                                  censusyear == 2016 & 
                                  `industry(anzsic4)description` == 'Pubs, Taverns and Bars' &
                                  cluesmallarea == 'Melbourne (CBD)')

pedestrian_final_summarised <- pedestrian_final %>%
  filter(Year == 2015 | Year == 2016) %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    total_volume = sum(Hourly_Counts),
    avg_volume = mean(Hourly_Counts)
  )

setnames(pedestrian_final_summarised, "Longitude", "long")
setnames(pedestrian_final_summarised, "Latitude", "lat")


#Attempt to join the two data sets on lat and long.

test <- full_join(cafe_final_filtered, pedestrian_final_summarised, by = "long")
joined <- read_csv("test_v2.csv")

qmplot(long, lat, data = joined, geom = "blank", maptype = "toner-lite", darken = 0.6, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .4, color = NA) +
  scale_fill_gradient2("Foot\nTraffic", low = "white", mid = "yellow", high = "mediumblue") + 
  geom_point(data = filter(joined, numberofseats > 0), aes(x = long, y = lat, size = numberofseats),
             colour = "black", alpha = 0.5) + labs(size = "Seating\navailability") + 
  theme(plot.title = element_text(hjust = 0.5))


# ------------------------------------UI------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  wellPanel(
    h1("Night out", align = "center", style = "font-family:Impact"),
    p(style = "font-family:Impact", 
      "The optimal night out", align = "center"),
    actionButton(inputId = "go",
                 label = "Update"), align = "center"),
  
  mainPanel(
    
      h2("Busiest spots in Melbourne for a night out"),
      plotOutput(outputId = "bars")

  )

)


# ------------------------------------SERVER------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$go, {
    
    print("Going")
    
    output$bars <- renderPlot(
      qmplot(long, lat, data = joined, geom = "blank", maptype = "toner-lite", darken = 0.6, legend = "topleft") +
        stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .4, color = NA) +
        scale_fill_gradient2("Foot\nTraffic", low = "white", mid = "yellow", high = "mediumblue") + 
        geom_point(data = filter(joined, numberofseats > 0), aes(x = long, y = lat, size = numberofseats),
                   colour = "black", alpha = 0.5) + labs(size = "Seating\navailability") +
        ggtitle("Busiest spots in Melbourne for a night out") + theme(plot.title = element_text(hjust = 0.5))
      
    )
    
    
  })
  
}

# ----------------------------------RUN--------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
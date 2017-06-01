# 06-observeEvent

library(shiny)
library(gtrendsR)
library(ggplot2)
library(rsconnect)

#-----------

#rsconnect::setAccountInfo(name='deep-dive',
# token='B441F4FE9DB77F1BF640BB019A99D816',
# secret='lD1t/3wYkNrK57iF1fMHuA8DTWW0K7A/bE7IFvAO')

#-----------

ui <- fluidPage(
  
  wellPanel( 
    
    h1("My shitty version of Google Trends", style = "font-family:Impact"),
    p(style = "font-family:Impact",
      "see my other stuff at my",
      a("website",
        href = "http://www.deepdiveanalytics.com.au/")
    ),
    
    textInput(inputId = "word", 
              label = "Enter word",
              value = "AFL"),
    actionButton(inputId = "go",
                 label = "Update")
  )
  ,
  
  plotOutput("chart")
  
)

server <- function(input, output) {
  
  observeEvent(input$word, { 
    usr <- "alexandredrr14@gmail.com"
    psw <- "honfleurPHR540!"
    gconnect(usr, psw)
    ggle <- gtrends(input$word)
    trnd <- ggle$trend
    
  })
  
  data <- eventReactive(input$go, {filter(trnd, keyword == isolate(input$word))})
  
  output$chart <- renderPlot({
    ggplot(data(), aes(x = start, y = hits)) + geom_line() + theme_bw() +
      labs(title = input$word) + ggtitle(input$word) + theme(plot.title = element_text(hjust = 0.5))
  })
  
}

shinyApp(ui = ui, server = server)

#-----------

#rsconnect::deployApp('/app.R')

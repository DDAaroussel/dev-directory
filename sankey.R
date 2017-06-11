library(shiny)
library(networkD3)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Home Loan Origination"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel=sidebarPanel(width=0),
    # Show a plot of the generated distribution
    mainPanel=mainPanel(
      sankeyNetworkOutput("sankey"),
      width=12
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$sankey<-renderSankeyNetwork({
    hl_nodes<-read.csv("HL_Funnel_Node.csv")
    
    hl_links<-read.csv("HL_Funnel_Link.csv")
    
    sankeyNetwork(Links=hl_links, Nodes=hl_nodes, Source="Source", Target="Target", Value="Value", NodeID="Name", fontSize=20, nodeWidth=50, height=20000, margin=NULL, nodePadding=110)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
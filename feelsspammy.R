# Description of job ------------------------------------------------------
#Attempting to build a tool which tells user if a text field they have
#chosen is spammy or not

# Load packages -----------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Load data ---------------------------------------------------------------

a <- read_csv("H:/R/r4ds/Spammy_words.csv")
a_v1 <- a$Spammy_words
spam_word_list <- str_replace(a_v1, "�", "")
spam_word_list
#Annoying unicode character removed from the list of words

# Create character lookup -------------------------------------------------

#User enters a word, and the tool must return whether the word appears in the spam list

my_subject_line <- c("Buying books")
my_subject_line_split <- data.frame(str_split(my_subject_line, " ", simplify = TRUE))
my_subject_line_split

my_subject_line_padded <- vector("character", ncol(my_subject_line_split))
for (i in seq_along(my_subject_line_split)) {
  my_subject_line_padded[i] <- str_pad(my_subject_line_split[,i], (str_length(my_subject_line_split[,i]) + 2), "both")
}

my_subject_line_padded_v2 <- data.frame(t(unlist(my_subject_line_padded)), stringsAsFactors = FALSE)

#Turn the data frame columns from factor to character
#my_subject_line_padded_v2 <- transform(my_subject_line_padded_v2, X1 = as.character(X1),
#                                 X2 = as.character(X2),
#                                 X3 = as.character(X3))

#Turn this sum below into a loop which sums to the last indexed column of the test data frame

output <- vector("double", ncol(my_subject_line_padded_v2))
for (i in seq_along(my_subject_line_padded_v2)) {
  output[i] <- sum(str_count(spam_word_list, my_subject_line_padded_v2[,i]))
}
output

if (sum(output) > 0){
  print("It's  Spammy")
} else {
  print("All good")
}

# ------------------------------------UI------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  wellPanel(
    h1("#feelsspammy", style = "font-family:Impact"),
    p(style = "font-family:Impact", 
      "Check if your email subject line is spammy"),
    
    textInput(inputId = "word",
              label = "Your heading here",
              value = "Test"),
    actionButton(inputId = "go",
                 label = "Update")
  ),
  
  textOutput("decision")
)



# ------------------------------------SERVER------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$word, {
    
  })
  
  
  
}


# ----------------------------------RUN--------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

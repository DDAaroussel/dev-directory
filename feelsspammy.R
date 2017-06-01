# Load libraries ----------------------------------------------------------
library(shiny)
library(dplyr)
library(stringr)


# Load data ---------------------------------------------------------------

spammy_names <- c("alex", "david", "mark", "michelle", "emma", "philippe")

email_subject <- c("test")


# Data prep ---------------------------------------------------------------

email_subject_split <- str_split(email_subject, " ", simplify = TRUE)

output <- vector("double", ncol(email_subject_split))
for (i in seq_along(email_subject_split)) {
  output[i] <- sum(str_count(a, email_subject_split[,i]))
}
output


# Checking for spammy string ----------------------------------------------

if (sum(output) > 0){
  print("It's  Spammy")
} else {
  print("All good")
}

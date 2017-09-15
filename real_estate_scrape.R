# Load packages -----------------------------------------------------------

#install.packages("rvest")
#install.packages("purrr")
library(rvest)
library(purrr)
library(magrittr)

# Creating a list of sold properties --------------------------------------

url_list <- character(length = 10)

for (i in 1:10){
  url_list[i] <- paste0("https://www.realestate.com.au/sold/list-",i)
}

linkcreation <- function(x) {

  link <- read_html(x) %>%
    html_nodes(".property-card__link") %>%
    html_attr("href")  %>%
    xml2::url_absolute("https://realestate.com.au/")
  
  return(link)
}

link_matrix <- sapply(url_list, linkcreation, USE.NAMES = FALSE, simplify = FALSE)
link_matrix_final <- unlist(link_matrix, recursive = TRUE, use.names = FALSE)


# Writing the different CSS selectors of each property to a list then rbinding the list ---------

results <- lapply(link_matrix_final[1:10], function(url)

  {
  
  address <- read_html(url) %>%
    html_node(".property-info-address__street") %>%
    html_text() %>%
    as.character()
  
  price <- read_html(url) %>%
    html_node(".property-info__price") %>%
    html_text() %>%
    as.character()
  
  suburb <- read_html(url) %>%
    html_node(".property-info-address__suburb") %>%
    html_text() %>%
    as.character()
  
  beds <- read_html(url) %>%
    html_node(".general-features__beds") %>%
    html_text() %>%
    as.character()
  
  baths <- read_html(url) %>%
    html_node(".general-features__baths") %>%
    html_text() %>%
    as.character()
  
  garage <- read_html(url) %>%
    html_node(".general-features__cars") %>%
    html_text() %>%
    as.character()
  
  headline <- read_html(url) %>%
    html_node(".medium-heading") %>%
    html_text() %>%
    as.character()
  
  describe <- read_html(url) %>%
    html_node(".tell-me-more__text") %>%
    html_text() %>%
    as.character()
  
  typedate <- read_html(url) %>%
    html_node(".property-info__secondary-content") %>%
    html_text() %>%
    as.character()
  
  data.frame(address, price, suburb, beds, baths, garage, headline, describe, typedate)
  
})


# Bind the different list items into a data frame -------------------------

results_v2 <- do.call(rbind, results)

# Load packages -----------------------------------------------------------

#install.packages("rvest")
#install.packages("purrr")
library(rvest)
library(purrr)
library(magrittr)
library(stringr)

# Creating a list of sold properties --------------------------------------
url_list <- character(length = 1000)

for (i in seq_along(url_list)){
  url_list[i] <- paste0("https://www.realestate.com.au/sold/list-",i)
  url_list[i] <- paste0(url_list[i],".html")
  setwd("D:/DEV/dev-directory/real_estate_scrape/html_files/lists")
  download.file(url_list[i], destfile = paste0(i,"_html.html"), quiet = TRUE)
  setwd("D:/DEV/dev-directory/real_estate_scrape")
  local_html_list[i] <- paste0(i,"_html.html")
}

linkcreation <- function(x) {
  
  setwd("D:/DEV/dev-directory/real_estate_scrape/html_files/lists")
  
  link <- read_html(x) %>%
    html_nodes(".property-card__link") %>%
    html_attr("href")  %>%
    xml2::url_absolute("https://realestate.com.au/")
  
  setwd("D:/DEV/dev-directory/real_estate_scrape")
  
  return(link)
}

#Link_matrix and link_matrix_final are overwritten each time a new url_list is generated
setwd("D:/DEV/dev-directory/real_estate_scrape/html_files/lists")
link_matrix <- sapply(local_html_list, linkcreation, USE.NAMES = FALSE, simplify = FALSE)
link_matrix_final <- unlist(link_matrix, recursive = TRUE, use.names = FALSE)
link_matrix_final_sub <- str_sub(link_matrix_final, start = -9)

# ID check on records that have already been downloaded -------------------

# master_list initialised once at the start 
#master_list <- link_matrix_final

#Filter link_matrix_final_v2 to remove any links already in the master list
link_matrix_final_v2 <- link_matrix_final[! link_matrix_final %in% master_list]

#Append each new link_matrix_final_v2 to a master list of all URLs
master_list <- append(master_list, link_matrix_final_v2)

for (i in seq_along(link_matrix_final_v2)) {
  setwd("D:/DEV/dev-directory/real_estate_scrape/html_files/properties")
  download.file(link_matrix_final[i], destfile = paste0(link_matrix_final_sub[i], "_html.html"), 
                quiet = TRUE)
  setwd("D:/DEV/dev-directory/real_estate_scrape")
}

properties_list <- paste0(link_matrix_final_sub, "_html.html")


# Writing the different CSS selectors of each property to a list then rbinding the list ---------

setwd("D:/DEV/dev-directory/real_estate_scrape/html_files/properties")

results <- lapply(properties_list, function(url)
  
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

setwd("D:/DEV/dev-directory/real_estate_scrape")

# Bind the different list items into a data frame -------------------------
results_temp <- do.call(rbind, results)

#Append these new records to a master dataset (first time need to create an empty DF)
results_master <- rbind(results_temp, results_master)

# Optionally write output to a file
#write.csv(results_master, file = "results_master.csv")

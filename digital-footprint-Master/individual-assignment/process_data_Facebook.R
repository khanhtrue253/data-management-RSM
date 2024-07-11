library(jsonlite)
library(tidyverse)
library(rvest)


#Functions to mask IP
mask_ip <- function(ip) {
  if (grepl(":", ip)) {
    # IPv6 address
    groups <- unlist(strsplit(ip, ":"))
    masked_ip <- paste0(groups[1], ":", paste(rep("X", 7), collapse = ":"))
  } else {
    # IPv4 address
    octets <- unlist(strsplit(ip, "\\."))
    masked_ip <- paste(octets[1], paste(rep("X", 3), collapse = "."), sep = ".")
  }
  return(masked_ip)
}


#Function to extract data from html
extract_table <- function(html_content) {
  table <- html_content %>%
    html_node("table") %>%
    html_table()
  return(table)
}


#Extract the location approximation that Facebook has
tables <- list()
for (i in 1:3) {
  page_link <- paste0("Facebook Data/download_data_logs/data_types/2_profile_and_device_interaction_information/page_"
                      , i, ".html")
  html_content <- read_html(page_link)
  table <- extract_table(html_content)
  table <- table[-1, ]
  tables[[i]] <- table
}
facebookLocation <- do.call(bind_rows, tables)
facebookLocation <- facebookLocation |>
  select(Date, `Current address`, `Current city`, `Current ZIP code`, `Primary country`, `Primary region`, 
         `Primary city`, `Primary ZIP code`) |>
  mutate(`Primary ZIP code` = paste0(substr(`Primary ZIP code`, 1, 1), "XX", substr(`Primary ZIP code`, 4, 4)))


#Extract the IP Address from logins
facebookIpAddress <-fromJSON("Facebook data/download_data_logs/data_types/security_and_login_information/ip_address_activity.json")
facebookIpAddress <- as.data.frame(facebookIpAddress[["used_ip_address_v2"]])
facebookIpAddress <- facebookIpAddress |>
  select(ip, action, timestamp) |>
  mutate(ip = sapply(ip, mask_ip)) |>
  mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01", tz="UTC"))

#Extract information about the time I spent on posts
tables <- list()
for (i in 1:17) {
  page_link <- paste0("Facebook Data/download_data_logs/data_types/4_information_about_content_you_ve_viewed_on_facebook/page_"
                      , i, ".html")
  html_content <- read_html(page_link)
  table <- extract_table(html_content)
  table <- table[-1, ]
  tables[[i]] <- table
}
facebookContentViewed <- do.call(bind_rows, tables)
facebookContentViewed <- facebookContentViewed |>
  mutate(`Content` = paste0(substr(`Content`, 1, 25), "XX"))

#Save objects
save(facebookLocation, facebookIpAddress, facebookContentViewed,
     file = ("Processed_Data/process_data_Facebook.RData"))
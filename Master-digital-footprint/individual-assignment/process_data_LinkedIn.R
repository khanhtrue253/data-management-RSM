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

linkedinLogins <- read.csv("LinkedIn data/Logins.csv")
linkedinLogins <- linkedinLogins |>
  select(Login.Date, IP.Address) |>
  mutate(IP.Address = sapply(IP.Address, mask_ip))

#Save objects
save(linkedinLogins,
     file = ("Processed_Data/process_data_LinkedIn.RData"))
library(jsonlite)
library(tidyverse)
library(rvest)

#Function to mask IP address
mask_ip <- function(ip) {
  if (grepl(":", ip)) {
    # IPv6 address
    groups <- unlist(strsplit(ip, ":"))
    masked_ip <- paste0(groups[1], ":", paste(rep("X", 7), collapse = ":"))
  } else {
    # IPv4/Other address
    octets <- unlist(strsplit(ip, "\\."))
    masked_ip <- paste(octets[1], paste(rep("X", 3), collapse = "."), sep = ".")
  }
  return(masked_ip)
}


#Get login IP address
riotGamesLocation <- fromJSON("Riot Games data/riotAccount/atlas_login_event.json")
riotGamesLocation <- riotGamesLocation %>%
  select(event_time_utc, event_type, ip_address) |>
  mutate(ip_address = sapply(ip_address, mask_ip))
riotGamesLocation <- as.tibble(riotGamesLocation)


#Get IP address when purchasing an item in-game store
riotGamesPurchaseLocation <-  fromJSON("Riot Games data/leagueoflegends/store_transactions.json")
riotGamesPurchaseLocation <- riotGamesPurchaseLocation |>
  select(created, ip_address, item_id, type) |>
  mutate(ip_address = sapply(ip_address, mask_ip))
riotGamesPurchaseLocation <- as.tibble(riotGamesPurchaseLocation)


#Save relevant data from Riot Games platform
save(riotGamesLocation,  riotGamesPurchaseLocation,
     file = ("Processed_Data/process_data_RiotGames.RData"))
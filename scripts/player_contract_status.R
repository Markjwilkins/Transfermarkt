# Pull player bios from Transfermarkt
# Including: player name, age, contract status

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(worldfootballR)


# Player info function ----------------------------------------------------

##edited worldfootballr::tm_player_bio() function

player_bio <- function(player_url) {
  player_page <-
    tryCatch(
      xml2::read_html(player_url),
      error = function(e)
        NA
    )
  
  X1 <-
    player_page %>% rvest::html_nodes(".info-table__content--regular") %>% rvest::html_text() %>% stringr::str_squish()
  
  X2 <-
    player_page %>% rvest::html_nodes(".info-table__content--bold") %>% rvest::html_text() %>% stringr::str_squish()
  
  a <-
    cbind(X1, X2) %>% data.frame()
  
  a <-
    a %>%
    tidyr::pivot_wider(names_from = .data$X1, values_from = .data$X2) %>%
    janitor::clean_names()
  
  return(a)
}


# Combine -----------------------------------------------------------------

##pull league URLs
league_one_teams <-
  tm_league_team_urls(start_year = 2021, league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")

##pull player links from team URLs
player_links <-
  league_one_teams %>%
  map(~ tm_team_player_urls(.x)) %>%
  unlist() %>%
  tibble()

##retrieve all bios & small clean up
all_bios <-
  player_links$. %>%
  map_df(possibly(~ player_bio(.x), otherwise = NA)) %>%
  rename(player_name = name_in_home_country) %>%
  filter(!is.na(player_name)) %>%
  select(player_name, everything())

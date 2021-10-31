# Pull player bios from Transfermarkt
# Including: player name, age, contract status

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(worldfootballR)
library(progress)

# Player info function ----------------------------------------------------

##edited worldfootballr::tm_player_bio() function

player_bio <- function(player_url) {
  pb$tick()
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
  
  ##initial scrape retruned NA name values so grabbed the name from the player header
  X3 <- 
    player_page %>% rvest::html_nodes("h1") %>% rvest::html_text()
  
  a <-
    cbind(X1, X2, X3) %>% data.frame()
  
  a <-
    a %>%
    tidyr::pivot_wider(names_from = .data$X1, values_from = .data$X2) %>%
    rename(player_name = X3) %>% 
    janitor::clean_names()
  
  return(a)
}


# Combine -----------------------------------------------------------------

##pull league URLs
teams <-
  tm_league_team_urls(start_year = 2021, league_url = "https://www.transfermarkt.com/championship/startseite/wettbewerb/GB2")

##pull player links from team URLs
player_links <-
  teams %>%
  map(~ tm_team_player_urls(.x)) %>%
  unlist() %>%
  tibble()

pb <- progress::progress_bar$new(total = length(player_links$.),
                                 format = "  downloading :current/:total (:percent) eta: :eta ")

##retrieve all bios & small clean up
all_bios <-
  player_links$. %>%
  map_df(possibly(~ player_bio(.x), otherwise = NA)) %>%
  select(player_name, everything())

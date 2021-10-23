# Load Packages -----------------------------------------------------------
library(tidyverse)
library(worldfootballR)
library(rvest)
library(progress)

##league links for top 4 English leagues
league_links <-
  c(
    "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1",
    "https://www.transfermarkt.com/championship/startseite/wettbewerb/GB2",
    "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3",
    "https://www.transfermarkt.com/league-two/startseite/wettbewerb/GB4"
  )

##retrieve team links
team_links <-
  league_links %>%
  map(~ tm_league_team_urls(league_url = .x, start_year = 2021)) %>%
  unlist()

##progress bar when using map
pb <-
  progress::progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                             total = length(team_links))


# function to pull team logos ---------------------------------------------
league_logos <- function(url) {
  pb$tick()
  
  team <- url %>%
    read_html() %>%
    html_nodes("h1") %>%
    html_text() %>%
    as.character() %>%
    stringr::str_squish()
  
  imgsrc <- url %>%
    read_html() %>%
    html_nodes(".dataBild img") %>%
    html_attr("src")
  
  df <-
    cbind(team_name = team, image_link = imgsrc) %>%
    data.frame()
  
  return(df)
}

##extract team logo url
data <-
  team_links %>%
  map( ~ league_logos(url = .x))

##add league name
data <-
  data %>%
  tibble() %>%
  unnest_wider(1) %>%
  mutate(
    league_name = case_when(
      row_number() %in% c(1:20) ~ "Premier League",
      row_number() %in% c(21:44) ~ "Championship",
      row_number() %in% c(45:68) ~ "League One",
      TRUE ~ "League Two"
    )
  )

##save output
saveRDS(data, "english_league_logos.RDS")



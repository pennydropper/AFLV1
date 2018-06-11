# 
# Provides methods for importing AFL match data.
#
# Created by James Northrop on 25-Feb-2018
#
# 

source("./code/setup.R")


# Source tipping results --------------------------------------------------

tipping.res.url <- "http://tipping.johnslyng.com.au/afl/reports/tipperladderreport23.htm"

tipping.res.raw <- read_html(tipping.res.url) %>%
  html_nodes('td:nth-child(2)') %>%
  html_text()

# write_rds(tipping.res.raw, paste0(data, "/", "tipping.res.url.rds"))
# tipping.res.raw <- read_rds(paste0(data, "/", "tipping.res.url.rds"))

tipping.res.raw

(tipping.res.head <- tipping.res.raw[1])

tipping.res <- tipping.res.raw[c(3:(length(tipping.res.raw)-2))] %>%
  as.numeric()



# Source team lists -------------------------------------------------------

tm.map <- read_csv(paste0(data, "/", "teams.csv"),
                   col_types = cols(
                     tm = col_character(),
                     tm.abbr = col_character(),
                     tm.alt1 = col_character()))


# Source historical odds --------------------------------------------------
# Based off data extracted from http://www.aussportsbetting.com/data/historical-afl-results-and-odds-data/
# Data downloaded on 4 March 2018

aus.bet.raw <- read_excel(paste0(data, "/", "afl.xlsx"), sheet = "Data", col_names = TRUE, skip = 1)

aus.bet <- aus.bet.raw %>%
  # glimpse()
  left_join(tm.map, by = c(`Home Team` = "tm.alt1")) %>%
  left_join(tm.map, by = c(`Away Team` = "tm.alt1"), suffix = c(".hm", ".aw")) %>%
  select(date = Date,
         tm.hm,
         tm.aw,
         final = `Play Off Game?`,
         Q4.diff.pred = `Home Line Close`,
         opp.pred = `Away Line Close`,
         tm1.Q4.tot = `Home Score`,
         tm2.Q4.tot = `Away Score`) %>%
  mutate(date = as_date(date),
         seas = year(date),
         Q4.diff.pred = -Q4.diff.pred,
         opp.pred = -opp.pred,
         tm1.Q4.lead.abs = tm1.Q4.tot - tm2.Q4.tot,
         pred.res = if_else(sign(Q4.diff.pred) == sign(tm1.Q4.lead.abs), 1, 0))

# aus.bet %>%
#   filter(is.na(final),
#          as.character(seas) >= "2013") %>%
#   group_by(seas) %>%
#   summarise(tip.sc = sum(pred.res, na.rm = TRUE),
#             marg.diff.avg = mean(Q4.diff.pred - tm1.Q4.lead.abs, na.rm = TRUE))

# Source AFL Results ------------------------------------------------------

afltables <- 'http://afltables.com/afl/seas/'




build.results <- function(seasons = c(2013:2017)) {
  # Builds a tibble of results
  
  seas.res <- vector("list", length(seasons))
  names(seas.res) <- seasons
  
  for (yr in seq_along(seasons)) {
    seas.res[seasons[[yr]]] <- read.afl.games(seas = seasons[[yr]])[[1]]
    # First item of the function represents actual results
  }
  
  bind_rows(seas.res)
  
}




# Source AFL Results from Wikipedia ---------------------------------------

afl.wiki <- 'https://en.wikipedia.org/wiki/'
afl.wiki.suff <- "_AFL_season"

read.afl.games.wiki <- function(src.url = afl.wiki, src.url.suff = afl.wiki.suff, seas = 2018) {
  # Scrapes AFL games from website and returns a list with 2 items.
  # Item 1 of the list is a tibble of the played games
  # Item 2 of the list are the future games
  
  afl.season <- vector("list", 2)
  
  seas.url <- paste0(src.url, seas, src.url.suff, ".html")
  
  seas.raw <- read_html("https://en.wikipedia.org/wiki/2018_AFL_season") %>%
    html_nodes('th, td') %>%
    html_text()
  
  seas.raw %>% head(80)
  
  # Initialise Objects ------------------------------------------------------
  
  match.det <- vector("list", 8)
  names(match.det) <-  c("rnd", "gm", "tm1", "tm1.score",
                         "tm2", "tm2.score", "date.time", "att.venue")
  
  afl.rounds <- vector("list", 23)
  
  # Load Season Data --------------------------------------------------------
  
  for (i in seq_along(seas.raw)) {
    if (str_sub(seas.raw[[i]], 1, 5) == "Round") {
      # Start of a new round. First is in position 14
      afl.round <- as.integer(str_sub(seas.raw[[i]], 6, 8))
      
      # i = 20
      
      if (str_detect(seas.raw[[i + 7]], "crowd")) {  # May need to add a sum to convert from vector to number
        # Round already played
        fut.rnd <- 0
      } else {
        fut.rnd <- 1
      }
      
      games.num <- sum(seas.raw[seq(i + 5, by = 6, length.out = 9)] == "vs.") +
        sum(str_detect(seas.raw[seq(i + 5, by = 6, length.out = 9)], "def\\.")) +
        sum(str_detect(seas.raw[seq(i + 5, by = 6, length.out = 9)], "drew with"))
        
      
      # games.num <- str_count(seas.raw[[i + 2 - fut.rnd]], paste0("-", seas))
      # print(str_c("Round: ", str_pad(afl.round, 2, side = "left"),
      #             " - number of games: ", games.num, sep = ""))
      
      # Initialise the round.games list
      round.games <- vector("list", games.num)
      
      for (game in 1:games.num) {
        # game = 1
        # For each game in the round (up to the point of a bye)
        game.offset <- 6 * (game - 1) + 3 # - fut.rnd  # offset from the Round header in seas.raw
        
        if (str_detect(seas.raw[[i + game.offset + 1]], "Bye") == TRUE) {
          # Assume no more games
          print(str_c("Break at game ", game))
          break()
          
        } else {
          # Derive the match details
          match.det[[1]] <- afl.round
          match.det[[2]] <- game
          match.det[[3]] <- seas.raw[[i + game.offset + 1]]  #tm1 includes the score
          match.det[[4]] <- seas.raw[[i + game.offset + 1]]  #tm1.score
          match.det[[5]] <- seas.raw[[i + game.offset + 3]]  #tm2, includes the score
          match.det[[6]] <- seas.raw[[i + game.offset + 3]]  #tm2 score
          match.det[[7]] <- seas.raw[[i + game.offset + 0]]  #date, time etc, eg "Thursday, 22 March (7:25 pm)"
          match.det[[8]] <- seas.raw[[i + game.offset + 4]]  #Attendance, venue
          
          round.games[[game]] <- match.det  # recursively add the matches to round.games list   
        }
        
        
      }
      
      afl.rounds[[afl.round]] <- bind_rows(round.games)
    }
  }
  
  season.games <- bind_rows(afl.rounds)
  
  games.played <- season.games %>%
    filter(str_detect(att.venue, "crowd:")) %>%
    mutate(seas = seas,
           tm1 = str_extract(tm1, ".*(?= \\d)"),
           tm2 = str_extract(tm2, ".*(?= \\d)"),
           venue = str_extract(att.venue, ".*(?= \\()"),
           game.date = dmy(paste0(str_extract(date.time, "(?<=, ).*(?=\\()"), seas)),
           tm1.Q3.B = NA,
           tm1.Q3.G = NA,
           tm1.Q4.B = as.numeric(str_extract(tm1.score, "(?<=\\.)\\d+")),
           tm1.Q4.G = as.numeric(str_extract(tm1.score, "(?<=\\s)\\d+")),
           tm2.Q3.B = NA,
           tm2.Q3.G = NA,
           tm2.Q4.B = as.numeric(str_extract(tm2.score, "(?<=\\.)\\d+")),
           tm2.Q4.G = as.numeric(str_extract(tm2.score, "(?<=\\s)\\d+"))) %>% 
    select(-c(tm1.score, tm2.score, date.time, att.venue))
  
  
  # afl.results %>% glimpse()
  
  afl.season[[1]] <- games.played
  
  games.future <- season.games %>%
    filter(str_detect(att.venue, "crowd:") == FALSE) %>%  # filter rows to include non-played games only
    mutate(venue = att.venue,
           date.time = if_else(str_detect(date.time, "–"), ", 25 August (", date.time),
           # Round 23 dates are TBC so place them on Saturday, 25/8
           game.date = dmy(paste0(str_extract(date.time, "(?<=, ).*(?=\\()"), seas))) %>%
    select(rnd:tm1, tm2, venue, game.date) %>%
    mutate(seas = seas)
  
  afl.season[[2]] <- games.future
  
  afl.season
  
}


build.results <- function(seasons = c(2013:2017)) {
  # Builds a tibble of results
  
  seas.res <- vector("list", length(seasons))
  names(seas.res) <- seasons
  
  for (yr in seq_along(seasons)) {
    seas.res[seasons[[yr]]] <- read.afl.games(seas = seasons[[yr]])[[1]]
    # First item of the function represents actual results
  }
  
  bind_rows(seas.res)
  
}




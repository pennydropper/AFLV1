

# source("./code/setup.R")
# 
# source("./code/source_external.R", local = TRUE)



# Build list of results ---------------------------------------------------

# afl.results <- build.results(c(2013:2017)) %>%
#   select(seas, everything())
# 
# write_rds(afl.results, paste0(data, "/", "afl.results.rds"))

afl.results <- read_rds(paste0(data, "/", "afl.results.rds"))


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
         tm.hm = `Home Team`,
         tm.aw = `Away Team`,
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

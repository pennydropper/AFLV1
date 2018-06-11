

tot.sc <- function(goals, behinds) {
  # Calculates the total score
  
  goals * 6 + behinds
  
}

q4.acc.df <- function(res.df = afl.results) {
  # Returns a tibble with the Q4 accuracy of each team
  
  df <- res.df %>%
    mutate(tm1.Q4.acc = tm1.Q4.G / (tm1.Q4.G + tm1.Q4.B),
           tm2.Q4.acc = tm2.Q4.G / (tm2.Q4.G + tm2.Q4.B),
           tm1.Q4.tot = tot.sc(tm1.Q4.G, tm1.Q4.B),
           tm2.Q4.tot = tot.sc(tm2.Q4.G, tm2.Q4.B),
           tm1.res = if_else(tm1.Q4.tot > tm2.Q4.tot, 1,
                             if_else(tm1.Q4.tot == tm2.Q4.tot, 0, -1)),
           tm1.rel.acc = tm1.Q4.acc - tm2.Q4.acc,
           tm1.Q4.ratio.sc = tm1.Q4.tot / (tm1.Q4.tot + tm2.Q4.tot),
           tm1.Q4.ratio.sh = (tm1.Q4.G + tm1.Q4.B) / ((tm1.Q4.G + tm1.Q4.B) + ((tm2.Q4.G + tm2.Q4.B))),
           hm = 1)
  
  df %>%
    select(seas, rnd, gm, tm = tm1, tm.Q4.acc = tm1.Q4.acc, tm.res = tm1.res,
           tm.rel.acc = tm1.rel.acc,
           tm.Q4.ratio.sc = tm1.Q4.ratio.sc,
           tm.Q4.ratio.sh = tm1.Q4.ratio.sh,
           hm) %>%
    bind_rows(df %>%
                mutate(tm2.res = -sign(tm1.res),
                       tm2.rel.acc = -tm1.rel.acc) %>%
                select(seas, rnd, gm, tm = tm2, tm.Q4.acc = tm2.Q4.acc, tm.res = tm2.res,
                       tm.rel.acc = tm2.rel.acc,
                       tm.Q4.ratio.sc = tm1.Q4.ratio.sc,
                       tm.Q4.ratio.sh = tm1.Q4.ratio.sh) %>%
                mutate(tm.Q4.ratio.sc = 1 - tm.Q4.ratio.sc,
                       tm.Q4.ratio.sh = 1 - tm.Q4.ratio.sh,
                       hm = 0)) %>%
    mutate(tm.res = factor(tm.res, levels = -1:1, labels = c("loss", "draw", "win"))) %>%
    arrange(seas, tm)
  
}

calc.margin <- function(res.df = afl.results) {
  # Returns a tibble with the margins for each team for each game
  
  df <- res.df %>%
    mutate(tm1.Q3.tot = tot.sc(tm1.Q3.G, tm1.Q3.B),
           tm1.Q4.tot = tot.sc(tm1.Q4.G, tm1.Q4.B),
           tm2.Q3.tot = tot.sc(tm2.Q3.G, tm2.Q3.B),
           tm2.Q4.tot = tot.sc(tm2.Q4.G, tm2.Q4.B),
           tm1.Q3.lead.abs = tm1.Q3.tot - tm2.Q3.tot,
           tm1.Q4.lead.abs = tm1.Q4.tot - tm2.Q4.tot,
           tm1.Q3.lead.rel = tm1.Q3.tot / (tm1.Q3.tot + tm2.Q3.tot),
           tm1.Q4.lead.rel = tm1.Q4.tot / (tm1.Q4.tot + tm2.Q4.tot)) %>%
    group_by(seas) %>%
    mutate(tm1.Q3.lead.abs.sd = scale(tm1.Q3.lead.abs),
           tm1.Q4.lead.abs.sd = scale(tm1.Q4.lead.abs),
           tm1.Q3.lead.rel.sd = scale(tm1.Q3.lead.rel),
           tm1.Q4.lead.rel.sd = scale(tm1.Q4.lead.rel),
           tm1.Q34.abs.sd.ch = tm1.Q4.lead.abs.sd - tm1.Q3.lead.abs.sd,
           tm1.Q34.rel.sd.ch = tm1.Q4.lead.rel.sd - tm1.Q3.lead.rel.sd) %>%
    ungroup()
  
  df %>%
    mutate(home = 1) %>%
    select(seas, rnd, game.date, tm = tm1, venue, opp = tm2,
           home,
           matches("lead"),
           matches("Q34")) %>%
    rename(tm.Q3.lead.abs = tm1.Q3.lead.abs,
           tm.Q4.lead.abs = tm1.Q4.lead.abs,
           tm.Q3.lead.rel = tm1.Q3.lead.rel,
           tm.Q4.lead.rel = tm1.Q4.lead.rel,
           tm.Q3.lead.abs.sd = tm1.Q3.lead.abs.sd,
           tm.Q4.lead.abs.sd = tm1.Q4.lead.abs.sd,
           tm.Q3.lead.rel.sd = tm1.Q3.lead.rel.sd,
           tm.Q4.lead.rel.sd = tm1.Q4.lead.rel.sd,
           tm.Q34.abs.sd.ch = tm1.Q34.abs.sd.ch,
           tm.Q34.rel.sd.ch = tm1.Q34.rel.sd.ch) %>%
    # glimpse()
    bind_rows(df %>%
                mutate(tm.Q3.lead.abs = -tm1.Q3.lead.abs,
                       tm.Q4.lead.abs = -tm1.Q4.lead.abs,
                       tm.Q3.lead.rel = 1 - tm1.Q3.lead.rel,
                       tm.Q4.lead.rel = 1 - tm1.Q4.lead.rel,
                       tm.Q3.lead.abs.sd = -tm1.Q3.lead.abs.sd,
                       tm.Q4.lead.abs.sd = -tm1.Q4.lead.abs.sd,
                       tm.Q3.lead.rel.sd = -tm1.Q3.lead.rel.sd,
                       tm.Q4.lead.rel.sd = -tm1.Q4.lead.rel.sd,
                       tm.Q34.abs.sd.ch = -tm1.Q34.abs.sd.ch,
                       tm.Q34.rel.sd.ch = -tm1.Q34.rel.sd.ch,
                       home = 0) %>%
                select(seas, rnd, game.date, tm = tm2, venue, opp = tm1,
                       home,
                       matches("^tm\\.")))
  
}

# rm(list = "df")



upd.ratings <- function(games.played, seas.rnd, rnd.ratings, p.diff.act = "tm.Q4.lead.abs") {
  # Updates the ratings after the round is played
  # Logic adapted from https://thearcfooty.com/2016/12/29/introducing-the-arcs-ratings-system/ in Feb-18
  
  df <- games.played %>%
    filter(rnd == seas.rnd,
           opp != "Bye") %>%
    select(-ends_with("Q3.tot")) %>%
    left_join(rnd.ratings, by = "tm") %>%
    rename(rtng.tm1.i = rating) %>%
    left_join(rnd.ratings, by = c("opp" = "tm")) %>%
    rename(rtng.tm2.i = rating) %>%
    mutate(rtng.diff = rtng.tm1.i - rtng.tm2.i,
           res.pred = (1 + 10^(-rtng.diff / theta))^-1,
           Q4.diff.act = !!as.name(p.diff.act),
           Q4.diff.pred = -log((1 - res.pred) / res.pred) / p.fact,
           res.act = 1 / (1 + exp(-p.fact * Q4.diff.act)),
           rtng.tm1.o = rtng.tm1.i + k.fact * (res.act - res.pred),
           rtng.tm2.o = rtng.tm2.i - k.fact * (res.act - res.pred)) %>%
    select(rnd, tm, opp, rtng.tm1.o, rtng.tm2.o, Q4.diff.pred, win.prob = res.pred)
  
  rtng.tm1 <- df %>%
    select(tm = tm,
           rating = rtng.tm1.o,
           Q4.diff.pred,
           win.prob,
           round = rnd)
  
  rtng.tm2 <- df %>%
    select(tm = opp,
           rating = rtng.tm2.o,
           Q4.diff.pred,
           win.prob,
           round = rnd) %>%
    mutate(Q4.diff.pred = -Q4.diff.pred,
           win.prob = 1 - win.prob)
  
  rtng.tm.bye <- rnd.ratings %>%
    select(tm, rating) %>% 
    anti_join(rtng.tm1, by = "tm") %>%
    anti_join(rtng.tm2, by = "tm") %>%
    mutate(Q4.diff.pred = NA,
           win.prob = NA,
           round = seas.rnd)
  
  bind_rows(rtng.tm1,
            rtng.tm2,
            rtng.tm.bye)
  
}




act.with.pred <- function(res.df = afl.results, p.optim.by = optim.by) {
  # Merges the actual results with the predicted results
  
  calc.margin(res.df) %>%
    filter(home == 1) %>%
    # mutate(seas = factor(seas),
    #        rnd = factor(rnd)) %>%
    select(seas:home, !!as.name(p.optim.by)) %>%
    left_join(full.ratings, by = c("seas", "rnd", "tm")) %>%
    rename(rating.hm = rating) %>%
    left_join(full.ratings, by = c("seas", "rnd", "opp" = "tm"), suffix = c(".tm", ".aw")) %>%
    rename(rating.opp = rating) %>%
    mutate(pred.hm = if_else(rating.hm > rating.opp, 1,
                             if_else(rating.hm < rating.opp, -1, 0)), # allow for predicted draw
           act.hm = sign(tm.Q4.lead.abs),
           pred.res = as.integer(pred.hm == act.hm),
           marg.diff.abs = abs(Q4.diff.pred.tm - tm.Q4.lead.abs))
  
}

# act.with.pred(afl.results, optim.by) %>%
#   select(seas == 2017) %>%
#   glimpse()


aus.bet.merge <- function(df = aus.bet) {
  # "gathers" the ausbet results
  
  bind_rows(
    df %>%
      select(game.date = date, seas, final, tm = tm.hm, opp = tm.aw, bet.pred.marg = Q4.diff.pred) %>%
      mutate(hm = 1),
    df %>%
      select(game.date = date, seas, final, tm = tm.aw, opp = tm.hm, bet.pred.marg = opp.pred) %>%
      mutate(hm = 0)
  )
  
}

# aus.bet.merge() %>%
#   filter(tm == "Adelaide", seas == 2017) %>%
#   arrange(desc(game.date))
# 
# aus.bet %>%
#   filter(date == ymd("2017/8/27"),
#          tm.aw == "Adelaide") %>%
#   glimpse()


pred.nxt.rnd <- function(full.ratings, fut.games) {
  # Reads in the season fixtures and generates tips and forecasts
  
  # fut.games <- read.afl.games.wiki()[[2]]
  
  rtngs <- full.ratings %>%
    filter(seas == max(seas), !is.na(rnd)) %>%
    filter(rnd == max(rnd)) %>%
    select(tm, rating)
  
  nxt.rnd <- fut.games %>%
    filter(rnd == min(rnd)) %>%
    left_join(rtngs, by = c("tm1" = "tm")) %>%
    rename(rtng.tm1.i = rating) %>%
    left_join(rtngs, by = c("tm2" = "tm")) %>%
    rename(rtng.tm2.i = rating) %>%
    mutate(rtng.diff = rtng.tm1.i - rtng.tm2.i,
           win.prob = (1 + 10^(-rtng.diff / theta))^-1,
           Q4.diff.pred = -log((1 - win.prob) / win.prob) / p.fact)
  
  nxt.rnd
  
}

calc.ratings <- function(afl.results, seas.sel, init.ratings, optim.by = "tm.Q4.lead.abs") {
  # Progresses through each round and generates the ratings
  
  # calc.ratings(games.played, 2018, ratings.last.sav)
  # afl.results <- games.played
  # seas.sel <- 2018
  # init.ratings <- ratings.last.sav
  
  afl.res.seas <- calc.margin(afl.results) %>%
    filter(seas == seas.sel, home == 1)
  # We only need 1 record per game, i.e. from home team's perspective
  
  # vector of each round in the season
  seas.rnds <- afl.res.seas %>%
    pull(., rnd) %>%
    unique(.) %>%
    sort(.)
  
  # Set up object to store round by round ratings 
  rnd.ratings <- vector("list", length(seas.rnds) + 1)
  
  # Set up ratings for first round in results
  rnd.ratings[[1]] <- init.ratings
  
  # Move through each played round and update team ratings
  for (r in seq_along(seas.rnds)) {
    # print(seas.rnds[[r]])
    rnd.ratings[[r + 1]] <- upd.ratings(afl.res.seas, seas.rnds[[r]], 
                                        rnd.ratings[[r]], optim.by) 
  } 

  
  seas.ratings <- bind_rows(rnd.ratings[-1], .id = "rnd") %>%
    mutate(rnd = factor(rnd, levels = seas.rnds, labels = seas.rnds),
           rnd = as.integer(as.character(rnd)),
           seas = seas.sel)
  
  return(seas.ratings)
  
}

read.afl.games <- function(src.url = afltables, seas = 2018) {
  # Scrapes AFL games from website and returns a list with 2 items.
  # Item 1 of the list is a tibble of the played games
  # Item 2 of the list are the future games
  
  afl.season <- vector("list", 2)
  
  seas.url <- paste0(src.url, seas, ".html")
  
  seas.raw <- read_html(seas.url) %>%
    html_nodes('td') %>%
    html_text()
  
  # Initialise Objects ------------------------------------------------------
  
  match.det <- vector("list", 7)
  names(match.det) <-  c("rnd", "gm", "tm1", "tm1.score",
                         "tm2", "tm2.score", "date.time.att.venue")
  
  afl.rounds <- vector("list", 23)
  
  # Load Season Data --------------------------------------------------------
  
  for (i in seq_along(seas.raw)) {
    if (str_sub(seas.raw[[i]], 1, 5) == "Round") {
      # Start of a new round
      afl.round <- as.integer(str_sub(seas.raw[[i]], -2))
      
      if (str_sub(seas.raw[[i + 1]], 1, 8) == "Rnd Att:") {
        # Round already played
        fut.rnd <- 0
      } else {
        fut.rnd <- 1
      }
      
      games.num <- str_count(seas.raw[[i + 2 - fut.rnd]], paste0("-", seas))
      # print(str_c("Round: ", str_pad(afl.round, 2, side = "left"),
      #             " - number of games: ", games.num, sep = ""))
      
      # Initialise the round.games list
      round.games <- vector("list", 9)
      
      for (game in 1:games.num) {
        # For each game in the round (up to the point of a bye)
        game.offset <- 8 * (game - 1) - fut.rnd  # offset from the Round header in seas.raw
        
        if (seas.raw[[i + game.offset + 4]] == "Bye") {
          # Assume no more games
          print(str_c("Break at game ", game))
          break()
          
        } else {
          # Derive the match details
          match.det[[1]] <- afl.round
          match.det[[2]] <- game
          match.det[[3]] <- seas.raw[[i + game.offset + 3]]  #tm1
          match.det[[4]] <- seas.raw[[i + game.offset + 4]]  #tm1.score
          match.det[[5]] <- seas.raw[[i + game.offset + 7]]  #tm2
          match.det[[6]] <- seas.raw[[i + game.offset + 8]]  #tm2 score
          match.det[[7]] <- seas.raw[[i + game.offset + 6]]  #date, time etc
          
          round.games[[game]] <- match.det  # recursively add the matches to round.games list   
        }
        
        
      }
      
      afl.rounds[[afl.round]] <- bind_rows(round.games)
    }
  }
  
  season.games <- bind_rows(afl.rounds)
  
  games.played <- season.games %>%
    filter(str_length(tm1.score) > 1) %>%  # filter rows to include played games only
    mutate(venue = str_extract(date.time.att.venue, "(?<=(Venue: )).*$"),
           game.date = dmy(str_sub(date.time.att.venue, 5, 15))) %>%
    select(rnd:tm1, tm2, venue, game.date, tm1.score, tm2.score) %>%
    # Now separate the team score string into separate components, e.g. "4.3   7.4  12.4  14.5"
    separate(tm1.score, into = str_c("tm1.Q", 1:4),
             sep = c(6, 12, 18)) %>%
    separate(tm2.score, into = str_c("tm2.Q", 1:4),
             sep = c(6, 12, 18)) %>%
    select(rnd:game.date, ends_with("Q3"), ends_with("Q4")) %>%
    # The next few lines of code aim to separate each team's goals and behinds scores
    gather(tm.qtr, score.code, tm1.Q3:tm2.Q4) %>%
    mutate(score.code = str_trim(score.code)) %>%
    separate(score.code, into = c("G", "B"), convert = TRUE) %>%
    gather(score.type, score, G:B) %>%
    mutate(tm.q.sc.type = str_c(tm.qtr, ".", score.type)) %>%
    select(-tm.qtr, -score.type) %>%
    spread(key = tm.q.sc.type, value = score) %>%
    mutate(seas = seas)
  
  afl.season[[1]] <- games.played
  
  games.future <- season.games %>%
    filter(str_length(tm1.score) == 1) %>%  # filter rows to include non-played games only
    mutate(venue = str_extract(date.time.att.venue, "(?<=(Venue: )).*$"),
           game.date = dmy(str_sub(date.time.att.venue, 5, 15))) %>%
    select(rnd:tm1, tm2, venue, game.date) %>%
    mutate(seas = seas)
  
  afl.season[[2]] <- games.future
  
  afl.season
  
}

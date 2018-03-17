

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

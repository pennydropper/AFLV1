

library(shiny)

library(tidyverse)
library(lubridate)
library(readxl)


data <- "./data/"
code <- "./code/"

source("code/sh.src_quick.R")     # Load data required
source("code/sh.gen_funcs.R")     # Load functions used in the analysis

optim.by <- "tm.Q4.lead.abs"      # Set up to allow optimising by different measures


seas.sel <- 2017

# WILL NEED STEPS TO IMPORT RATINGS DATA etc


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AFL Teams' Ratings Review"),
   
   # Select team
   fluidRow(
     column(
       width = 2
     ),
     column(
       width = 5,
       selectInput("tm.sel",
                   "Select team to highlight",
                   tm.map$tm,
                   selected = "Melbourne")
     ),
     column(
       width = 4,
       selectInput("seas",
                   "Select season to chart",
                   2017:2013,
                   selected = seas.sel)
     )
     
   ),
   
   fluidRow(
     column(
       plotOutput("rtng.lines",
                  height = "600px"),
       width = 8
     ),
     
     column(
       plotOutput("rtng.ladder",
                  height = "600px"),
       width = 4
     )
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rtng.lines <- renderPlot({
    
    rtngs.plot.data <- calc.margin(afl.results) %>%
      select(seas:home, !!as.name(optim.by)) %>%
      filter(seas == input$seas) %>%
      left_join(full.ratings, by = c("seas", "rnd", "tm")) %>%
      rename(rating.tm = rating) %>%
      left_join(full.ratings, by = c("seas", "rnd", "opp" = "tm"), suffix = c(".tm", ".aw")) %>%
      rename(rating.opp = rating) %>%
      left_join(tm.map, by = c("opp" = "tm")) %>%
      mutate(opp.abbr = tm.abbr,
             lbl = paste0(opp.abbr, ": ", if_else(tm.Q4.lead.abs > 0, "+", ""), tm.Q4.lead.abs),
             tm.win = factor(sign(tm.Q4.lead.abs), levels = -1:1, labels = c("loss", "draw", "win")),
             op.win = factor(sign(-tm.Q4.lead.abs), levels = -1:1, labels = c("loss", "draw", "win"))) %>%
      left_join(tm.map, by = "tm")
    
    rtngs.plot.data %>%
      # filter(seas == input$seas) %>%
      ggplot(aes(x = rnd, y = rating.tm, group = tm)) +
      geom_hline(aes(yintercept = 1500), colour = "black", alpha = 0.5) +
      geom_line(aes(group = tm), colour = "grey", na.rm = TRUE) +
      geom_line(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                aes(y = rating.tm, group = tm), na.rm = TRUE, colour = "blue") +
      geom_point(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                 aes(y = rating.tm, fill = tm.win), na.rm = TRUE, shape = 21, colour = "blue", size = 2) +
      geom_point(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                 aes(y = rating.opp, fill = op.win), na.rm = TRUE, shape = 21, colour = "black", size = 2) +
      geom_text(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                aes(label = lbl, y = rating.opp),
                size = 5, angle = 90, colour = "black", hjust = "bottom",
                nudge_y = 10, alpha = 0.75) +
      ggrepel::geom_text_repel(data = rtngs.plot.data %>% filter(rnd %in% c(23)),
                               aes(label = tm.abbr.y, y = rating.tm), colour = "grey", nudge_x = 1, segment.color = "#737373") +
      ggrepel::geom_text_repel(data = rtngs.plot.data %>% filter(rnd %in% c(1)),
                               aes(label = tm.abbr.y, y = rating.tm), colour = "grey", nudge_x = -1, segment.color = "#737373") +
      labs(title = paste0(input$tm.sel, "'s rating through ", input$seas),
           x = "round", y = "rating at start of round") +
      scale_fill_manual("", values = c("win" = "blue", "loss" = "white", "draw" = "grey")) +
      theme(legend.position = "bottom") +
      scale_x_continuous(limits = c(1, 23), breaks = 1:23, minor_breaks = NULL) +
      scale_y_continuous(breaks = seq(1200, 1800, 100)) +
      coord_cartesian(ylim = c(1250, 1750))

    
  })
   
   output$rtng.ladder <- renderPlot({
     # Plot bar chart of final ratings

     full.ratings %>%
       filter(seas == input$seas,
              !is.na(rnd)) %>%
       filter(rnd == max(rnd)) %>%
       ggplot(aes(x = fct_reorder(tm, rating), y = rating, 
                  colour = tm, fill = tm)) +
       geom_col(show.legend = FALSE) +
       geom_hline(aes(yintercept = 1500), colour = "grey") +
       coord_flip(ylim = c(1250, 1750)) +
       labs(title = paste0("Ratings ladder after last round of season ", input$seas),
            x = "")     
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


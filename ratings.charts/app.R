

library(shiny)

seas.sel <- 2016

rtngs.plot.data <- calc.margin(afl.results) %>%
  select(seas:home, !!as.name(optim.by)) %>%
  filter(seas == seas.sel) %>%
  left_join(full.ratings, by = c("seas", "rnd", "tm")) %>%
  rename(rating.tm = rating) %>%
  left_join(full.ratings, by = c("seas", "rnd", "opp" = "tm"), suffix = c(".tm", ".aw")) %>%
  rename(rating.opp = rating) %>%
  left_join(tm.map, by = c("opp" = "tm")) %>%
  mutate(opp.abbr = tm.abbr,
         lbl = paste0(opp.abbr, ": ", if_else(tm.Q4.lead.abs > 0, "+", ""), tm.Q4.lead.abs))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AFL Ratings Review"),
   
   # Select team
   fluidRow(
     column(
       width = 2
     ),
     column(
       width = 11,
       selectInput("tm.sel",
                   "Select team to highlight",
                   tm.map$tm)
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
    
    rtngs.plot.data %>%
      ggplot(aes(x = rnd, y = rating.tm, colour = tm)) +
      geom_hline(aes(yintercept = 1500), colour = "black", alpha = 0.5) +
      geom_line(aes(group = tm), colour = "grey", na.rm = TRUE) +
      geom_line(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                aes(y = rating.tm), na.rm = TRUE, colour = "blue") +
      geom_point(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                 aes(y = rating.tm, colour = tm), na.rm = TRUE, colour = "blue") +
      geom_point(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                 aes(y = rating.opp), colour = "black", na.rm = TRUE) +
      geom_text(data = rtngs.plot.data %>% filter(tm == input$tm.sel),
                aes(label = lbl, y = rating.opp), 
                size = 5, angle = 90, colour = "black", hjust = "bottom",
                nudge_y = 10, alpha = 0.75) +
      labs(title = paste0(input$tm.sel, "'s rating through 2017"),
           x = "round", y = "rating at start of round")
    
  })
   
   output$rtng.ladder <- renderPlot({
     # Plot bar chart of final ratings

     full.ratings %>%
       filter(seas == seas.sel,
              !is.na(rnd)) %>%
       filter(rnd == max(rnd)) %>%
       ggplot(aes(x = fct_reorder(tm, rating), y = rating, 
                  colour = tm, fill = tm)) +
       geom_col(show.legend = FALSE) +
       geom_hline(aes(yintercept = 1500), colour = "grey") +
       coord_flip(ylim = c(1250, 1750)) +
       labs(title = paste0("Ratings ladder after last round of season ", seas.sel),
            x = "")     
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(shinythemes)

df = read.csv("./shiny_data/df_value.csv")

fluidPage(
  
  title = "Fantasy Football Draft Calculator",
  
  theme=shinytheme("superhero"),
  
  titlePanel(
    h3("Fantasy Football Draft Calculator", style="padding-left: 2vh;"), 
    ),
  
  titlePanel(
    h5("Sam Hughes", style="padding-left: 2vh;"), 
  ),
  
  sidebarPanel(
    
    selectInput(
      "mode", "Mode", c("Simple", "Advanced")
    ),
    
    sliderInput('teams', 'Teams', 
                min=1, max=32, value=12),    
    
    sliderInput('draft_order', 'Your Draft Order', 
                min=1, max=32, value=1),
    
    sliderInput('round', 'Current Round of Draft', 
                min=1, max=20, value=1),
    
    conditionalPanel(
      "input.mode == 'Advanced'", 
      sliderInput('qb', 'ADP of Best QB Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "QB", "rank_adp"])),
      
      sliderInput('rb', 'ADP of Best RB Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "RB", "rank_adp"])),
      
      sliderInput('wr', 'ADP of Best WR Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "WR", "rank_adp"])),
      
      sliderInput('te', 'ADP of Best TE Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "TE", "rank_adp"])),
      
      sliderInput('dst', 'ADP of Best DST Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "DST", "rank_adp"])),
      
      sliderInput('k', 'ADP of Best K Available', 
                  min=1, 
                  max=250, 
                  value=min(df[df$pos == "K", "rank_adp"]))
    )
    
  , style="height: 85vh; overflow-y: auto;"),
  
  mainPanel(
    textOutput('text'),
    tags$head(tags$style("#text{font-size: 30px;
                                text-align: center;
                                padding: 1vh}")),
    tableOutput('table'),
    fluidRow(
      splitLayout(cellWidths=c("49%", "2%", "49%"), plotOutput('plot1'), NULL, plotOutput('plot2'))
    )
  )
)
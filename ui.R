library(shiny)
shinyUI(fluidPage(
  titlePanel("MANY MONEY GAME"),
  
  titlePanel("Do you want play a game?"),
  sidebarLayout(
    sidebarPanel(
          selectInput("input_type", "Play What Game",
                      c("rock-paper-scissors","rock-paper-scissors(probability)", "dice Big-Small","dice Big-Small(probability)","Guess Number","Guess Number(probability)","bat-tiger-hen-bug","bat-tiger-hen-bug(probability)")
          ),
          uiOutput("ui")
        ),z
     

    mainPanel(
      tags$p("The results:"),
      verbatimTextOutput("input_type")
    )
    )
  )
)

# presentation
PUSIS 5th final presentation-lln
Example Shiny app
This is an example application for Shiny. The main purpose of this example is to illustrate how to run Shiny apps from a remote source. There are many ways to download and run it:

library(shiny)

# Easiest way is to use runGitHub
runGitHub("presentation", "rstudio")



# First clone the repository with git. If you have cloned it into
# ~/shiny_example, first go to that directory, then use runApp().
setwd("~/shiny_example")
runApp()
To run a Shiny app from a subdirectory in the repo or zip file, you can use the subdir argument. This repository happens to contain another copy of the app in inst/shinyapp/.

runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")

runUrl("https://github.com/rstudio/shiny_example/archive/master.tar.gz",
  subdir = "inst/shinyapp/")

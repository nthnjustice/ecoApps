library(shiny)
library(shinythemes)
library(shinydashboard)

deterministicGeometricModel <- function(x=20, y=0.1) {
  maxYear <- 40
  popSize <- c(x)
  currentPop <- x
  r <- y
  i <- 1

  for(i in 2:maxYear) {
    currentPop <- round(r*popSize[i-1])
    popSize <- c(popSize, currentPop+popSize[i-1])
    currentPop <- popSize[i]
  }

  return(popSize)
}

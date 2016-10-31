################################################################################
#     Run this script to download the dependcies the Lotka-Volterra
#       Predator-Prey Shiny App requires
#
#     By: Nathan Justice
#     Last edited: 24July2015
################################################################################

install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyAce")
install.packages("deSolve")
install.packages("cpm")
#install.packages("earlywarnings")
install.pakcages("moments")
install.packages("plotrix")

shiny::runApp("~/Desktop/EcoApps/lvpredpreyApp")

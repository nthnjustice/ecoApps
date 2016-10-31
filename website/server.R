################################################################################
################################################################################
############################## EcoApps #########################################
########################## By: Nathan Justice ##################################
#################### Last edited: 20September2015 ##############################
################################################################################
################################################################################

###### Shiny server ######

### start: server ###

shinyServer(function(input, output, session){
  output$lvpredatorpreyScreenshot <- renderImage({
    list(src="lvpredatorpreyScreenshot.png",
         contentType="image/png",
         width=597,
         height=447,
         alt="A screenshot of the Lotka-Volterra Predator-Prey application."
    )
  }, deleteFile=FALSE)

  output$pitcherplantScreenshot <- renderImage({
    list(src="pitcherplantScreenshot.png",
         contentType="image/png",
         width=597,
         height=447,
         alt="A screenshot of the Pitcher Plant application."
    )
  }, deleteFile=FALSE)

  output$githubScreenshot <- renderImage({
    list(src="githubScreenshot.png",
         contentType="image/png",
         width=597,
         height=447,
         alt="A screenshot of the EcoApps Github repository."
    )
  }, deleteFile=FALSE)

  output$hflogo <- renderImage({
    list(src="hflogo.jpg",
         contentType="image/jpg",
         alt="The Harvard Forest Logo."
    )
  }, deleteFile=FALSE)

  output$sidePanelScreenshot <- renderImage({
    list(src="sidePanelScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the sidebar panel."
    )
  }, deleteFile=FALSE)

  output$stateVariableToggle1 <- renderImage({
    list(src="stateVariableToggle1.png",
         contentType="image/png",
         alt="A screenshot of the state variable toggle buttons."
    )
  }, deleteFile=FALSE)

  output$stateVariableToggle2 <- renderImage({
    list(src="stateVariableToggle2.png",
         contentType="image/png",
         alt="A screenshot of the state variable toggle buttons."
    )
  }, deleteFile=FALSE)

  output$stateVariableToggle3 <- renderImage({
    list(src="stateVariableToggle3.png",
         contentType="image/png",
         alt="A screenshot of the state variable toggle buttons."
    )
  }, deleteFile=FALSE)

  output$dataTableTabScreenshot <- renderImage({
    list(src="dataTableTabScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the data table tab."
    )
  }, deleteFile=FALSE)

  output$modelTabScreenshot <- renderImage({
    list(src="modelTabScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the model tab."
    )
  }, deleteFile=FALSE)

  output$stateVariableSelectionScreenshot <- renderImage({
    list(src="stateVariableSelectionScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the state variable dropdown."
    )
  }, deleteFile=FALSE)

  output$decompositionSelectionScreenshot <- renderImage({
    list(src="decompositionSelectionScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the decomposition dropdown."
    )
  }, deleteFile=FALSE)

  output$drawnBreakpointLineScreenshot <- renderImage({
    list(src="drawnBreakpointLineScreenshot.png",
         contentType="image/png",
         alt="A screenshot of drawn breakpoint lines."
    )
  }, deleteFile=FALSE)

  output$EWSoverviewScreenshot <- renderImage({
    list(src="EWSoverviewScreenshot.png",
         contentType="image/png",
         alt="An overview of the statistical methods for the early warning signal analysis."
    )
  }, deleteFile=FALSE)

  output$drawnEWSLineScreenshot <- renderImage({
    list(src="drawnEWSLineScreenshot.png",
         contentType="image/png",
         alt="A screenshot of drawn early warning signal line."
    )
  }, deleteFile=FALSE)

  output$EWStableScreenshot <- renderImage({
    list(src="EWStableScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the EWS statistic table."
    )
  }, deleteFile=FALSE)

  output$RcodeTabScreenshot <- renderImage({
    list(src="RcodeTabScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the R Code tab."
    )
  }, deleteFile=FALSE)

  output$customizeGraphTabScreenshot <- renderImage({
    list(src="customizeGraphTabScreenshot.png",
         contentType="image/png",
         alt="A screenshot of the customize graph tab."
    )
  }, deleteFile=FALSE)

}) # end: shinyServer

### end: server ###

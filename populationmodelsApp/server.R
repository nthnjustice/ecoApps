source("global.R", local=TRUE)

shinyServer(function(input, output, session) {
  deterministicGeometric <- reactive({
    deterministicGeometricModel(input$initPop1, input$r1)
  })

  output$plotResults1 <- renderPlot({
    populations <- deterministicGeometric()
    maxPop <- max(populations)

    par(mar=c(5, 20, 5, 5))
    plot(1:40, populations, xlab="Year", ylab="", type="o", pch=15, col="blue", xlim=c(0, 45), ylim=c(0, maxPop+(maxPop/10)), yaxt="n")
    axis(2, at=pretty(populations), labels=format(pretty(populations), scientific=FALSE), las=1)
    mtext(text="Population Size", side=2, line=-1.5)
    grid()
  })

  output$tableResults1 <- renderDataTable({
    table <- cbind(timeIndex=seq(1, 40, by=1), deterministicGeometric())
    colnames(table) <- c("Year", "Population Size")
    return(table)
  }, options=list(pageLength=10))

})

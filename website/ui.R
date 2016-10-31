################################################################################
################################################################################
############################## EcoApps #########################################
########################## By: Nathan Justice ##################################
#################### Last edited: 20September2015 ##############################
################################################################################
################################################################################

##### Shiny user interface #####

# load dependencies
library(shinydashboard)
library(shiny)
library(shinythemes)

### start: interface ###

dashboardPage(skin="green",

  ## start: dashboardHeader ##

  dashboardHeader(title="EcoApps"),

  ## end: dashbaordHeader ##

  ## start: dashboardSidebar ##

  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName="About", icon=icon("home")),
      menuItem("Apps", tabName="Apps", icon=icon("line-chart")),
      menuItem("Tutorial", tabName="Tutorial", icon=icon("book")),
      menuItem("Source", tabName="Source", icon=icon("github")),
      menuItem("Contact", tabName="Contact", icon=icon("male"))
    ) # end: sidebarMenu
  ),

  ## end: dashboardSidebar

  ## start: dashboardBody ##

  dashboardBody(
    tabItems(
      tabItem(tabName="About",
        fluidRow(
          column(width=12,
            box(width=NULL,
              fluidRow(
                column(width=12, align="center",
                  a(imageOutput("hflogo"),
                    href="http://harvardforest.fas.harvard.edu/", target="_blank")
                ) # end: column
              ), # end: fluidRow
              h1(strong("EcoApps:")),
              h2(em("a tool for exploring ecosystem dynamics, tipping points,
                    and early warning signals")),
              br(),
              br(),
              h2(strong("Abstract")),
              h3("The goal of this project is to provide a computational tool
                 that will allow researchers, regardless of their familiarity
                 with formal programming languages, the ability to simulate
                 ecosystem dynamics and perform tipping point and early warning
                 signal analyses."),
              h3("Ostensibly stable ecosystems are sometimes susceptible to
                 abrupt and drastic changes from one state to another. These
                 rapid state changes, termed tipping points, represent a
                 threshold for a change in the ecosystem dynamics. The complexity
                 of ecosystem state changes is compounded by the knowledge
                 that alternative states are often not transient."),
              h3("Generally, humans and other wildlife are accustomed to a
                 preferred ecosystem state, and crossing a tipping point does not provide
                 sufficient opportunity for adaptation. Unexpected tipping
                 points are increasingly more likely to occur as human
                 impacts continue to affect and alter ecosystem dynamics
                 across the biosphere."),
              h3("Mounting ecological research suggests tipping points can
                be simulated with mathematical and statistical models. In
                addition, simulations of these models have the potential to
                illuminate early warning signals, which are probable indicators
                 that proceed an imminent tipping point."),
              h3("The product of this
                 project, EcoApps, is a series of web applications implemented
                 using the Shiny framework in R. Each application simulates a
                 distinct ecosystem model. Users are able to explore the model
                 by manipulating parameters and perform tipping point and early
                 warning signal analyses on the components (observed data,
                 trend, periodicity, and residuals) of the simulation.")
            ) # end: box
          ) # end: column
        ) # end: fluidRow
      ), # end: tabItem - Home
      tabItem(tabName="Apps",
        fluidRow(
          column(12, align="center",
            h1(strong("View the apps:"))
          )
        ), # end: fluidRow
        fluidRow(
          column(12, align="center",
            box(width=NULL,
              a(imageOutput("lvpredatorpreyScreenshot"),
                href="https://ecoapps.shinyapps.io/lvpredatorprey", target="_blank"),
              br(),
              br(),
              br(),
              a(h2("Click here or on the image to launch the Predator-Prey app through RStudio's Shiny server"),
                href="https://ecoapps.shinyapps.io/lvpredatorprey", target="_blank"),
              h4("or"),
              a(h3("Click here to launch the Predator-Prey app through Heroku"),
                href="https://ecoapps-lvpredatorprey.herokuapp.com/", target="_blank")
            ) # end: box
          ) # end: column
        ), # end: fluidRow
        fluidRow(
          column(12, align="center",
            box(width=NULL,
              a(imageOutput("pitcherplantScreenshot"),
                href="https://ecoapps.shinyapps.io/pitcherplant", target="_blank"),
              br(),
              br(),
              br(),
              a(h2("Click here or on the image to launch the Pitcher Plant app through RStudio's Shiny server"),
                href="https://ecoapps.shinyapps.io/pitcherplant", target="_blank"),
              h4("or"),
              a(h3("Click here to launch the Pitcher Plant app through Heroku"),
                href="http://ecoapps-pitcherplant.herokuapp.com/", target="_blank")
            ) # end: box
          ) # end: column
        ) # end: fluidRow
      ), #end: tabItem - Apps
      tabItem(tabName="Tutorial",
        box(width=NULL,
          h1(strong("Each app has three primary features:")),
          h2("1) Simulate a distinct ecosystem model"),
          h2("2) Perform a breakpoint (tipping point) analysis"),
          h2("3) Perform a early warning signal analysis"),
          hr(),
          hr(),
          hr(),
          h2(strong("Simulation")),
          h3("The main plot graphically displays the values computed by the ecosystem model."),
          h3("Use the input-boxes in the sidebar panel to adjust parameter values."),
          imageOutput("sidePanelScreenshot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          hr(),
          h3("Use the radio buttons below the plot to view individual state variables."),
          fluidRow(
            column(12, align="left",
              imageOutput("stateVariableToggle1"),
              br(),
              br(),
              br(),
              imageOutput("stateVariableToggle2"),
              br(),
              br(),
              imageOutput("stateVariableToggle3"),
              br(),
              br(),
              br(),
              hr()
            ) # end: column
          ), # end: fluidRow
          h3("The", em("Data Table"), "tab reports the actual values of the state variables."),
          h3("This table can be searched, sorted, and downloaded as a .csv file."),
          imageOutput("dataTableTabScreenshot"),
          br(),
          br(),
          hr(),
          h3("The mathematical and R implementations of the model can be reviewed under the",
             em("Model"), "tab."),
          hr(),
          hr(),
          hr(),
          h2(strong("Tipping Points")),
          h3("Once the desired parameter values have been selected, a tipping point analysis
             can be performed on a particular state variable."),
          h3("There are two methods to perform this analysis:"),
          h3(strong("1) The", em("Quick Analysis"), "subtab is useful for those who have
                    less familiarity and experience with ecological modeling and ecosystem dynamics.")),
          h4("This method is more abstract in that arguments are mostly obscured from the
             user and default values are used."),
          h3(strong("2) The", em("Advanced Analysis"), "is ideal for those who have a better
                    understanding of ecological modeling.")),
          h4("With this method, users have more control over argument values. The default
             values (which can be modified through this method) are the same used with the Quick Analysis."),
          h3("First choose the desired state variable for the tipping point analysis."),
          br(),
          imageOutput("stateVariableSelectionScreenshot"),
          h3("The frequency will be set to an arbitrarily determined (best fit) value, but
             this value can be modified."),
          h3("A decomposition method will run based on the state variable and frequency.
             This method returns the original (observed) data, the subtracted periodicity,
             the subtracted trend, and the residuals."),
          h3("Next, select a decomposed data set for analysis."),
          br(),
          imageOutput("decompositionSelectionScreenshot"),
          br(),
          br(),
          br(),
          br(),
          h3("The model can be changed to either a Gaussian sequence or an exponential distribution."),
          h3("The rollowing window size is relevant to the early warning signal analysis
             - the default value is typically appropriate and it does not affect the
             tipping point analysis."),
          h3("Once the desired argument values have been determined, click the", em("Run analysis"),
             "button to execute the method."),
          h3("Once the analysis completes, breakpoints (indicators of potential tipping points)
             will be displayed."),
          h3("Check the", em("Draw Breakpoint Lines"), "box to add the breakpoint lines to the
             main plot."),
          imageOutput("drawnBreakpointLineScreenshot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          hr(),
          hr(),
          hr(),
          h2(strong("Early Warning Signals")),
          h3("In addition to detecting potential tipping points, the application is able to run
             an early warning signals analysis on the model simulation."),
          h3("Again, there are two methods to perform this analysis:"),
          h3(strong("1) The", em("Quick Analysis"), "subtab is useful for those who have less
                    familiarity and experience with ecological modeling and ecosystem dynamics.")),
          h4("This method is more abstract in that arguments are mostly obscured from the user
             and default values are used."),
          h3(strong("2) The", em("Advanced Analysis"), "is ideal for those who have a better
                    understanding of ecological modeling.")),
          h4("With this method, users have more control over argument values. The default
             values (which can be modified through this method) are the same used with the Quick Analysis."),
          h3("After performing the tipping point analysis following the steps detailed in
             the Tipping Points section, the user may select a statistical method of interest
             as the early warning signal."),
          br(),
          imageOutput("EWSoverviewScreenshot"),
          h3("Clicking the", em("Download Early Warning Statistics"), "button will download
             the values for all of the statistical methods as a .csv file."),
          h3("Selecting one of the radio buttons will run the early warning signals analysis
             and plot the results for the corresponding method."),
          imageOutput("drawnEWSLineScreenshot"),
          h3("Checking the", em("Show Statistic Table"), "box will display the values
             computed by the desired statistical method."),
          imageOutput("EWStableScreenshot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          hr(),
          hr(),
          hr(),
          h2(strong("Additional Features")),
          h3("An example of the R script use to generate the simulation and perform the tipping
             point and early warning signals analyses can be downloaded from the", em("R Code"), "tab"),
          imageOutput("RcodeTabScreenshot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          h3("Labels and titles for the main plot can be customized under the", em("Simulation"),
             "tab and", em("Customize Graph"), "subtab."),
          imageOutput("customizeGraphTabScreenshot"),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
        ) # end: box
      ), # end: tabItem - Tutorial
      tabItem(tabName="Source",
        fluidRow(
          column(12, align="center",
            box(width=NULL,
              a(h1("Click to view the 'EcoApps' Github repository."),
                href="https://github.com/HarvardForest/ecoapps", target="_blank"),
              br(),
              a(imageOutput("githubScreenshot"),
                href="https://github.com/HarvardForest/ecoapps", target="_blank"),
              br(),
              br(),
              br()
            ) # end: box
          ) # end: column
        ) # end: fluidRow
      ), # end: tabItem - Source
      tabItem(tabName="Contact",
        box(width=NULL,
          h2("For support, please contact:"),
          a(h1("Matthew Lau"),  href="http://harvardforest.fas.harvard.edu/researchers/8438", target="_blank"))
      ) # end: tabItem - Contact
    ) # end: tabItems
  )

  ## end: dashboardBody ##

) # end: dashboardPage

### end: interface ###


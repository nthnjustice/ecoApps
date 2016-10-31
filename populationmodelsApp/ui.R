

dashboardPage(skin="purple",
  dashboardHeader(title="EcoApps: Population Models", titleWidth=300),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Deterministic Geometric Growth", tabName="DeterministicGeometric")
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName="DeterministicGeometric",
        fluidRow(
          column(width=12, align="center",
            h1(strong("Deterministic, Geometric Growth")),
            h2("n(t) = n(t-1) + dn(t-1)"),
            h2("dn(t-1) = r * n(t-1)")
          )
        ),
        fluidRow(
          column(width=12, align="left",
            h2(strong("Instructions:")),
            h3("Alter the r-value and initial population size. Loot at the effects on maximum population and rate of growth (pay attention to values on the y-axis)."),
            h2(strong("Major Points:")),
            h3("unrestricted growth, deterministic = same growth trajectory every time"),
            h2(strong("Question:")),
            h3("What is the relationship between r and the pattern of the population size curve?"),
            br(),
            br()
          )
        ),
        fluidRow(
          column(width=6, align="center",
            strong(numericInput("initPop1", label=h4(strong("Initial Population Size")), value=20, step=1))
          ),
          column(width=6, align="center",
            strong(numericInput("r1", label=h4(strong("r-value")), value=0.1, step=0.1))
          )
        ),
        fluidRow(
          column(width=12, align="center",
            plotOutput("plotResults1"),
            br(),
            br()
          )
        ),
        fluidRow(
          column(width=10, offset=1, align="center",
            h3(dataTableOutput("tableResults1"))
          )
        )
      )
    )
  )
)




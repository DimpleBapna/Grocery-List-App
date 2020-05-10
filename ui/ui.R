dashboardSidebar <- dashboardSidebar(
  width = 320,
  selectizeInput(
    inputId = "recipe",
    label = "Recipe",
    choices = recipe_data$title,
    selected = NULL,
    multiple = FALSE,
    options = list(
      placeholder = 'Type to search...',
      onInitialize = I('function() { this.setValue(""); }')
    )
  ),
  conditionalPanel('input.recipe != ""',
                   uiOutput('quantity'),
                   uiOutput("add"))
)


dashboardBody <- dashboardBody(
  theme_blue_gradient,
  tags$head(tags$style(HTML('{margin:5px;}'))),

  fluidPage(
    fluidRow(width = 5,
             valueBoxOutput('calories'),
             valueBoxOutput('Protein'),
             valueBoxOutput('Sodium'),
             valueBoxOutput('Fat'),
             valueBoxOutput('Saturated_Fat'),
             valueBoxOutput('Sugar')
    ),tags$hr(),
    fluidRow(
      column(width = 6, uiOutput("RecipeListUI")),
      column(width = 6, plotly::plotlyOutput('bubble_chart'))
    ),tags$hr(),
    fluidRow(
      column(width =6, fluidRow(uiOutput("groceryListUI"))),
      column(width =6, plotly::plotlyOutput("centralPlot", height = "450px", width = "450px"))
    ),tags$hr(),
    fluidRow(
      uiOutput('pie_chart_choices')
    ),
    fluidRow(
      width = 10,
      plotly::plotlyOutput('pie_chart', width = "1000px"),
    )
  )
)

rightSideBar <- rightSidebar(
  background = "dark",
  width = 350,
  rightSidebarTabContent(
    id = 1,
    title = "Go to URL",
    icon = "desktop",
    active = TRUE,
    uiOutput("instruction_URL"),
    uiOutput("url")
  ),
  rightSidebarTabContent(
    id = 2,
    title = "Recommendation",
    uiOutput("MachineLearningUI")
  ),
  rightSidebarTabContent(
    id = 3,
    icon = "paint-brush",
    title = "Instructions",
    uiOutput("instructionUI"),
    uiOutput("instructionSteps")
  )
  #,
  #   rightSidebarTabContent(
  #     id = 4,
  #     icon = "paint-brush",
  #     title = "Dev Info",
  #     uiOutput("Developer")
  # )
)

# define UI logic
ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    title = "Rapid Groceries", titleWidth = 320),
  dashboardSidebar,
  dashboardBody,
  rightSideBar
)


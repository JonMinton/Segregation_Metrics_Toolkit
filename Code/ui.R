require(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Computing the dissimilarity index for simulated data"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    h3(textOutput("sidebartitle")), 
    
    helpText("This App simulates data based on the conditions specified below, and 
             compares the true dissimilarity index to estimates obtained using
             two different methods. The first estimation method uses the classical formula for the 
             dissimilarity index and computes a confidence interval using a data 
             re-sampling bootstrap. The second is a Bayesian spatial model, which computes
             the posterior predictive median and credible interval for the index D"),
    
    br(),

    sliderInput("n.area", 
                "Number of areal units:", 
                 value = 500,
                 min = 50, 
                 max = 1000),
    
    sliderInput("n.population", 
                "Population size in each areal unit:", 
                value = 100,
                min = 100, 
                max = 10000),

    sliderInput("sd", 
                "Level of variation in the minority proportion", 
                value = 5,
                min = 0.01, 
                max = 10),

    sliderInput("mean", 
                "Average value of the minority proportion", 
                value = 0.2,
                min = 0.01, 
                max = 0.5),
    
    br(),
    
    actionButton("run", "Update")
    #submitButton("Run")
    ),

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
     br(),
     h3(textOutput("plottitle")), 
     plotOutput("plot"), 
     br(),
     br(),
     br(),
     verbatimTextOutput("summary1"),
     br(),
     h3(textOutput("summarytitle")), 
     verbatimTextOutput("summary2") 
    )

))


runApp()

#### Examples of sidebarPanel() inputs in ui.R. These are what the user inputs.
selectInput("variable", "Variable:",
            list("Cylinders" = "cyl", 
                 "Transmission" = "am", 
                 "Gears" = "gear"))


selectInput("dataset", "Choose a dataset:", 
            choices = c("rock", "pressure", "cars"))

textInput("caption", "Caption:", "Data Summary")

checkboxInput("outliers", "Show outliers", FALSE)

sliderInput("obs", 
            "Number of observations:", 
            min = 1,
            max = 1000, 
            value = 500)

numericInput("obs", "Number of observations to view:", 10)


#### Examples of mainPanel() outputs in ui.R. These are the results that are displayed.
verbatimTextOutput("summary")

tableOutput("view")

plotOutput("distPlot")

h3(textOutput("caption")), 



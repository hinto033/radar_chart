# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals"),
      selectInput("race", 
                  label = "Choose a race of interest",
                  choices = c("Non-Hispanic White", "Non-Hispanic Black",
                              "Mex American/Other Hispanic"),
                  selected = "Non-Hispanic White"),
      selectInput("gender", 
                  label = "Choose a gender of interest",
                  choices = c("Male", "Female"),
                  selected = "Male"),
      sliderInput("totFmi", label = "Total FMI" ,
                  min = 0, max = 25, value = 10, step = 0.1),
      sliderInput("trunkFmi", label = "Trunk FMI" ,
                  min = 0, max = 14, value = 7, step = 0.1),
      sliderInput("armFmi", label = "Arm FMI" ,
                  min = 0, max = 2, value = 1, step = 0.01),
      sliderInput("legFmi", label = "Leg FMI" ,
                  min = 0, max = 5, value = 2, step = 0.01)
      #Later: options for a certain age range, or options for certian z scores
      ),
    mainPanel(
      p("Total Prev: Under 16, Under 17, Under 18.5, Over 25, over 30, over 35, over 40"),
      br(),
      textOutput("text1"),
      br(),
      textOutput("text2"),
      br(),
      textOutput("text3"),
      br(),
      textOutput("text4"),
      br(),
      textOutput("text5")
      )
  )
))
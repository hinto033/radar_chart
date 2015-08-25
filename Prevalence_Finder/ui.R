# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals"),
      selectInput("race", 
                  label = "Choose a race of interest",
                  choices = c("Non-Hispanic White", "Non-Hispanic Black",
                              "Hispanic", "Alien"),
                  selected = "Non-Hispanic White"),
      selectInput("gender", 
                  label = "Choose a gender of interest",
                  choices = c("Male", "Female"),
                  selected = "Male"),
      sliderInput("totFmi", label = "Total FMI" ,
                  min = 0, max = 25, value = 10, step = 0.1),
      sliderInput("trunkFmi", label = "Trunk FMI" ,
                  min = 0, max = 15, value = 7, step = 0.1),
      sliderInput("armFmi", label = "Arm FMI" ,
                  min = 0, max = 4, value = 2, step = 0.1),
      sliderInput("legFmi", label = "Leg FMI" ,
                  min = 0, max = 6, value = 2, step = 0.1)
      #Later: options for a certain age range, or options for certian z scores
      ),
    mainPanel(
      textOutput("text1"),
      textOutput("text2")
      )
  )
))
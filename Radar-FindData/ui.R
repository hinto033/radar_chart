# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals"),
      
      #Need a way to check which variables I want to restrict. 
      
      
      
      selectInput("race", 
                  label = "Choose a race of interest",
                  choices = c("White", "Black",
                              "Hispanic", "ANY"),
                  selected = "White"),
      
      selectInput("gender", 
                  label = "Choose a gender of interest",
                  choices = c("Male", "Female", "ANY"),
                  selected = "Male"),
      
      #maybe for age range later
      #sliderInput("range", 
      #            label = "Age Range of interest:",
      #            min = 0, max = 100, value = c(0, 100)),
      
      
      sliderInput("age", label = "Age of Interest" ,
                  min = 0, max = 100, value = 50),
      
      sliderInput("BMI", label = "Goal BMI",
                  min = 0, max = 10),
      
      sliderInput("LMI", label = "Goal LMI"),
      
      sliderInput("FMI", label = "Goal FMI"),
      
      sliderInput("TrunkZFMI", label = "Trunk Z FMI")
      
      #Later: options for a certain age range, or options for certian z scores
    ),
    
    mainPanel(
      plotOutput("map"),
      br(),
      textOutput("text1"),
      br(),
      textOutput("text2"))
  )
))
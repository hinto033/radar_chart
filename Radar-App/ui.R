# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals"),
      
      selectInput("race", 
                  label = "Choose a race of interest",
                  choices = c("White", "Black",
                              "Hispanic"),
                  selected = "White"),
      
      selectInput("gender", 
                  label = "Choose a gender of interest",
                  choices = c("Male", "Female"),
                  selected = "Male"),
      
      #maybe for age range later
      #sliderInput("range", 
      #            label = "Age Range of interest:",
      #            min = 0, max = 100, value = c(0, 100)),
      
      
      sliderInput("age", label = "Age of Interest" ,
                  min = 0, max = 100, value = 50)
      
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
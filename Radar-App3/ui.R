# ui.R
#Should produce radar charts of people ina range of FMI LMI Values


shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals"),
      
#       selectInput("race", 
#                   label = "Choose a race of interest",
#                   choices = c("Non-Hispanic White", "Non-Hispanic Black",
#                               "Hispanic", "Alien"),
#                   selected = "Non-Hispanic White"),
#       
#       selectInput("gender", 
#                   label = "Choose a gender of interest",
#                   choices = c("Male", "Female"),
#                   selected = "Male"),
      
      #maybe for age range later
      #sliderInput("range", 
      #            label = "Age Range of interest:",
      #            min = 0, max = 100, value = c(0, 100)),
      
      
      sliderInput("BMIMIN", label = "Minimum BMI" ,
                  min = 0, max = 50, value = 19,step = 0.1),
      
      sliderInput("FMIMIN", label = "Minimum ZFMI" ,
                  min = -3, max = 3, value = 0,step = 0.1),
      
      sliderInput("LMIMIN", label = "Minimum ZLMI" ,
                  min = -3, max = 3, value = 0,step = 0.1),

      sliderInput("BMIMAX", label = "Max BMI" ,
                  min = 0, max = 50, value = 20,step = 0.1),
      
      sliderInput("FMIMAX", label = "Max ZFMI" ,
                  min = -3, max = 3, value = 1,step = 0.1),
      
      sliderInput("LMIMAX", label = "Max ZLMI" ,
                  min = -3, max = 3, value = 1,step = 0.1)
      
      #Later: options for a certain age range, or options for certian z scores
      ),
    
mainPanel(plotOutput("map"),
          br(),
          textOutput("text1")
  ))
))
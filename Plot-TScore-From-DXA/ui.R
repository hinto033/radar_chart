# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals from Fit3d Scans"),
      

      #maybe for age range later
      #sliderInput("range", 
      #            label = "Age Range of interest:",
      #            min = 0, max = 100, value = c(0, 100)),
      
      
      sliderInput("Person", label = "Person of Interest" ,
                  min = 0, max = 30, value = 10)
      
      #Later: options for a certain age range, or options for certian z scores
      ),
    
    mainPanel(plotOutput("map"),
              br(),
              textOutput("text1"),
              br(),
              textOutput("text2"),
              br(),
              textOutput("text3"),
              br()
)
  )
))
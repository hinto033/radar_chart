# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create radar charts of  individuals from Fit3d Scans"),
      

          numericInput("AgeYr", 
                    label = h3("Enter Age"), 
                    value = 20) , 
          
          numericInput("height_cm", 
                   label = h3("Enter Height in cm"), 
                   value = 180)   ,
          
          
          
          selectInput("Race", label = h3("Select Race"), 
                 choices = list("Hispanic" = 1, "Non-Hispanic White" = 2,
                                "Non-Hispanic Black" = 3), selected = 1),
          
          
          selectInput("Gender", label = h3("Gender"), 
                 choices = list("Female" = 1, "Male" = 2), selected = 1),
          
          
          numericInput("RARM_FAT", 
                   label = h3("Enter Right Arm Fat Mass in Grams"), 
                   value = 1500),   
          numericInput("RARM_LEAN", 
                   label = h3("Enter Right Arm Lean Mass in Grams"), 
                   value = 4000),   
          numericInput("LARM_FAT", 
                 label = h3("Enter Left Arm Fat Mass in Grams"), 
                 value = 1500),   
          numericInput("LARM_LEAN", 
                 label = h3("Enter Left Arm Lean Mass in Grams"), 
                 value = 4000),   
          numericInput("L_LEG_FAT", 
               label = h3("Enter Left Leg Fat Mass in Grams"), 
               value = 4000),   
          numericInput("L_LEG_LEAN", 
               label = h3("Enter Left Leg Lean Mass in Grams"), 
               value = 10000),  
          numericInput("R_LEG_FAT", 
               label = h3("Enter Right Leg Fat Mass in Grams"), 
               value = 4000),   
          numericInput("R_LEG_LEAN", 
               label = h3("Enter Right Leg Lean Mass in Grams"), 
               value = 10000),   
          numericInput("TRUNK_FAT", 
               label = h3("Enter Trunk Fat Mass in Grams"), 
               value = 10000),   
          numericInput("TRUNK_LEAN", 
               label = h3("Enter Trunk Lean Mass in Grams"), 
               value = 25000)   
          
      ),#Sidebar panel
            
      #Later: options for a certain age range, or options for certian z scores
     # ),
    
          mainPanel(plotOutput("map"),
                    br(),
                    textOutput("text1"),
                    br()

    )
    )#Sidebar Layout


))#Shiny ui and fluid page

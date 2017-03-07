# ui.R

#http://rmarkdown.rstudio.com/authoring_shiny_widgets.html

shinyUI(fluidPage(

  #First row selections
  fluidRow(style = "padding-bottom: 5px;",
          column(3,selectInput("Race", label = h6("Select Race"),
                               choices = list("Hispanic" = 1, "Non-Hispanic White" = 2, "Non-Hispanic Black" = 3),
                               selected = 1)),
          column(3,selectInput("Gender", label = h6("Select Gender"),
                               choices = list("Female" = 1, "Male" = 2), selected = 1))),
  #Second Row Selections
  fluidRow(style = "padding-bottom: 5px;",
           column(3,numericInput("AgeYr", label = h6("Enter Age (8-85)"),value=20)),
           column(3,numericInput("height_cm", label = h6("Height (cm)"),value=160)),
           column(3,numericInput("TRUNK_FAT", label = h6("Trunk Fat Mass (kg)"),value = 12.0, step = 0.1)),
           column(3,numericInput("TRUNK_LEAN", label = h6("Trunk Lean Mass (kg)"),value = 21.0, step = 0.1)) ),
  #ThirdRow Selections
  fluidRow(style = "padding-bottom: 5px;",
          column(3,numericInput("RARM_FAT", label = h6("Right Arm Fat Mass (kg)"),value = 1.4, step = 0.1)),
          column(3,numericInput("RARM_LEAN", label = h6("Right Arm Lean Mass (kg)"),value = 2.2, step = 0.1)),
          column(3,numericInput("LARM_FAT", label = h6("Left Arm Fat Mass (kg)"),value = 1.4, step = 0.1)),
          column(3,numericInput("LARM_LEAN", label = h6("Left Arm Lean Mass (kg)"),value = 2.2, step = 0.1)) ),
  #FourthRowSelctions
  fluidRow(style = "padding-bottom: 5px;",
          column(3,numericInput("R_LEG_FAT", label = h6("Right leg Fat Mass (kg)"),value = 5.0, step = 0.1)),
          column(3,numericInput("R_LEG_LEAN", label = h6("Right leg Lean Mass (kg)"),value = 7.0, step = 0.1)),
          column(3,numericInput("L_LEG_FAT", label = h6("Left leg Fat Mass (kg)"),value = 5.0, step = 0.1)),
          column(3,numericInput("L_LEG_LEAN", label = h6("Left leg Lean Mass (kg)"),value = 7.0, step = 0.1)) ),
  #Fifth row contains save button and the radar chart map.
  fluidRow( style = "padding-top: 50px;", style = "padding-left: 50px",
          column(4,actionButton("saveAction", label = "click to save output"),
                 br(),
                 textInput("Filename", label = h5("Enter Desired Filename Below: "), value = "MyPlotName") ),
          column(4,plotOutput("map")) )
))#End of shiny and fluidpage


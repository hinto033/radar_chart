# ui.R

#http://rmarkdown.rstudio.com/authoring_shiny_widgets.html



shinyUI(fluidPage(

      tags$head(
        tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", "textarea { max-width: 185px;  }"),
        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        tags$style(type='text/css', ".well { max-width: 310px; }"),
        tags$style(type='text/css', ".span4 { max-width: 310px; }")
      ),
  fluidRow(style = "padding-bottom: 5px;",
          column(3,selectInput("Race", label = h6("Select Race"),
                               choices = list("Hispanic" = 1, "Non-Hispanic White" = 2, "Non-Hispanic Black" = 3),
                               selected = 1)),
          column(3,selectInput("Gender", label = h6("Select Gender"),
                               choices = list("Female" = 1, "Male" = 2), selected = 1))),
  fluidRow(style = "padding-bottom: 5px;",
           column(3,numericInput("AgeYr", label = h6("Enter Age (8-85)"),value=20)),
           column(3,numericInput("height_cm", label = h6("Height in cm"),value=160)),
           column(3,numericInput("TRUNK_FAT", label = h6("Trunk Fat Mass (kg)"),value = 12.0, step = 0.1)),
           column(3,numericInput("TRUNK_LEAN", label = h6("Trunk Lean Mass (kg)"),value = 21.0, step = 0.1)) ),
  fluidRow(style = "padding-bottom: 5px;",
          column(3,numericInput("RARM_FAT", label = h6("Right Arm Fat Mass (kg)"),value = 1.4, step = 0.1)),
          column(3,numericInput("RARM_LEAN", label = h6("Right Arm Lean Mass (kg)"),value = 2.2, step = 0.1)),
          column(3,numericInput("LARM_FAT", label = h6("Left Arm Fat Mass (kg)"),value = 1.4, step = 0.1)),
          column(3,numericInput("LARM_LEAN", label = h6("Left Arm Lean Mass (kg)"),value = 2.2, step = 0.1)) ),
  fluidRow(style = "padding-bottom: 5px;",
          column(3,numericInput("R_LEG_FAT", label = h6("Right leg Fat Mass (kg)"),value = 5.0, step = 0.1)),
          column(3,numericInput("R_LEG_LEAN", label = h6("Right leg Lean Mass (kg)"),value = 7.0, step = 0.1)),
          column(3,numericInput("L_LEG_FAT", label = h6("Left leg Fat Mass (kg)"),value = 5.0, step = 0.1)),
          column(3,numericInput("L_LEG_LEAN", label = h6("Left leg Lean Mass (kg)"),value = 7.0, step = 0.1)) ),


  
  fluidRow( 
    column(4,actionButton("saveAction", label = "click to save output")),
    column(4,plotOutput("map"))
    )
))


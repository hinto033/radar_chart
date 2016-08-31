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
          column(2,selectInput("Race", label = h6("Select Race"),
                               choices = list("Hispanic" = 1, "Non-Hispanic White" = 2, "Non-Hispanic Black" = 3),
                               selected = 1)),
          column(2,selectInput("Gender", label = h6("Select Gender"),
                               choices = list("Female" = 1, "Male" = 2), selected = 1)),
          column(2,numericInput("AgeYr", label = h6("Enter Age"),value=20)),
          column(2,numericInput("height_cm", label = h6("Enter height in cm"),value=180)),
          column(2,numericInput("TRUNK_FAT", label = h6("Enter Trunk Fat Mass in Grams"),value = 10000)),
          column(2,numericInput("TRUNK_LEAN", label = h6("Enter Trunk Lean Mass in Grams"),value = 25000)) ),
  fluidRow(style = "padding-bottom: 5px;",
          column(2,numericInput("RARM_FAT", label = h6("Enter Right Arm Fat Mass in Grams"),value = 1500)),
          column(2,numericInput("RARM_LEAN", label = h6("Enter Right Arm Lean Mass in Grams"),value = 4000)),
          column(2,numericInput("LARM_FAT", label = h6("Enter Left Arm Fat Mass in Grams"),value = 1500)),
          column(2,numericInput("LARM_LEAN", label = h6("Enter Left Arm Lean Mass in Grams"),value = 4000)),
          column(2,numericInput("R_LEG_FAT", label = h6("Enter Right leg Fat Mass in Grams"),value = 4000)),
          column(2,numericInput("R_LEG_LEAN", label = h6("Enter Right leg Lean Mass in Grams"),value = 10000)),
          column(2,numericInput("L_LEG_FAT", label = h6("Enter Left leg Fat Mass in Grams"),value = 4000)),
          column(2,numericInput("L_LEG_LEAN", label = h6("Enter Left leg Lean Mass in Grams"),value = 10000)) ),


  
  fluidRow( plotOutput("map"))
))
  # 
  # titlePanel("Radar Chart Generator"),
  # 
  # sidebarLayout(
  #   sidebarPanel(

  #     helpText("Create radar charts of  individuals from DXA Data"),
  #         numericInput("AgeYr", 
  #                label = h6("Enter Age"), 
  #                value = 20) , 
  #     tags$style(type="text/css", "input.shiny-bound-input { font-size:12px; height:20px;}"),
  #     tags$style(type="text/css", "select.shiny-bound-input { font-size:12px; height:20px;}"),
  #         numericInput("height_cm", 
  #                label = h6("Enter Height in cm"), 
  #                value = 180)   ,
  #         selectInput("Race", label = h6("Select Race"), 
  #                choices = list("Hispanic" = 1, "Non-Hispanic White" = 2,
  #                               "Non-Hispanic Black" = 3), selected = 1),
  #         selectInput("Gender", label = h6("Gender"), 
  #                choices = list("Female" = 1, "Male" = 2), selected = 1),
  #         numericInput("RARM_FAT", 
  #                label = h6("Enter Right Arm Fat Mass in Grams"), 
  #                value = 1500),   
  #         numericInput("RARM_LEAN", 
  #                label = h6("Enter Right Arm Lean Mass in Grams"), 
  #                value = 4000),   
  #         numericInput("LARM_FAT", 
  #                label = h6("Enter Left Arm Fat Mass in Grams"), 
  #                value = 1500),   
  #         numericInput("LARM_LEAN", 
  #                label = h6("Enter Left Arm Lean Mass in Grams"), 
  #                value = 4000),   
  #         numericInput("L_LEG_FAT", 
  #                label = h6("Enter Left Leg Fat Mass in Grams"), 
  #                value = 4000),   
  #         numericInput("L_LEG_LEAN", 
  #                label = h6("Enter Left Leg Lean Mass in Grams"), 
  #                value = 10000),  
  #         numericInput("R_LEG_FAT", 
  #                label = h6("Enter Right Leg Fat Mass in Grams"), 
  #                value = 4000),   
  #         numericInput("R_LEG_LEAN", 
  #                label = h6("Enter Right Leg Lean Mass in Grams"), 
  #                value = 10000),   
  #         numericInput("TRUNK_FAT", 
  #                label = h6("Enter Trunk Fat Mass in Grams"), 
  #                value = 10000),   
  #         numericInput("TRUNK_LEAN", 
  #                label = h6("Enter Trunk Lean Mass in Grams"), 
  #                value = 25000)   
  #         
    #   ),#Sidebar panel
    #         
    #   #Later: options for a certain age range, or options for certian z scores
    #  # ),
    # 
    #       mainPanel(plotOutput("map"),
    #                 br(),
    #                 textOutput("text1"),
    #                 br()
    # 
    # )
    # )#Sidebar Layout


# ))#Shiny ui and fluid page

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("My Shiny App"),
  
  sidebarLayout(
    sidebarPanel( 
      h1("Installation"),
      
      p("Shiny is available in CRAN so you can install it the usual way from your R console"),
      br(),
      br(),
      code("install.package('shiny')"),
      br(),
      br(),
      img(src = "bigorb.png", height = 72, width = 72),
      "shiny is a product of ", 
      span("RStudio", style = "color:blue")
      
      ),
    mainPanel(
      h1("Main Panel"),
      p("Shiny is a new Package in rStudio that makes it ",
      em("incredibly"),
      "Easy to build interactive web applications in R"),
      br(),
      p("For an Introduction and live examples, visit the ",
      a("Shiny Website", href = "http://www.rstudio.com/shiny")),
      
      
      h1("Features"),
      p("* Build useful web applications with only a few lines of code - no JS required."),
      p("*Shiny applications are automatically live in the same way that",
      strong("Spreadsheets"),
      "are live.Outputs change instnatly as users modify inputs,
        without reloading of the browser")
    
         )
  )
  
  
))
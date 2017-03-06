library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(navbarPage("Coursera Data Science Capstone",
                   theme = shinytheme("cerulean"),
                   tabPanel("Predict Next Word",
                            fluidRow(column(3),
                                     column(6,
                                            tags$div(textInput("inputString",
                                                               label = h3("Enter text here:"),
                                                               value = ""),
                                                     tags$span(style = "color:grey", ("Note: Only English words are predicted")),
                                                     br(),
                                                     tags$hr(),
                                                     h4("Top 5 Predicted Next Words:"),
                                                     br(),
                                                     tags$span(style = "color:darkred", tags$strong(tags$h5(textOutput("predictedWord1")))),
                                                     br(),
                                                     tags$span(style = "color:darkred", tags$strong(tags$h5(textOutput("predictedWord2")))),
                                                     br(),
                                                     tags$span(style = "color:darkred", tags$strong(tags$h5(textOutput("predictedWord3")))),
                                                     br(),
                                                     tags$span(style = "color:darkred", tags$strong(tags$h5(textOutput("predictedWord4")))),
                                                     br(),
                                                     tags$span(style = "color:darkred", tags$strong(tags$h5(textOutput("predictedWord5")))),
                                                     tags$hr(),
                                                     align = "center"
                                            )
                                     ),
                                     column(3)
                            )
                   ),
                   tabPanel("About",
                            fluidRow(column(2, p("")),
                                     column(8, includeMarkdown("www/about.md")),
                                     column(2, p(""))
                            )
                   ),
                   tags$hr(),
                   tags$br(),
                   tags$span(style = "color:grey",
                             tags$footer(img(src = "./organizations.png"),
                                         tags$br(),
                                         tags$br(),
                                         ("Prepared by"), 
                                         tags$a(href = "https://www.linkedin.com/in/wong-zhen-yao-9260b257/",
                                                target = "_blank",
                                                "Zhen Yao, Wong"),
                                         tags$br(),
                                         ("March, 2017"),
                                         tags$br(),
                                         ("Build with"),
                                         tags$a(href = "http://www.r-project.org/",
                                                target = "_blank",
                                                "R"),
                                         ("&"),
                                         tags$a(href = "http://shiny.rstudio.com",
                                                target = "_blank",
                                                "Shiny"),
                                         align = "center"
                             )
                   )
)
)

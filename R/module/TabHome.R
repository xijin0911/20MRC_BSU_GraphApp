tabHome <- tabPanel("Home", icon=icon("home", lib = "glyphicon"),
                      fluidRow(
                        column(12,
                               includeHTML("www/home_title.html")
                        ),
                        br(),br(),
                        column(9,
                          includeHTML("www/home.html")
                        ),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        column(3,style = "background-color: skyblue;",
                               h3("Learning Materials"),
                               downloadButton("downloadData", label = "Slides"),
                               p("Please download what you need here.")
                        ),
                        br(),br()
                      )
                    )
                    
                    
                    
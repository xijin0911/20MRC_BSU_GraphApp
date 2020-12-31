
tabHome <- tabPanel("Home", icon=icon("home", lib = "glyphicon"),
                      fluidRow(
                        column(12,includeHTML("www/home_title.html")),
                        br(),br(),
                        column(10,includeHTML("www/home.html")),
                        # useShinyjs(),
                        # extendShinyjs(text = "shinyjs.toTop = function() {document.body.scrollTop = 0;}"),
                        # actionButton("toTop", "jump to top"),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        column(2,style = "background-color: skyblue;",
                               h5("Lecture Slides"),
                               downloadButton("downloadData", label = "Slides"),
                               
                               br(),br()),
                        br(),br()
                      ))
                    
                    
                    
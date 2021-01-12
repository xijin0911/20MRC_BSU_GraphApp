
tabHome <- tabPanel("Home", icon=icon("home", lib = "glyphicon"),
                      fluidRow(
                        column(12,includeHTML("www/home_title.html")),
                        br(),br(),
                        column(8,includeHTML("www/home.html")),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        br(),br(),
                        column(4,style = "background-color: skyblue;",
                               h5("Learning Materials"),
                               p("The slides for the course."),
                               downloadButton("downloadSlide", label = "Slides"),
                               br(),br(),
                               h5("Example video (Draw)"),
                               HTML("You can directly by adding nodes and edges in the Draw panel to obtaion the result"),
                               tags$video(id="video1", type = "video/mp4",width="350", height="240",
                                          src = "Draw_Ex.mp4", controls = "controls"),
                               br(),br(),
                               h5("Example video (Example)"),
                               HTML("You can simply by chosing the <em>Bonferroni-Holm Test</em> option in the <em>Common procedures</em> part in the <em>Example</em> panel"),
                               tags$video(id="video1", type = "video/mp4",width="350", height="240",
                                          src = "Example_Ex.mp4", controls = "controls"),
                               br(),br(),
                               h5("The R markdown File"),
                               p("You can run the core function directly. The document could be compiled into different forms of file."),
                               downloadButton("downloadRMD", label = "Document"),
                               br(),br(),
                               
                               br(),br()),
                        column(12,includeHTML("www/reference.html")),
                        br(),br()
                      ))
                    
                    
                    
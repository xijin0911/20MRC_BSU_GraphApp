
tabHome <- tabPanel("Home", icon=icon("home", lib = "glyphicon"),
                    fluidRow(
                      column(12,includeHTML("www/home_title.html")),
                      br(),br(),
                      column(8,
                             includeHTML("www/home_welcome.html"),
                            includeHTML("www/home_method.html")),
                      br(),br(),
                      br(),br(),
                      br(),
                      br(),br(),
                      br(),br(),
                      column(4,style = "background-color: skyblue;",
                             includeHTML("www/home_howto.html"),
                             br(),br(),
                             # h5("An example for the Bonferroni-Holm procedure"),
                             # tags$img(src = "procedure_ordered.gif",align = "center",
                                      # width='200', height="100"),
                             h5("Learning Materials"),
                             p("The slides is about Advanced Multiplicity Correction"),
                             downloadButton("downloadSlide", label = "Slides"),
                                      # width = "100px", height = "100px")
                             # h5("R markdown File"),
                             # HTML("To implement the graphical approach, you can use the <em>Draw </em>/<em> Examples</em> tab, or download the R markdown file to use in R."),
                             # HTML("To implement the graphical approach, you can run the core function <code>graph_app</code> directly. The rejection results and adjusted <i>p</i>-values are avaliable, 
                             #       but there is no graph visualization."),
                             # br(),br(),
                             # downloadButton("downloadRMD", label = "Document"),
                             # 
                             br(),br(),
                             br(),br()),
                      column(12,includeHTML("www/reference.html")),
                      br(),br()))
                    
                    
                    
tabTest <- tabPanel("Specific tests",
                    # icon=icon("picture", lib = "glyphicon"),  
         column(3,style = "background-color: skyblue;",
                h3("Examples", align = "center"),
                numericInput(inputId = "alpha_test", 
                             label = HTML("Total &alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                prettyRadioButtons(
                  inputId = "exRadio",
                  label = "Click me!",
                  choices = c("Simple successive procedure",
                              "Parallel gatekeeping procedure"),
                  shape = "round",
                  fill = TRUE,
                  inline = TRUE
                )
         ),
         column(4,# style = "background-color:#FFFAFA;",
                h3("Details",align = "center"),
                h5(HTML("Transition matrix <em>G</em>")),
                tableOutput("uioutput_Tmatrix_df"),
                br(),br(),
                h5(HTML("Weights <em>w</em> and <em>p</em>-values")),
                tableOutput("uioutput_Tmatrix_wp"),
                br(),
                shinyjs::useShinyjs()),
         column(5,style = "background-color: AliceBlue;",
                h3("Results", align = "center"),
                plotOutput("resPlots_both")
                # plotOutput("resPlots_final")
         )
)
         

tabTest <- tabPanel("Example tests", icon=icon("picture", lib = "glyphicon"),  
         column(3,style = "background-color: skyblue;",
                h3("Examples", align = "center"),
                prettyRadioButtons(
                  inputId = "exRadio",
                  label = "Click me!",
                  choices = c("Simple successive procedure",
                              "Second"),
                  shape = "round",
                  fill = TRUE,
                  inline = TRUE
                ),
                numericInput(inputId = "alpha_test", 
                             label = HTML("&alpha;"),
                             value = 0.05,step = 0.001,min = 0),
         ),
         column(5,
           h2("Details", align = "center"),
           plotOutput("resPlots_ini"),
           plotOutput("resPlots_final")
         ),
         column(width = 4,
           h2("Result", align = "center"),
           tableOutput("uioutput_Tmatrix_df"),
           br(),
           tableOutput("uioutput_Tmatrix_wp")
         ))
         

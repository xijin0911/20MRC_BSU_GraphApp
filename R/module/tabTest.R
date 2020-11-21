tabTest <- tabPanel("Example test", icon=icon("picture", lib = "glyphicon"),  
         column(3,style = "background-color: skyblue;",
                h3("Examples", align = "center"),
                prettyRadioButtons(
                  inputId = "exRadio",
                  label = "Click me!",
                  choices = c("Simple successive procedure",
                              "Second", 
                              "Third"),
                  shape = "round",
                  fill = TRUE,
                  inline = TRUE
                ),
                
         ),
         column(4,
           h2("Details", align = "center"),
           p("Please double click the cell to edit")
         ),
         column(
           h2("Result", align = "center"),
           width = 5
         ))
         

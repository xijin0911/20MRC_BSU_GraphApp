tabTest <- tabPanel("Specific tests",
                    # icon=icon("picture", lib = "glyphicon"), 
                    # includeCSS("introjs.min.css"),
                    # includeCSS("app.css"),
                    # includeScript("intro.min.js"),
                    # includeScript("app.js"),
                    # div(class="flexcontainer",
                    #     span(actionButton(inputId="test_instruction", 
                    #                       label="Guide", 
                    #                       class="btn-default",icon("atom")),
                    #          style = "position:absolute;left:2em;")),
                    fluidRow( column(3,style = "background-color: skyblue;",
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
                # h5(HTML("Transition matrix <em>G</em>")),
                actionButton("G_infor", HTML("Transition matrix <em>G</em>"), icon("paper-plane"),
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("G_infor", "The propagation of significance levels",
                          "right", options = list(container = "body")),
                tableOutput("uioutput_Tmatrix_df"),
                br(),br(),
                # h5(HTML("Weights <em>w</em> and <em>p</em>-values")),
                actionButton("wp_infor", HTML("Weights <em>w</em> and <em>p</em>-values"), icon("paper-plane"),
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("wp_infor", "Initial weights and <em>p</em>-values",
                          "right", options = list(container = "body")),
                tableOutput("uioutput_Tmatrix_wp"),
                br(),
                shinyjs::useShinyjs()),
         column(5,style = "background-color: AliceBlue;",
                h3("Results", align = "center"),
                tableOutput("rejtable"),
                plotOutput("resPlots_both")
                # plotOutput("resPlots_final"))
         ))
)
         

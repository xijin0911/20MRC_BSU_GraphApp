tabTest <- tabPanel("Specific tests",
                    fluidRow(
                      column(3,style = "background-color: skyblue;",
                h2("Examples", align = "center"),
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
                ),
                hr(),
                p(style="font-family:courier;","Reference"),
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  div(HTML("Maurer, W., Glimm, E., & Bretz, F. (2011). Multiple and repeated testing of primary, coprimary, and secondary hypotheses. <i>Statistics in Biopharmaceutical Research</i>, 3(2), 336-352."))
                ),
                conditionalPanel(
                  condition = "input.exRadio == 'Parallel gatekeeping procedure'",
                  div(HTML("Dmitrienko, A., Offen, WW, & Westfall, PH (2003). Gatekeeping strategies for clinical trials that do not require all primary effects to be significant. <i>Statistics in medicine</i>, 22 (15), 2387-2400."))
                ),br()
         ),
         column(4,# style = "background-color:#FFFAFA;",
                h2("Details",align = "center"),
                actionButton("G_infor", HTML("Transition matrix <em>G</em>"), 
                             icon("paper-plane"),width = "180px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("G_infor", "The propagation of significance levels",
                          "right", options = list(container = "body")),
                withSpinner(tableOutput("uioutput_Tmatrix_df")),
                br(),br(),
                actionButton("wp_infor", HTML("Weights <em>w</em> and <em>p</em>-values"),
                             icon("paper-plane"),width = "180px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("wp_infor", "Initial weights and <em>p</em>-values",
                          "right", options = list(container = "body")),
                withSpinner(tableOutput("uioutput_Tmatrix_wp")),
                br(),
                shinyjs::useShinyjs()),
         column(5,style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:80%;",
                h2("Results", align = "center"),
                withSpinner(tableOutput("rejtable")),
                withSpinner(plotOutput("resPlots_both"))
         ))
)
         

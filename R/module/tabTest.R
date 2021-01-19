tabTest <- tabPanel("Case studies",
                    fluidRow(
                      column(3,style = "background-color: skyblue;",
                h2("Clinical trials", align = "center"),
                numericInput(inputId = "alpha_test", 
                             label = HTML("Total &alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                prettyRadioButtons(
                  inputId = "exRadio",
                  label = "Specific procedures",
                  choices = c("Parallel gatekeeping procedure",
                              "Simple successive procedure"),
                  shape = "round",
                  fill = TRUE,
                  inline = TRUE),
                br(),
                conditionalPanel(
                  condition = "input.exRadio == 'Parallel gatekeeping procedure'",
                  div("Hypotheses are classified into primary and secondary hypotheses. 
                      One may proceed to secondary family tests when at least one of the primary tests exhibits significance.")
                ),
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  div(HTML("Hypotheses is arranged in a hierarchy with some hypotheses being equally important and
                  others being formally tested only conditional on the rejection of more important ones."))
                ),
                hr(),
                p(style="font-family:courier;","Reference"),
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  div(HTML("Maurer, W., Glimm, E., & Bretz, F. (2011). Multiple and repeated testing of primary, coprimary, and secondary hypotheses. <i>Statistics in Biopharmaceutical Research</i>, 3(2), 336-352."))),
                conditionalPanel(
                  condition = "input.exRadio == 'Parallel gatekeeping procedure'",
                  div(HTML("Dmitrienko, A., Offen, WW, & Westfall, PH (2003). Gatekeeping strategies for clinical trials that do not require all primary effects to be significant. <i>Statistics in medicine</i>, 22 (15), 2387-2400."))
                ),br()
         ),
         column(4,# style = "background-color:#FFFAFA;",
                h2("Details",align = "center"),
                HTML("<p>Please click in the white space after finishing inputs.</p>"),
                actionButton("G_infor2", HTML("Transition matrix <em>G</em>"), 
                             icon("paper-plane"),width = "180px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("G_infor2", "The propagation of significance levels",
                          "right", options = list(container = "body")),
                withSpinner(tableOutput("uioutput_Tmatrix_df")),
                br(),br(),
                actionButton("wp_infor2", HTML("Weights <em>w</em> and <em>p</em>-values"),
                             icon("paper-plane"),width = "180px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                bsTooltip("wp_infor2", "Initial weights and <em>p</em>-values",
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
         

tabProcedure <- tabPanel("Common procedures",
                         includeCSS("www/style.css"),
                         fluidRow(
                           column(id="Settings_procedure",3,
                                  style="background-color: skyblue;",
                                  h2("Settings", align = "center"),
                              collapsible = FALSE,solidHeader = TRUE,collapsed = TRUE,
                              numericInput(inputId="Number_Hypotheses",
                                           label="Number of hypotheses:",
                                           value=3,step = 1,min = 1),
                              numericInput(inputId = "alpha_procedure", 
                                           label = HTML("Total &alpha;"),
                                           value = 0.05,step = 0.001,min = 0),
                              selectInput(inputId = "common_procedures",
                                          label = "Common procedures",
                                          choices = c("Bonferroni-Holm procedure",
                                                      "Fixed sequence test",
                                                      "Fallback procedure"),
                                          selected = "Bonferroni-Holm procedure"),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Bonferroni-Holm procedure'",
                                div("All hypotheses have the same weights if the procedure is unweighted.")
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fixed sequence test'",
                                div(HTML("Each hypothesis is tested in the pre-specified sequence at level &alpha; until the first non-rejection."))
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fallback procedure'",
                                div(strong("Note:"), HTML("Each hypothesis is tested in the pre-specified sequence, and the total &alpha; is split into hypotheses. 
                                                          &sum;<sub>1</sub><sup>K</sup>&alpha;<sub>i</sub>=&alpha; for each <em>H<sub>i</sub></em>."))
                              ),
                              hr(),
                              p(style="font-family:courier;","Reference"),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Bonferroni-Holm procedure'",
                                div(HTML("Holm, S. (1979). A Simple Sequentially Rejective Multiple Test Procedure. <i>Scandinavian Journal of Statistics</i>, 6(2), 65-70. Retrieved December 7, 2020."))
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fixed sequence test'",
                                div(HTML("Lehmacher, W., Kieser, M., & Hothorn, L. (2000). Sequential and Multiple Testing for Dose-Response Analysis. <i>Drug Information Journal</i>, 34(2), 591-597.")),
                                div(HTML("Westfall, PH, & Krishen, A. (2001). Optimally weighted, fixed sequence and gatekeeper multiple testing procedures. <i>Journal of Statistical Planning and Inference</i>, 99 (1), 25-40."))
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fallback procedure'",
                                div(HTML("Wiens, BL (2003). A fixed sequence Bonferroni procedure for testing multiple endpoints. Pharmaceutical Statistics: <i>The Journal of Applied Statistics in the Pharmaceutical Industry</i>, 2 (3), 211-215."),
                                    div(HTML("Bretz F., Maurer W., Brannath W., Posch M.: A graphical approach to sequentially rejective multiple test procedures. <i>Statistics in Medicine</i> 2009; 28:586-604.")))
                              ),br()
                       ),
                       column(id="Details_procedure",4,# style = "background-color:#FFFAFA;",
                              style="background-color: AliceBlue;border-color: AliceBlue",
                              h2("Details", align = "center"),
                              HTML("<p>Please click in the white space after finishing inputs.</p>"),
                              actionButton("G_infor", HTML("Transition matrix <em>G</em>"), 
                                           icon("paper-plane"), width = "180px",
                                           style="background-color: AliceBlue; padding:8px; font-size:100%;
                                            border-color: AliceBlue"),
                              br(),
                              bsTooltip("G_infor", "The propagation of significance levels",
                                        "right", options = list(container = "body")),
                              uiOutput("uioutput_Tmatrix1"),
                              br(),br(),
                              actionButton("wp_infor", HTML("Weights <em>w</em> and <em>p</em>-values"), 
                                           icon("paper-plane"),width = "180px",
                                           style="background-color: AliceBlue; padding:8px; font-size:100%;
                                            border-color: AliceBlue; material-flat"),
                              br(),
                              bsTooltip("wp_infor", "Initial weights and <em>p</em>-values",
                                        "right", options = list(container = "body")),
                              uiOutput("uioutput_Tmatrix2"),
                              br(),br(),
                              # HTML("<p>Please click in the white space after finishing inputs.</p>"),
                              shinyjs::useShinyjs()),
                       column(id="Results_procedure",5,
                              style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:80%;",
                              h2("Results", align = "center"),
                              withSpinner(tableOutput("rej_table")),
                              withSpinner(plotOutput("ResultPlot"))
                              ))
                       )

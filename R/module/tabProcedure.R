tabProcedure <- tabPanel("Common procedures", 
                         icon=icon("cog", lib = "glyphicon"),
                         includeCSS("www/style.css"),
                         fluidRow(
                           column(id="Settings_procedure",4,
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
                              # p("Currently, the first attempt of GraphApp is to visualize Bonferroni-based graphical test procedures."),
                              # uiOutput("common_procedures_infor2"),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Bonferroni-Holm procedure'",
                                div(strong("Note:"), 
                                HTML("<p>All hypotheses have the same weights if the procedure is unweighted, 
                                    the test procedure could also be applied with unequal splitting of the significance level 
                                    into a list of &alpha;i, the sum of which should be total &alpha;.</p>"))
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fixed sequence test'",
                                div(strong("Note:"), 
                                    HTML("<p>Each hypothesis is tested in the pre-specified sequence at level &alpha; until the first non-rejection. 
                                         As soon as a hypothesis <em>Hi</em> cannot be rejected, <em>pi</em> > &alpha;, 
                                         the procedure stops and all remaining hypotheses are not rejected.</p>"))
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fallback procedure'",
                                div(strong("Note:"), 
                                HTML("Each hypothesis is tested in the pre-specified sequence, and the total &alpha; is split into hypotheses 
                                with individual &alpha;<sub>i</sub> for each <em>H<sub>i</sub></em>. 
                                     All hypotheses will be tested even if initial hypotheses are not rejected.")
                                    )
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
                              ),
                              br()
                       ),
                       column(id="Details_procedure",4,# style = "background-color:#FFFAFA;",
                              style="background-color: AliceBlue;border-color: AliceBlue",
                              h2("Details", align = "center"),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Bonferroni-Holm procedure'",
                                actionLink("link_to_tabpanel_b", "5-step animated example"),
                              ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fixed sequence test'",
                                p("Graphs for three hypotheses with the rejection sequences"),
                                HTML("<div style='height: 150px;'>"),
                                imageOutput('FSfig',"30%"),
                                HTML("</div>")
                                # ,
                                # actionButton("FS_show", "Know more!")
                                ),
                              conditionalPanel(
                                condition = "input.common_procedures == 'Fallback procedure'",
                                HTML("<p style='font-size:80%;'>Graphs for three hypotheses with the rejection sequences</p>"),
                                HTML("<div style='height: 150px;'>"),
                                imageOutput('FBfig',"30%"),
                                HTML("</div>")
                                ),
                              actionButton("wp_infor", HTML("Weights <em>w</em> and <em>p</em>-values (Nodes)"), 
                                           icon("paper-plane"),width = "240px",
                                           style="background-color: AliceBlue; padding:8px; font-size:100%;
                                            border-color: AliceBlue; material-flat"),
                              br(),
                              # bsTooltip("wp_infor", "Initial weights and <em>p</em>-values",
                              #           "right", options = list(container = "body")),
                              uiOutput("uioutput_Tmatrix2"),
                              br(),br(),
                              
                              actionButton("G_infor", HTML("Transition matrix <em>G</em> (Edges)"), 
                                           icon("paper-plane"), width = "220px",
                                           style="background-color: AliceBlue; padding:8px; font-size:100%;
                                            border-color: AliceBlue"),
                              br(),
                              # bsTooltip("G_infor", "The propagation of significance levels",
                              #           "right", options = list(container = "body")),
                              uiOutput("uioutput_Tmatrix1"),
                              br(),br(),
                              # HTML("<p>Please click in the white space after finishing inputs.</p>"),
                              shinyjs::useShinyjs()),
                       column(id="Results_procedure",4,
                              style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:80%;",
                              h2("Results", align = "center"),
                              withSpinner(tableOutput("rej_table")),
                              withSpinner(plotOutput("ResultPlot"))
                              ))
                       )

tabtweak <- tabPanel("Example", icon=icon("cog", lib = "glyphicon"),  
         column(3,style = "background-color: skyblue;",
                h2("Settings"),
                br(),
                collapsible = FALSE,solidHeader = TRUE,collapsed = TRUE,
                numericInput(inputId="Number_Hypotheses",
                             label="Number of Hypotheses:",
                             value=3,step = 1,min = 1),
                br(),
                numericInput(inputId = "alpha", 
                             label = HTML("&alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                br(),
                selectInput(inputId = "Weighting_Strategy2",
                            label = "Weighting Strategy",
                            choices = c("Bonferroni-Holm procedure",
                                        "Fixed sequence test",
                                        "Fallback procedure",
                                        "Simple successive procedure"),
                            selected = "Bonferroni-Holm procedure"),
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Bonferroni-Holm procedure'",
                  div(strong("Note:"), "All hypothesese have the same weights.", style = "color:blue")
                ),
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Fixed sequence test'",
                  div(strong("Note:"), "The subsequent tests will not be performed unless the previous hypothesis is tested significantly", style = "color:blue")
                ),
                
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Fallback procedure'",
                  div(strong("Note:"), "All hypotheses with same weights have a priori testing orde", style = "color:blue")
                ),
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Simple successive procedure'",
                  div(strong("Note:"), "The number of hypotheses is fixed, you must set it as 4", style = "color:red")
                ),
                hr(),
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Bonferroni-Holm procedure'",
                  p("Holm, S. (1979). A Simple Sequentially Rejective Multiple Test Procedure. Scandinavian Journal of Statistics, 6(2), 65-70. Retrieved November 2, 2020, from http://www.jstor.org/stable/4615733")
                ),
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Fixed sequence test'",
                  p(("Lehmacher, W., Kieser, M., & Hothorn, L. (2000). Sequential and Multiple Testing for Dose-Response Analysis. Drug Information Journal, 34(2), 591–597.")),
                  p(("Westfall, PH, & Krishen, A. (2001). Optimally weighted, fixed sequence and gatekeeper multiple testing procedures. Journal of Statistical Planning and Inference , 99 (1), 25-40."))
                ),
                
                conditionalPanel(
                  condition = "input.Weighting_Strategy2 == 'Fallback procedure'",
                  p(("Wiens, BL (2003). A fixed sequence Bonferroni procedure for testing multiple endpoints. Pharmaceutical Statistics: The Journal of Applied Statistics in the Pharmaceutical Industry , 2 (3), 211-215."),
                    p(("Bretz F., Maurer W., Brannath W., Posch M.: A graphical approach to sequentially rejective multiple test procedures. Statistics in Medicine 2009; 28:586-604.")))
                ),
                
                conditionalPanel(
                condition = "input.Weighting_Strategy2 == 'Simple successive procedure'",
                p(("Maurer W., Glimm E., Bretz F.: Multiple and repeated testing of primary, co-primary and secondary hypotheses. Statistics in Biopharmaceutical Reserach 2011.")
                ))
         ),
                
         column(9,
                uiOutput("uioutput_Tmatrix"),
                br(),
                box(width=10,
                    actionButton("TestButton", "Testing!"),
                    conditionalPanel(condition = "input.TestButton != 0",
                                     plotOutput("ResultPlot")),
                    br(),br(),
                    p("Initial and final graph")),
         br(),br(),br(),
         shinyjs::useShinyjs()),
         column(3),
         column(9,
         a(id = "Moreinformation",
           "More information about the resulting Transition Matrix and Wights"),
         shinyjs::hidden(
           div(id = "moreinfor",
               box(width=4,
                   tableOutput("extend1")),
               box(width=4,
                   tableOutput("extend2"))
               )
         )),
)

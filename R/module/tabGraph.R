tabgraph <- tabPanel("graph",                
         fluidPage(
           fluidRow(
             column(3,style = "background-color: skyblue;",
                    h2("Settings"),
                    hr(),
                    useShinyalert(),
                    
                    actionButton("spec", "Number of Hypotheses"),
                    bsTooltip("spec", "It must be specified if you specify the weighting strategy",
                              "right", options = list(container = "body")),
                    hr(),
                    selectInput(inputId = "Weighting_Strategy",
                                label = "Weighting Strategy",
                                choices = c("Specify ...","Bonferroni-Holm procedure","Fixed sequence test","Fallback procedure"),
                                selected = "Specify ..."),
                    conditionalPanel(
                      condition = "input.Weighting_Strategy == 'Bonferroni-Holm procedure'",
                      div(strong("Note:"), "All hypothesese have the same weights.", style = "color:blue"),
                      # tabPanel("Literature",
                      #         p("Holm, S. (1979). A Simple Sequentially Rejective Multiple Test Procedure. Scandinavian Journal of Statistics, 6(2), 65-70. Retrieved November 2, 2020, from http://www.jstor.org/stable/4615733"))
                    ),
                    conditionalPanel(
                      condition = "input.Weighting_Strategy == 'Fixed sequence test'",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Description",
                                           div(strong("Note:"), "The subsequent tests will not be performed unless the previous hypothesis is tested significantly", style = "color:blue")),
                                  tabPanel("Literature",
                                           p(("Lehmacher, W., Kieser, M., & Hothorn, L. (2000). Sequential and Multiple Testing for Dose-Response Analysis. Drug Information Journal, 34(2), 591–597.")),
                                           p(("Westfall, PH, & Krishen, A. (2001). Optimally weighted, fixed sequence and gatekeeper multiple testing procedures. Journal of Statistical Planning and Inference , 99 (1), 25-40."))
                                  ))),
                    
                    conditionalPanel(
                      condition = "input.Weighting_Strategy == 'Fallback procedure'",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Description",
                                           div(strong("Note:"), "All hypotheses with same weights have a priori testing orde", style = "color:blue")),
                                  tabPanel("Literature",
                                           p(("Wiens, BL (2003). A fixed sequence Bonferroni procedure for testing multiple endpoints. Pharmaceutical Statistics: The Journal of Applied Statistics in the Pharmaceutical Industry , 2 (3), 211-215."),
                                             p(("Bretz F., Maurer W., Brannath W., Posch M.: A graphical approach to sequentially rejective multiple test procedures. Statistics in Medicine 2009; 28:586-604.")))
                                  ))),
                    
                    # verbatimTextOutput("print2"),
                    hr(),
                    # numericInput(inputId="Number_Hypotheses2",
                    #              label="Number of Hypotheses:",
                    #              value=3,step = 1,min = 1),
                    numericInput(inputId = "alpha2", 
                                 label = HTML("&alpha;"),
                                 value = 0.05,step = 0.001,min = 0),
                    hr(),
             ),
             column(5,
                    h2("Graph"),
                    actionButton(inputId = "refreshGraph", label = "Refresh Graph"),
                    visNetworkOutput("visGraph")),
             column(4,
                    h2("Details"),
                    hr(),
                    # actionButton("getNodes", "Nodes for Hypotheses:"),
                    # tableOutput("nodes_all"),
                    hr(),
                    dataTableOutput("graphOutput_visNodes"),
                    hr(),
                    # actionButton("getEdges", "Edges for Transition:"),
                    # tableOutput("edges_all"),
                    dataTableOutput("graphOutput_visEdges")
                    # verbatimTextOutput("print1")
             )),
           shinyjs::useShinyjs(),
           a(id = "toggleAdvanced", "More"),
           hidden(
             div(id = "advanced",
                 box(width=6,
                     numericInput("age", "Weight Matrix", 30)),
                 box(width=6,
                     textInput("company", "Transition Matrix", "")
                 ))
           )
         )
)

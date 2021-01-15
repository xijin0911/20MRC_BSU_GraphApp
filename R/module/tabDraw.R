tabDraw <- tabPanel("Draw", icon=icon("pencil", lib = "glyphicon"),
                    includeCSS("www/introjs.min.css"),
                    includeCSS("www/app.css"),
                    includeScript("www/intro.min.js"),
                    includeScript("www/app.js"),  
                    br(),
                    div(class="flexcontainer",
                        span(actionButton(inputId="draw_instruction", 
                                          label="Instruction", 
                                     class="btn-default",icon("atom")),
                             style = "position:absolute;left:2em;")),
                    fluidRow(
                      # column 1
                      column(id="Graph",width = 5,
                             style="background-color: AliceBlue;border-color: AliceBlue", 
                             h2("Graph", align = "center"),
                             HTML("<p class='text-danger'>You cannot add a node with 'undefined' elements.</p>"),
                             withSpinner(visNetworkOutput("editable_network", height = "400px"))), # loading part
                      # column 2
                      column(id="Details",width = 4,
                             style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:85%;",
                             h2("Details", align = "center"),
                             p("Please double click the cells below to edit and click in the white space after finishing inputs."),
                             numericInput(inputId = "alpha_draw",
                                          label = HTML("Total &alpha;"),
                                          value = 0.05,step = 0.001,min = 0),
                        actionButton("node_infor", "Nodes:", icon("paper-plane"),
                                     style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                        bsTooltip("node_infor", "The nodes table includes the information about individual hypotheses",
                                  "right", options = list(container = "body")),
                        withSpinner(DTOutput("graphOutput_visNodes")), # loading part
                        # error information about the sum of weights
                        verbatimTextOutput("sum_weight_draw"),
                        actionButton("edge_infor", "Edges:", icon("paper-plane"),
                                     style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                        bsTooltip("edge_infor", "The edges table includes the information about propagation",
                                  "right", options = list(container = "body")),
                        withSpinner(DTOutput("graphOutput_visEdges")), # loading part
                        HTML("The edges table is transformed from the transition matrix <em>G</em>. The propagation level is transferred from the hypothes specified in the row of the transition matrix  <em>G</em> to the hypothesis specified in the column of the transition matrix  <em>G</em>."),
                      ), 
                      # column 3
                    column(id="Results",width = 3,
                           style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:80%;",
                           h2("Results", align = "center"),
                           p(""),br(),
                      tags$head( 
                        tags$style(HTML("h4 {text-decoration: underline;}" 
                        ))),
                      withSpinner(tableOutput("res_Table")), # loading part
                      HTML("The adjusted <i>p</i>-values of hypothesis <em>H<sub>j</sub></em> is the smallest significance level at which 
                           one can reject the hypothesis using the given multiple test procedure."),
                      br(),br(),
                      radioButtons('format', 'Report', c('PDF', 'HTML', 'Word'),
                                   inline = TRUE),
                      downloadButton('report'),
                      br(),br()
                    ))
)

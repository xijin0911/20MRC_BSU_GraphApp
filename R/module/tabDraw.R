tabDraw <- tabPanel("Draw", icon=icon("pencil", lib = "glyphicon"),
                    includeCSS("introjs.min.css"),
                    includeCSS("app.css"),
                    includeScript("intro.min.js"),
                    includeScript("app.js"),  
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
                             h3("Graph", align = "center"),
                             p("Click the Edit button"),
                             visNetworkOutput("editable_network", height = "400px")),
                      # column 2
                      column(id="Details",width = 4,
                             style="background-color: AliceBlue;border-color: AliceBlue", 
                             h3("Details", align = "center"),
                             p("Double click the cells below to edit"),
                             numericInput(inputId = "alpha_draw",
                                          label = HTML("Total &alpha;"),
                                          value = 0.05,step = 0.001,min = 0),
                        actionButton("node_infor", "Nodes:", icon("paper-plane"),
                                     style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                        bsTooltip("node_infor", "The nodes table includes the information about individual hypotheses",
                                  "right", options = list(container = "body")),
                        DTOutput("graphOutput_visNodes"),
                        p("Sum of weights should be no larger than 1."),
                        actionButton("edge_infor", "Edges:", icon("paper-plane"),
                                     style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                        bsTooltip("edge_infor", "The edges table includes the information about transition",
                                  "right", options = list(container = "body")),
                        DTOutput("graphOutput_visEdges")),
                      # column 3
                    column(id="Results",width = 2,
                           style="background-color: AliceBlue;border-color: AliceBlue", 
                           h3("Results", align = "center"),
                           p(""),br(),
                      tags$head( 
                        tags$style(HTML("h4 {text-decoration: underline;}" 
                        ))),
                      tableOutput("res_Table"),
                      downloadButton("report", "Generate report")
                    ))
)

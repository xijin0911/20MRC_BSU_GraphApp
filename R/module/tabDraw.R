tabDraw <- tabPanel("Draw",   icon=icon("pencil", lib = "glyphicon"),
                    
                    includeCSS("introjs.min.css"),
                    includeCSS("app.css"),
                    includeScript("intro.min.js"),
                    includeScript("app.js"),
                    div(class="flexcontainer",
                        actionButton(inputId="startHelp", label="instructions  ", 
                                     class="btn-default",icon("atom"))
                    ),
                    # useShinyalert(),  # Set up shinyalert
                    # actionButton("inst", "Instructions",icon("atom")),
                    fluidRow(
                      # column 1
                      column( h3("Graph", align = "center"),id="Graph",
                              style="background-color: AliceBlue;border-color: AliceBlue", 
                        width = 4,
                        p("Place new nodes and edges with corresponding elements or edit the elements"),
                        visNetworkOutput("editable_network", height = "400px")),
                      # column 2
                      column(h3("Details", align = "center"),id="Details",
                             style="background-color: AliceBlue;border-color: AliceBlue", 
                        p("You can also double click the cells below to edit"),
                        numericInput(inputId = "alpha_draw", 
                                     label = HTML("Total &alpha;"),
                                     value = 0.05,step = 0.001,min = 0),
                        width = 4,
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
                    column(h3("Results", align = "center"),id="Results",
                    style="background-color: AliceBlue;border-color: AliceBlue", 
                      width = 4,
                      p("Manipulate on graph or in tables to obtain the result"),
                      br(),
                      tags$head( 
                        tags$style(HTML("h4 {text-decoration: underline;}" 
                        ))),
                      # actionButton("rej_infor", "Rejection results:", icon("paper-plane"),
                      #              style="background-color: AliceBlue;
                      #                       border-color: AliceBlue"),
                      # bsTooltip("rej_infor", "",
                      #           "right", options = list(container = "body")),
                      tableOutput("res_Table"),
                      # radioButtons('format', 'Report_Draw', c('PDF', 'HTML', 'Word'),
                      #              inline = TRUE),
                      # downloadButton('Report_Draw'),
                      # sliderInput("slider", "Slider", 1, 100, 50),
                      downloadButton("report", "Generate report")
                    ))
)

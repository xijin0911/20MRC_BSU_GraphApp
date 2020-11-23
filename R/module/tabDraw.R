tabDraw <- tabPanel("Draw",   icon=icon("pencil", lib = "glyphicon"),      
                    fluidRow(
                      column(
                        h2("Graph", align = "center"),
                        width = 4,
                        p("Place new nodes and edges with corresponding elements"),
                        visNetworkOutput("editable_network", height = "400px")
                      ),
                      column(
                        h2("Details", align = "center"),
                        p("You can also double click the cells below to edit"),
                        numericInput(inputId = "alpha_draw", 
                                     label = HTML("&alpha;"),
                                     value = 0.05,step = 0.001,min = 0),
                        width = 4,
                        h4("Nodes:"),
                        DTOutput("graphOutput_visNodes"),
                        h4("Edges:"),
                        DTOutput("graphOutput_visEdges")
                      ),
                      
                    column(
                      h2("Result", align = "center"),
                      width = 4,
                      p("Manipulate on graph or in tables to obtain the result"),
                      br(),
                      tags$head( 
                        tags$style(HTML("h4 {text-decoration: underline;}" 
                        )) 
                      ),
                      h4("Rejection results:"),
                      tableOutput("res_Table1"),
                      h4("Adjusted p-values:"),
                      tableOutput("res_Table2")
                    ))
)

tabDraw <- tabPanel("Draw",   icon=icon("pencil", lib = "glyphicon"),      
                    fluidRow(
                      column(
                        h2("Graph", align = "center"),
                        width = 5,
                        visNetworkOutput("editable_network", height = "400px")
                      ),
                      column(
                        h2("Details", align = "center"),
                        p("Please double click the cell to edit"),
                        width = 4,
                        h4("Nodes:"),
                        DTOutput("graphOutput_visNodes"),
                        h4("Edges:"),
                        DTOutput("graphOutput_visEdges")
                      ),
                    column(
                      h2("Result", align = "center"),
                      width = 3,
                      numericInput(inputId = "alpha_draw", 
                                   label = HTML("&alpha;"),
                                   value = 0.05,step = 0.001,min = 0),
                      p("Rejection Results:"),
                      tableOutput("res_Table1"),
                      p("Adjusted p-values:"),
                      tableOutput("res_Table2")
                    ))
)

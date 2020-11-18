tabDraw <- tabPanel("Draw",   icon=icon("pencil", lib = "glyphicon"),      
                    fluidRow(
                      column(
                        width = 6,
                        visNetworkOutput("editable_network", height = "400px")
                      ),
                      column(
                        width = 6,
                        h3("Nodes in the graph:"),
                        DTOutput("all_nodes"),
                        h3("Edges in the graph:"),
                        DTOutput("all_edges")
                      )
                    )
)

tabDrawer <- tabPanel("Drawer", icon=icon("picture", lib = "glyphicon"),
               fluidPage(
                 tagList(
                   fluidRow(
                     column(
                       2,
                       h3("Settings"),
                       hr(),
                       
                       h4("other graph settings"),
                       checkboxInput(
                         inputId = ("addNull"),
                         label = "Automatically add recursive null edges?",
                         value = TRUE
                       ),
                       checkboxInput(
                         inputId = ("showNull"),
                         label = "Show null edges?",
                         value = TRUE
                       ),
                       checkboxInput(
                         inputId = ("crosstalk"),
                         label = "Enable Crosstalk?",
                         value = TRUE
                       ),
                       selectInput(
                         inputId = ("labels"),
                         label = "What info should edge labels contain?",
                         choices = c(
                           "state1", "state2", "type", "parameter", "penalty",
                           "K", "a", "min", "max"
                         ),
                         selected = c("type", "penalty"),
                         multiple = TRUE
                       ),
                       actionButton(inputId = ("refreshGraph"), label = "Refresh Graph")
                     ),
                     
                     # Main graph and changepoints colum
                     column(
                       5,
                       h3("Graph", align = "center"),
                       hr(),
                       visNetworkOutput(("gfpopGraph")) %>% withSpinner(type = 6)
                     ),
                     column(
                       5,
                       h3("Graph Details", align = "center"),
                       tabPanel(
                         h5("Edges"),
                         dataTableOutput(("graphOutput_visEdges")),
                         h5("Nodes"),
                         dataTableOutput(("graphOutput_visNodes"))
                       )
                     )
                     
                   ),
                   
                   actionButton(("browser"), "browser"),
                   tags$script(paste0("$('#", ("browser"), "').hide();")))
               )
)
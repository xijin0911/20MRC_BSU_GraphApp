## app.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)  # load css 
library(rintrojs) # introBox
library(shinyMatrix) # transition matrix
library(shinyThings)
library(shinyWidgets)
library(igraph)
library(dplyr)
library(network)
library(shinythemes)
library(ggnetwork)
library(ggpubr)
library(shinyalert)
library(DT)
library(DiagrammeR)
library(dagitty)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(ggdag)
library(reshape2)
# the most recent version of visNetwork
# devtools::install_github("datastorm-open/visNetwork")
library(visNetwork)
# library(networkD3)

source("R/module/clickpad.R")
source("R/columns.R")
source("R/node.R")
source("R/components.R")
source("R/functions.R")
source("R/func/gMCP_xc2.R")
source("R/func/modify_Visnetwork.R")
source("R/func/generate_graph.R")


# -----------------------------------------------------
ui <- fluidPage(
      theme = shinytheme("cerulean"),
      navbarPage(id = "tabs",title = "GraphApp",collapsible = TRUE,
        #--- graph --
        tabPanel("graph",                
          fluidPage(
            fluidRow(
              column(3,style = "background-color: skyblue;",
                h2("Settings"),
                hr(),
                useShinyalert(),
                selectInput(inputId = "Weighting_Strategy",
                            label = "Weighting Strategy",
                            choices = c("Specify ...","Bonferroni-Holm procedure","Fixed sequence test","Fallback procedure"),
                            selected = "Specify ..."),
                hr(),
                p("It must be specified if you specify the weighting strategy"),
                actionButton("spec", "Number of Hypotheses"),
                # verbatimTextOutput("print2"),
                
                hr(),
                # numericInput(inputId="Number_Hypotheses2",
                #              label="Number of Hypotheses:",
                #              value=3,step = 1,min = 1),
                numericInput(inputId = "alpha2", 
                             label = HTML("&alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                hr()
                ),
              column(5,
                     h2("Graph"),
                     actionButton(inputId = "refreshGraph", label = "Refresh Graph"),
                     visNetworkOutput("graph")),
              column(4,
                h2("Details"),
                hr(),
                # actionButton("getNodes", "Nodes for Hypotheses:"),
                # tableOutput("all_nodes"),
                dataTableOutput("graphOutput_visNodes"),
                # actionButton("getEdges", "Edges for Transition:"),
                # tableOutput("all_edges")
                dataTableOutput("graphOutput_visEdges")
              )),
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
        ),
        #--- tweak --
        tabPanel("tweak", 
              ## 1. hypotheses & alpha 
              box(width = 12,
                  box(witdth = 6, collapsible = FALSE,
                      solidHeader = TRUE,
                      collapsed = TRUE,
                      numericInput(inputId="Number_Hypotheses",
                                   label="Number of Hypotheses:",
                                   value=3,step = 1,min = 1)),
                  box(witdth = 6, collapsible = FALSE,
                      solidHeader = TRUE,
                      collapsed = TRUE,
                      numericInput(inputId = "alpha", 
                                   label = HTML("&alpha;"),
                                   value = 0.05,step = 0.001,min = 0)),
                  uiOutput("uioutput_Tmatrix")),
              br(),br(),
              box(width=12,
                  actionButton("TestButton", "Testing!"),
                  conditionalPanel(condition = "input.TestButton != 0",plotOutput("ResultPlot")),
                  br(),br(),
                  p("Initial and final graph")),
              a(id = "Moreinformation", "More information about the result"),
              hidden(
                div(id = "Moreinfor",
                    box(width=3,"Resulting weights",
                        tableOutput("extend1")),
                    box(width=6,"Resulting Transition Matrix",
                        tableOutput("extend2")),
                    box(width=3,"Resulting Adjusted p-values",
                        tableOutput("extend3"))
                ))),
        #--- examples --
    tabPanel("examples",
             tags$script(src = "net.js"),
             box("Examples",width = "16 col-md-8"),
             box(width = "8 col-md-4",
                 components$examples$team
                )
             )
    ))

init.nodes.df = data.frame(id=character(),
                           label=character(),
                           title = character(),
                           shape = character(),
                           Test=character(),
                           weight=numeric(),
                           pvalue=numeric(),
                           stringsAsFactors=FALSE)
init.edges.df = data.frame(from = character(), 
                           to = character(),
                           title = character(),
                           label = character(),
                           propagation = numeric(),
                           stringsAsFactors = F)

server <- function(input, output,session) { 
  # pop-up for the specification of the number of hypotheses
  values <- reactiveValues()
  values$num <- 3
  observeEvent(input$spec, {
    shinyalert("Number of hypotheses", 
               type = "input",
               inputType = "number",
               inputValue = "3",
               inputId = "num_alert",
               inputPlaceholder = "",
               confirmButtonText = "Yes", 
               # showCancelButton = TRUE,cancelButtonText = "No", 
               callbackR = modalCallback)
  })
  modalCallback <- function(value) {
    value$num <- input$num_alert
  }
  
  # initial values
    graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
    
    output$graph <- renderVisNetwork({
      input$refreshGraph
      Strategy = input$Weighting_Strategy
      num_hypotheses = as.numeric(input$num_alert)
      generate_graph(graph_data,Weighting_Strategy=Strategy,
                     num_hypotheses)
    }) 
    observeEvent(input$graph_Change, {
      event <- input$graph_Change
      graph_data <- modify_visNetwork(event,graph_data)
    })
  
    #----------------------------------------------------------------------
    
    # Update graph when a cell is edited in the visEdges datatable
    # proxy_visEdges <- dataTableProxy("graphOutput_visEdges")
    # observeEvent(input$graphOutput_visEdges_cell_edit, {
    #   info <- input$graphOutput_visEdges_cell_edit
    #   i <- info$row
    #   # Add one to column to shift for id
    #   j <- info$col + 1
    #   v <- info$value
    #   
    #   # Update visNetwork data via proxy
    #   graph_data$edges[i, j] <<- DT::coerceValue(
    #     v, graph_data$edges[i, j, with = F]
    #   )
    #   replaceData(proxy_visEdges, graph_data$edges, resetPaging = FALSE)
    #   
    #   # Make sure the main graphdata stays up-to-date
    #   # graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
    # })
    
    
    # Update graph when a cell is edited in the visNodes datatable
    # proxy_visNodes <- dataTableProxy("graphOutput_visNodes")
    # observeEvent(input$graphOutput_visNodes_cell_edit, {
    #   info <- input$graphOutput_visNodes_cell_edit
    #   i <- info$row
    #   # Add one to column to shift for id
    #   j <- info$col + 1
    #   v <- info$value
    #   
    #   # Update visNetwork data via proxy
    #   graph_data$nodes[i, j] <<- DT::coerceValue(
    #     v, graph_data$nodes[i, j, with = F]
    #   )
    #   replaceData(proxy_visNodes, graph_data$nodes, resetPaging = FALSE)
    #   
    #   # Make sure the main graphdata stays up-to-date
    #   # graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
    # })
    
    
    output$graphOutput_visEdges <- DT::renderDT(
      {
        graph_data$edges[,c("from","to","label","propagation")]
      },
      editable = TRUE,
      options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
    )
    
    output$graphOutput_visNodes <- DT::renderDT(
      {
        graph_data$nodes[,c("Test","weight","pvalue")]
      },
      editable = TRUE,
      options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
    )
    
    output$nodes_all = renderTable({
      graph_data$nodes
      # [,c("Test","weight","pvalue")]
      # DT::datatable(tab_nodes,options = list(searching = FALSE,
      #                                    paging = FALSE))
    })
    output$edges_all = renderTable({
      graph_data$edges[,c("from","to","propagation")]
    })
    
    
    
    output$nodes_data_from_shiny <- DT::renderDataTable({
      if(!is.null(input$ini_network_nodes)){
        nodes <- data.frame(input$ini_network_nodes)
       # info <- nodes[,c("Test","weight","pvalue")]
        # colnames(info) <- lapply(1:num, function(i) {
        #   paste0("H", i)
        # })
        DT::datatable(nodes,options = list(searching = FALSE,
                                           paging = FALSE))
      }
    })
    output$edges_data_from_shiny <- DT::renderDataTable({
      if(!is.null(input$ini_network_edges)){
        edges <- data.frame(input$ini_network_edges)
        DT::datatable(edges,options = list(searching = FALSE,
                                           paging = FALSE))
      }
    })
 
    
    output$uioutput_Tmatrix <- renderUI({
      num <- as.integer(input$Number_Hypotheses)
      df <- (1-diag(num))/(num-1)
      rownames(df) <- lapply(1:num, function(i) {
        paste0("H", i)
      })
      colnames(df) <- rownames(df)
      box(width = 12,
          box(title = "Transition matrix",
              status = "primary", 
              solidHeader = TRUE,
              width = 6, collapsible = TRUE,collapsed = TRUE,
              helpText(""),
              matrixInput(inputId = "TransitionMatrixG",
                          value = df,class = "numeric",
                          cols = list(
                            names = TRUE,extend = FALSE,
                            editableNames = TRUE,delta = 2),
                          rows = list(
                            names = TRUE, extend = FALSE,
                            editableNames = TRUE,delta = 1),
                          copy = TRUE,paste = TRUE)),
          box(title = "Weights and P-values",
              status = "primary",solidHeader = TRUE,
              width = 6,collapsible = TRUE,collapsed = TRUE,
              matrixInput(inputId = "WeightPvalue",
                          value = matrix(cbind(
                            (lapply(1:num, function(i) {
                              paste0("H", i)
                            })),rep(1/num,num),rep(0.01,num)),
                            nrow = num, ncol = 3,
                            dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
                          cols = list(names = TRUE, extend = FALSE,
                                      editableNames = FALSE, delta = 2),
                          rows = list(
                            names = FALSE, extend = FALSE,
                            editableNames = TRUE, delta = 1),
                          copy = TRUE, paste = TRUE)
          ),
      )
    })
    
    # Render the table showing all the nodes in the graph.
    output$all_nodes = renderUI({
      num <- as.integer(input$Number_Hypotheses2)
      df <- (1-diag(num))/(num-1)
      rownames(df) <- lapply(1:num, function(i) {
        paste0("H", i)
      })
      colnames(df) <- rownames(df)
          box(width = 10,
              matrixInput(inputId = "WeightPvalue2",
              value = matrix(cbind(
                (lapply(1:num, function(i) {
                  paste0("H", i)
                })),rep(1/num,num),rep(0.01,num)),
                nrow = num, ncol = 3,
                dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
              cols = list(names = TRUE, extend = FALSE,
                          editableNames = FALSE, delta = 2),
              rows = list(
                names = FALSE, extend = FALSE,
                editableNames = TRUE, delta = 1),
              copy = TRUE, paste = TRUE)
              )
    })
    
    output$all_edges = renderUI({
      num <- as.integer(input$Number_Hypotheses2)
      df <- (1-diag(num))/(num-1)
      rownames(df) <- lapply(1:num, function(i) {
        paste0("H", i)
      })
      colnames(df) <- rownames(df)
      box(width = 10,
          matrixInput(inputId = "TransitionMatrixG2",
                      value = df,class = "numeric",
                      cols = list(
                        names = TRUE,extend = FALSE,
                        editableNames = TRUE,delta = 2),
                      rows = list(
                        names = TRUE, extend = FALSE,
                        editableNames = TRUE,delta = 1),
                      copy = TRUE,paste = TRUE)
      )
    })
    
    
    output$fin_network <- renderVisNetwork(
      finalPlot()
    )
    
    output$uioutput_Tmatrix <- renderUI({
        num <- as.integer(input$Number_Hypotheses)
        df <- (1-diag(num))/(num-1)
        rownames(df) <- lapply(1:num, function(i) {
            paste0("H", i)
        })
        colnames(df) <- rownames(df)
        box(width = 12,
            box(title = "Transition matrix",
                status = "primary", 
                solidHeader = TRUE,
                width = 6, collapsible = TRUE,collapsed = TRUE,
                helpText(""),
                matrixInput(inputId = "TransitionMatrixG",
                            value = df,class = "numeric",
                            cols = list(
                                names = TRUE,extend = FALSE,
                                editableNames = TRUE,delta = 2),
                            rows = list(
                                names = TRUE, extend = FALSE,
                                editableNames = TRUE,delta = 1),
                            copy = TRUE,paste = TRUE)),
            box(title = "Weights and P-values",
                status = "primary",solidHeader = TRUE,
                width = 6,collapsible = TRUE,collapsed = TRUE,
                matrixInput(inputId = "WeightPvalue",
                            value = matrix(cbind(
                                        (lapply(1:num, function(i) {
                                          paste0("H", i)
                                        })),rep(1/num,num),rep(0.01,num)),
                                        nrow = num, ncol = 3,
                                        dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
                            cols = list(names = TRUE, extend = FALSE,
                                        editableNames = FALSE, delta = 2),
                            rows = list(
                                      names = FALSE, extend = FALSE,
                                      editableNames = TRUE, delta = 1),
                            copy = TRUE, paste = TRUE)
                    ),
            )
    })
    
    twoPlots <- eventReactive(input$TestButton,
                              {
                                net <- network(input$TransitionMatrixG,
                                               directed = TRUE,
                                               names.eval = "weights",
                                               ignore.eval = FALSE)
                                net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(net)
                                ###
                                a <-  ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
                                             color = "grey50",
                                             curvature = 0.15) +
                                  geom_nodes(aes(x, y),color = "grey", size = 8) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  geom_edgetext_repel(aes(label = weights), color = "white", fill = "grey25",
                                                      box.padding = unit(0.25, "line")) +
                                  scale_color_brewer(palette = "Set2") +
                                  theme(margin = c(0.1,0.1,0.1,0.1))+
                                  #  scale_size_area("importance", breaks = 1:3, max_size = 9) +
                                  theme_blank()
                                
                                
                                ###
                                WPmatrix <- input$WeightPvalue
                                # 
                                res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                                                weights=as.numeric(input$WeightPvalue[,2]),
                                                pvalues=as.numeric(input$WeightPvalue[,3]),
                                                alpha = input$alpha,fweights = F)
                                res_pvalues <- res$pvalues
                                res_weights <- round(res$weights,digits = 2)
                                res_G <- round(res$G,digits = 2)
                                
                                res_net <- network(res_G,directed = TRUE,
                                                   names.eval = "weights",ignore.eval = FALSE)
                                res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(res_net)
                                res_net %v% "Rejection" <- res$rejected
                                b <- ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  geom_edges(arrow = arrow(length = unit(15, "pt"), type = "closed"),
                                             color = "grey50",
                                             curvature = 0.15) +
                                  geom_nodes(aes(x, y,color = Rejection), size = 12) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  scale_color_brewer(palette = "Set2") +
                                  theme_blank()
                                ggarrange(a,b,
                                          ncol = 2, nrow = 1)
                              })
    
    output$ResultPlot <- renderPlot(
      twoPlots()
    )
    
     output$extend1 <- renderTable(
        {
            net <- network(input$TransitionMatrixG,
                           directed = TRUE,
                           names.eval = "weights",
                           ignore.eval = FALSE)
            res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                     weights = as.numeric(input$WeightPvalue[,2]),
                     pvalues = as.numeric(input$WeightPvalue[,3]),
                     alpha = input$alpha,fweights = F)
            data.frame(weights = res$weights)
        })
     output$extend2 <- renderTable(
         {
             net <- network(input$TransitionMatrixG,
                            directed = TRUE,
                            names.eval = "weights",
                            ignore.eval = FALSE)
             res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                             weights = as.numeric(input$WeightPvalue[,2]),
                             pvalues = as.numeric(input$WeightPvalue[,3]),
                             alpha = input$alpha,fweights = F)
             result <- data.frame(res$G)
             colnames(result) <- rownames(input$TransitionMatrixG)
             result
         })
     output$extend3 <- renderTable(
         {
             net <- network(input$TransitionMatrixG,
                            directed = TRUE,
                            names.eval = "p-values",
                            ignore.eval = FALSE)
             res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                             weights = as.numeric(input$WeightPvalue[,2]),
                             pvalues = as.numeric(input$WeightPvalue[,3]),
                             alpha = input$alpha,fweights = F)
             data.frame("Adjusted p-values" = res$adjpvalues)
         })
    
    # result <- eventReactive(input$TestButton,{
    #     resultG <- gMCP_xc2(matrix = input$TransitionMatrixG,
    #                         weights = rep(num,1/num),
    #                         pvalues= input$WeightPvalue[,3],
    #                         alpha = input$alpha,fweights = F)
    #     colnames(resultG$G) <- NULL
    #     resultG$G
    # })
    # output$resultMatrixG <- renderTable({
    #     result()
    # }) 

    
}

shinyApp(ui, server)


# Manipulate An Existing DataTables Instance
# https://rstudio.github.io/DT/shiny.html

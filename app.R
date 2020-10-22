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

# the most recent version of visNetwork
# devtools::install_github("datastorm-open/visNetwork")
library(visNetwork)
library(ggnetwork)
library(ggpubr)
# library(networkD3)

library(DiagrammeR)
library(dagitty)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(ggdag)
library(reshape2)


#library(visNetwork_new)
source("R/module/clickpad.R")
source("R/columns.R")
source("R/node.R")
source("R/functions.R")
source("R/func/gMCP_xc2.R")
# something wrong with the original function in the package
source("R/func/visOption_xc.R")


components <- list(toolbar = list())




# Components - examples ----
components$examples <- list()

components$examples$team <- tagList(
    h3("Development Team"),
    tags$ul(
        tags$li(tags$a(href = "https://www.mrc-bsu.cam.ac.uk/people/in-alphabetical-order/n-to-s/david-robertson/",
                       "David Robertson")),
        tags$li("Xijin Chen")
    ),
    ### TODO 
    p(
        "All code and detailed instructions for usage is available on GitHub",
        tags$a(
            href = "https://github.com/xijin0911")
    ),
    ### TODO 
    p(
        "If you have any questions or comments, we would love to hear them.",
        "You can email us at ",
        tags$a(href = "chenxijin2017@gmail.com", "chenxijin2017@gmail.com"),
    )
)


# Components - Build ----
components$build <- box(
    title = "Graphical approach for multiple test procedures",
    id = "build-box",
    width = 12,
    fluidRow(
        id = "shinydag-toolbar",
        tags$div(
            class = "col-xs-12 col-md-5 shinydag-toolbar-actions",
            tags$div(
                class = "col-xs-12 col-sm-6 col-md-12",
                id = "shinydag-toolbar-node-list-action",
                components$toolbar$node_list_action
            ),
            tags$div(
                class = "col-xs-12 col-sm-6 col-md-12",
                style = "padding: 10px",
                id = "shinydag-toolbar-clickpad-action",
                components$toolbar$clickpad_action
            )
        ),
        tags$div(
            class = "col-xs-12 col-md-7",
            components$toolbar$node_list_name
        )
    ),
    fluidRow(
        column(
            width = 12,
            tags$div(
                class = "pull-left",
                uiOutput("node_list_helptext")
            ),
            shinyThings::undoHistoryUI(
                id = "undo_rv", 
                class = "pull-right",
                back_text = "Undo",
                fwd_text = "Redo"
            )
        )
    ),
    fluidRow(
        column(
            width = 12,
            clickpad_UI("clickpad", height = "600px", width = "100%")
        )
    ),
    if (getOption("shinydag.debug", FALSE)) fluidRow(
        column(width = 12, shinyThings::undoHistoryUI_debug("undo_rv"))
    ),
    fluidRow(
        tags$div(
            class = class_3_col,
            selectInput("exposureNode", "Exposure", choices = c("None" = ""), width = "100%")
        ),
        tags$div(
            class = class_3_col,
            selectInput("outcomeNode", "Outcome", choices = c("None" = ""), width = "100%")
        ),
        tags$div(
            class = class_3_col,
            selectizeInput("adjustNode", "Adjust for...", choices = c("None" = ""), width = "100%", multiple = TRUE)
        )
    ),
    fluidRow(
        tags$div(
            class = "col-sm-12 col-md-9 col-lg-6",
            uiOutput("dagExposureOutcomeDiagnositcs")
        )
    )
)

# -----------------------------------------------------
ui <- fluidPage(
      theme = shinytheme("cerulean"),
      navbarPage(
        id = "tabs",
        title = "GraphApp",
        collapsible = TRUE,
        #--- graph --
        tabPanel(
          "graph",                
          fluidPage(
            fluidRow(
              column(
                3,style = "background-color: skyblue;",
                h2("Settings"),
                hr(),
                numericInput(inputId="Number_Hypotheses2",
                             label="Number of Hypotheses:",
                             value=3,step = 1,min = 1),
                numericInput(inputId = "alpha2", 
                             label = HTML("&alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                hr(),
                selectInput(inputId = "Weighting_Strategy",
                            label = "Weighting Strategy",
                            choices = c("Specify the weighting strategy...","Bonferroni-Holm procedure","Fixed sequence test","Fallback procedure"),
                            selected = "Specify the weighting strategy..."),
                actionButton(inputId = "refreshGraph", label = "Refresh Graph"),
                br(),
                actionButton(inputId = "runGfpop", label = "Run!"),
              ),
              
            
              column(6,
                     h2("Graph"),
                     visNetworkOutput("ini_network")),
              column(3,
                h2("Details"),
                hr(),
                actionButton("getNodes", "Nodes for Hypotheses:"),
                tableOutput("nodes_all"),
                # tableOutput("all_nodes"),
                
                actionButton("getEdges", "Edges for Transition:"),
                tableOutput("edges_all")
                #  tableOutput("all_edges")
              )
            ),
            # box(width=12,
            #     box(width=6,
            #         actionButton("getNodes", "Nodes"),
            #         #   DT::dataTableOutput("nodes_data_from_shiny")),
            #         # tableOutput("nodes_all")
            #         ),
            #     box(width=6,
            #         actionButton("getEdges", "Edges"),
            #         #  DT::dataTableOutput("edges_data_from_shiny"))
            #         # tableOutput("edges_all")
            #         )
            # ),
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
            #---
            # tweak
            #---
    tabPanel(
              "tweak", 
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
              
              br(),
              br(),
              box(width=12,
                  actionButton("TestButton", "Testing!"),
                  conditionalPanel(condition = "input.TestButton != 0",plotOutput("ResultPlot")),
                  br(),
                  br(),
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
            #---
            # examplese
            #---
    tabPanel(
                 "examples",
                 tags$script(src = "net.js"),
                box(
                    title = "Examples",
                    width = "16 col-md-8",
                   # examples_UI("example")
                ),
                 box(
                    width = "8 col-md-4",
                    components$examples$team
                )
             )
    
    ))
#         )
#     )
# )
init.nodes.df = data.frame(id=integer(),
                           label=character(),
                           Hypothesis=character(),
                           weight=numeric(),
                           pvalue=numeric(),
                           stringsAsFactors=FALSE)
init.edges.df = data.frame(from = character(), 
                           to = character(),
                           propagation = numeric(),
                           stringsAsFactors = F)
server <- function(input, output,session) {    
    # ---- graph - Xijin ----
    # onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))
    # onclick("Moreinformation", toggle(id = "Moreinfor", anim = TRUE))
    # observe({ toggle(id="action", condition=!is.null(input$location))})
    
    
    # `graph_data` is a list of two data frames: one of nodes, one of edges.
    graph_data = reactiveValues(
      nodes = init.nodes.df,
      edges = init.edges.df
    )
    
    output$ini_network <- renderVisNetwork({
      # -- different plots in common --
        num <- as.integer(input$Number_Hypotheses2)
        names <- as.matrix(lapply(1:num, function(i) {
            paste0("H", i)
        }))
        # -- plot --
        if(input$Weighting_Strategy == "Specify the weighting strategy..."){
          # nodes <- data.frame(NULL)
          # edges <- data.frame(from = NULL, to = NULL)
          
          netplot <-  visNetwork(graph_data$nodes, graph_data$edges, 
                                 width="100%", height="800px") %>%
            visExport() %>%
            visEdges(arrows = 'to') %>% 
            # visOption_xc(highlightNearest = TRUE,
            #            manipulation = TRUE
            #            #nodesIdSelection = list(enabled = TRUE, selected = "a")
            # ) %>%
            visOption_xc(manipulation = list(enabled = T,
                                             editEdgeCols = c("propagation"),
                                             editNodeCols = c("Hypothesis", "weight", "pvalue"),
                                             addNodeCols = c("Hypothesis", "weight", "pvalue"))) %>%
            visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                           dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
        }
        if(input$Weighting_Strategy == "Bonferroni-Holm procedure"){
          nodes <- data.frame(id = names)
          nodes$label <- names
          nodes$Hypothesis <- names
          nodes$weight <- round(rep(1/num,num),digits = 2)
          nodes$pvalue <- rep(0.01,num)
          nodes$title  <- lapply(1:num, function(i) {
            paste(paste0(nodes$id[i],":"),
                  paste0("weight= ", round(nodes$weight[i],digits = 2)),
                  paste0("p-value= ", nodes$pvalue[i]),sep="<br/>")
          })
          graph_data$nodes <- nodes
          
          # -- initial setting for edges based on weighting strategy --
          df <- (1-diag(num))/(num-1)  
          rownames(df) <- names
          colnames(df) <- names
          net <- network(df,   
                         directed = TRUE,
                         names.eval = "weights",
                         ignore.eval = FALSE)
          edges <- melt(df)
          colnames(edges) <- c("from","to","propagation")
          edges <- edges[which(edges$propagation!=0),] 
          edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
          graph_data$edges <- edges
          
          netplot <-  visNetwork(graph_data$nodes, graph_data$edges, 
                                 width="100%", height="800px") %>%
            visExport() %>%
            visEdges(arrows = 'to') %>% 
            visOptions(manipulation = list(enabled = T,
                                           editEdgeCols = c("propagation"),
                                           editNodeCols = c("Hypothesis", "weight", "pvalue"),
                                           addNodeCols = c("Hypothesis", "weight", "pvalue"))) %>%
            visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                           dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
        }
        if(input$Weighting_Strategy == "Fixed sequence test"){
          nodes <- data.frame(id = names)
          nodes$label <- names
          nodes$Hypothesis <- names
          nodes$weight <- rep(0,num)
          nodes$weight[1] <- 1 
          nodes$pvalue <- rep(0.01,num)
          nodes$title  <- lapply(1:num, function(i) {
            paste(paste0(nodes$id[i],":"),
                  paste0("weight= ", round(nodes$weight[i],digits = 2)),
                  paste0("p-value= ", nodes$pvalue[i]),sep="<br/>")
          })
          graph_data$nodes <- nodes
          
          df <- matrix(0, nrow = num, ncol = num)
          rownames(df) <- names
          colnames(df) <- names
          for (i in 1:(num-1)){
            df[i,i+1] <- 1
          }
          net <- network(df,   
                         directed = TRUE,
                         names.eval = "weights",
                         ignore.eval = FALSE)
          edges <- melt(df)
          colnames(edges) <- c("from","to","propagation")
          edges <- edges[which(edges$propagation!=0),] 
          edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
          graph_data$edges <- edges
          
          netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                     width="100%", height="800px") %>%
            visExport() %>%
            visEdges(arrows = 'to') %>%
            visOptions(manipulation = list(enabled = T,
                                             editEdgeCols = c("propagation"),
                                             editNodeCols = c("Hypothesis", "weight", "pvalue"),
                                             addNodeCols = c("Hypothesis", "weight", "pvalue"))) %>%
           visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                          dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
        }
        if(input$Weighting_Strategy == "Fallback procedure"){
          nodes <- data.frame(id = names)
          nodes$Hypothesis <- names
          nodes$label <- names
          nodes$weight <- round(matrix(1/num,nrow=num,ncol=1),digits=2) 
          nodes$pvalue <- rep(0.01,num)
          nodes$title  <- lapply(1:num, function(i) {
            paste(paste0(nodes$id[i],":"),
                  paste0("weight= ", round(nodes$weight[i],digits = 2)),
                  paste0("p-value= ", nodes$pvalue[i]),sep="<br/>")
          })
          graph_data$nodes <- nodes
          
          df <- matrix(0, nrow = num, ncol = num)
          rownames(df) <- names
          colnames(df) <- names
          for (i in 1:(num-1)){
            df[i,i+1] <- 1
          }
          net <- network(df,   
                         directed = TRUE,
                         names.eval = "weights",
                         ignore.eval = FALSE)
          edges <- melt(df)
          colnames(edges) <- c("from","to","propagation")
          edges <- edges[which(edges$propagation!=0),] 
          edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
          graph_data$edges <- edges
          
          netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                                 width="100%", height="800px") %>%
            visEdges(arrows = 'to',shadow = FALSE) %>%
            visOptions(manipulation = list(enabled = T,
                                             editEdgeCols = c("propagation"),
                                             editNodeCols = c("Hypothesis", "weight", "pvalue"),
                                             addNodeCols = c("Hypothesis", "weight", "pvalue"))) %>%
            visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                           dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
            visExport() 
        }
        netplot
    })
    
    # reaction after editing the graph
    observeEvent(input$ini_network_graphChange, {
      # If the user added a node, add it to the data frame of nodes.
      if(input$ini_network_graphChange$cmd == "addNode") {
        temp = bind_rows(
          graph_data$nodes,
          data.frame(id = input$ini_network_graphChange$id,
                     label = input$ini_network_graphChange$label,
                     Hypothesis = input$ini_network_graphChange$Hypothesis,
                     weight = input$ini_network_graphChange$weight,
                     pvalue = input$ini_network_graphChange$pvalue,
                     stringsAsFactors = F)
        )
        graph_data$nodes = temp
      }
      # If the user added an edge, add it to the data frame of edges.
      else if(input$ini_network_graphChange$cmd == "addEdge") {
        temp = bind_rows(
          graph_data$edges,
          data.frame(
                     from = input$ini_network_graphChange$from,
                     to = input$ini_network_graphChange$to,
                     propagation = input$ini_network_graphChange$propagation,
                     stringsAsFactors = F)
        )
        graph_data$edges = temp
      }
      # If the user edited a node, update that record.
      else if(input$ini_network_graphChange$cmd == "editNode") {
        temp = graph_data$nodes
        temp$label[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$label
        temp$Hypothesis[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$Hypothesis
        temp$weight[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$weight
        temp$pvalue[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$pvalue
        graph_data$nodes = temp
      }
      # If the user edited an edge, update that record.
      else if(input$ini_network_graphChange$cmd == "editEdge") {
        temp = graph_data$edges
        temp$from[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$from
        temp$to[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$to
        temp$propagation[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$propagation
        graph_data$edges = temp
      }
      # If the user deleted something, remove those records.
      else if(input$ini_network_graphChange$cmd == "deleteElements") {
        for(node.id in input$ini_network_graphChange$nodes) {
          temp = graph_data$nodes
          temp = temp[temp$id != node.id,]
          graph_data$nodes = temp
        }
        for(edge.id in input$ini_network_graphChange$edges) {
          temp = graph_data$edges
          temp = temp[temp$id != edge.id,]
          graph_data$edges = temp
        }
      }
    })
    
    output$nodes_all = renderTable({
      graph_data$nodes[,c("Hypothesis","weight","pvalue")]
    })
    output$edges_all = renderTable({
      graph_data$edges[,c("from","to","propagation")]
    })
    
    
    # --- try 1019 ---

    
    output$nodes_data_from_shiny <- DT::renderDataTable({
      if(!is.null(input$ini_network_nodes)){
        nodes <- data.frame(input$ini_network_nodes)
       # info <- nodes[,c("Hypothesis","weight","pvalue")]
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
    # observeEvent(input$getNodes,{
    #   print(input$current_nodes_selection)
    # })
    # 
    # observeEvent(input$getEdges,{
    #   print(input$current_edges_selection)
    # })
    
    observeEvent(input$getNodes,{
      visNetworkProxy("ini_network") %>%
        visGetNodes()
    })

    observeEvent(input$getEdges, {
      visNetworkProxy("ini_network") %>%
        visGetEdges()
    })
    
    output$shiny_return <- renderPrint({
      input$current_node_weight
    })
    
    # output$nodes_data_from_shiny <- renderDataTable( {
    #   if (!is.null(input$current_node_id) && !is.null(input$ini_network_nodes)) {
    #     info <- data.frame(matrix(unlist(input$ini_network_nodes), 
    #                               ncol = dim(nodes)[1], byrow = T),
    #                        stringsAsFactors = FALSE)
    #     colnames(info) <- colnames(nodes)
    #     info[info$id == input$current_node_id, ]
    #   }
    # })
    # 
    # observeEvent(input$current_node_id, {
    #   visNetworkProxy("ini_network") %>%
    #     visGetNodes() 
    # })
 
    
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
    
      
 #   finalPlot <-eventReactive(input$TestButton2,
                               #  {
                               #    num <- as.integer(input$Number_Hypotheses)
                               #    if(input$Weighting_Strategy == "Bonferroni-Holm procedure"){
                               #    net <- network(input$TransitionMatrixG,
                               #                   directed = TRUE,
                               #                   names.eval = "weights",
                               #                   ignore.eval = FALSE)
                               # #   net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                               #    e <- network.edgecount(net)
                               #    WPmatrix <- input$WeightPvalue
                               #    # 
                               #    res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                               #                    weights=as.numeric(input$WeightPvalue[,2]),
                               #                    pvalues=as.numeric(input$WeightPvalue[,3]),
                               #                    alpha = input$alpha,fweights = F)
                               #    res_pvalues <- res$pvalues
                               #    res_weights <- round(res$weights,digits = 2)
                               #    res_G <- round(res$G,digits = 2)
                               #    
                               #    res_net <- network(res_G,directed = TRUE,
                               #                       names.eval = "weights",ignore.eval = FALSE)
                               #  #  res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                               #    e <- network.edgecount(res_net)
                               #  #  res_net %v% "Rejection" <- res$rejected
                               #    wide <- as.matrix(res_net)
                               #    fromto <- melt(wide)
                               #    fromto <- cbind(fromto,value=melt(input$TransitionMatrixG)[,"value"])
                               #    colnames(fromto) <- c("from","to","trans","label")
                               #    edges <- fromto[which(fromto$trans!=0),] 
                               #    names <- as.matrix(lapply(1:num, function(i) {
                               #      paste0("H", i)
                               #    }))
                               #    nodes <- data.frame(id = names)
                               #    nodes$title  <- lapply(1:num, function(i) {
                               #      paste0("H", i,":",weights[i])
                               #    })
                               #    visNetwork(nodes, edges,
                               #                           width="100%", height="800px") %>%
                               #      visExport() %>%
                               #      visEdges(arrows = 'to') %>%
                               #      visOptions(highlightNearest = TRUE,
                               #                 manipulation = TRUE
                               #                 #nodesIdSelection = list(enabled = TRUE, selected = "a")
                               #      )%>%
                               #     visInteraction(navigationButtons = TRUE)
                               #  })
    
                               #  }
                              # if(input$Weighting_Strategy == "Fixed sequence test"){
                              # }
                              # if(input$Weighting_Strategy == "Fallback procedure"){
                              # }
    
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

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
library(data.table)
library(ggplot2)
library(gridExtra)
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
# source("R/func/modify_visNetwork.R")
source("R/func/generate_graph.R")
source("R/func/generate_data.R")
source("R/func/function_matrix.R")
source("R/func/graph_create.R")
# ui output
source("R/module/tabTweak.R")
source("R/module/tabGraph.R")


# -----------------------------------------------------
ui <- tagList(
  fluidPage(
      theme = shinytheme("cerulean"),
      navbarPage(id = "tabs",title = "GraphApp",collapsible = TRUE,
        tabgraph,
        tabtweak
        # tabexample
    )),
  br(),br(),br(),
  components$foot
)
# -----------------------------------------------------


server <- function(input, output,session) { 
  shinyjs::onclick("Moreinformation",
                   shinyjs::toggle(id = "moreinfor", anim = TRUE))
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
   # graph_data initial setting -----------------------
   # NOTE: It seems that my functions (node_create and edge_create) can not be used in observeEvent function 
  nodes <- reactive({
    switch(input$Weighting_Strategy,
           "Specify ..." = node_create(as.numeric(input$num_alert), "Specify ..."),
           "Bonferroni-Holm procedure" = node_create(as.numeric(input$num_alert),"Bonferroni-Holm procedure"),
           "Fixed sequence test" = node_create(as.numeric(input$num_alert),"Fixed sequence test"),
           "Fallback procedure" = node_create(as.numeric(input$num_alert),"Fallback procedure")
    )
  })
  
  
  edges <- reactive({
    switch(input$Weighting_Strategy,
           "Specify ..." = edge_create(as.numeric(input$num_alert), "Specify ..."),
           "Bonferroni-Holm procedure" = edge_create(as.numeric(input$num_alert),"Bonferroni-Holm procedure"),
           "Fixed sequence test" = edge_create(as.numeric(input$num_alert),"Fixed sequence test"),
           "Fallback procedure" = edge_create(as.numeric(input$num_alert),"Fallback procedure")
    )
  })  
  
  output$graph_data_node <- renderDT({
    init.nodes.df <- nodes()
    init.nodes.df[,c("Test","weight","pvalue")]
  })
  
  output$graph_data_edge <- renderDT({
    init.edges.df <- edges()
    init.edges.df[,c("from","to","propagation")]
  })
# 80 - 110: the depending initial setting for matrix 
  # graph_data initial setting -----------------------
  init.nodes.df = data.frame(id=character(),
                             label=character(),
                             title = character(),
                             shape = character(),
                             Test=character(),
                             weight=numeric(),
                             pvalue=numeric(),
                             stringsAsFactors=FALSE)
  init.edges.df = data.frame(id = character(),
                             label = character(),
                             from = character(), 
                             to = character(),
                             title = character(),
                             propagation = numeric(),
                             stringsAsFactors = F)
  
  
  # NOTE: Must be here, or the changes to the visNetwork cannot be saved in the graph
    graph_data = list(
      nodes = init.nodes.df,
      edges = init.edges.df
    )
    
    output$visGraph <- renderVisNetwork({
      input$refreshGraph
      Strategy = input$Weighting_Strategy
      num_hypotheses = as.numeric(input$num_alert)
      generate_graph(graph_data,Weighting_Strategy=Strategy,
                     num_hypotheses)
    })
    
    # observeEvent(input$visGraph_graphChange, {
    #   # If the user added a node, add it to the data frame of nodes.
    #   if(input$visGraph_graphChange$cmd == "addNode") {
    #     temp = bind_rows(
    #       graph_data$nodes,
    #       data.frame(
    #         id = input$visGraph_graphChange$id,
    #         label = input$visGraph_graphChange$label,
    #         shape = input$visGraph_graphChange$shape,
    #         title = input$visGraph_graphChange$title,
    #         Test = input$visGraph_graphChange$Test,
    #         weight = input$visGraph_graphChange$weight,
    #         pvalue = input$visGraph_graphChange$pvalue,
    #         stringsAsFactors = F)
    #     )
    #     graph_data$nodes = temp
    #   }
    #   # If the user added an edge, add it to the data frame of edges.
    #   else if(input$visGraph_graphChange$cmd == "addEdge") {
    #     temp = bind_rows(
    #       graph_data$edges,
    #       data.frame(
    #         from = input$visGraph_graphChange$from,
    #         title = input$visGraph_graphChange$title,
    #         to = input$visGraph_graphChange$to,
    #         label = input$visGraph_graphChange$label,
    #         propagation = input$visGraph_graphChange$propagation,
    #         stringsAsFactors = F)
    #     )
    #     graph_data$edges = temp
    #   }
    #   # If the user edited a node, update that record.
    #   else if(input$visGraph_graphChange$cmd == "editNode") {
    #     temp = graph_data$nodes
    #     temp$label[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$label
    #     temp$shape[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$shape
    #     temp$title[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$title
    #     temp$Test[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$Test
    #     temp$weight[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$weight
    #     temp$pvalue[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$pvalue
    #     graph_data$nodes = temp
    #   }
    #   # If the user edited an edge, update that record.
    #   else if(input$visGraph_graphChange$cmd == "editEdge") {
    #     temp = graph_data$edges
    #     temp$from[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$from
    #     temp$title[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$title
    #     temp$label[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$label
    #     temp$to[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$to
    #     temp$propagation[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$propagation
    #     graph_data$edges = temp
    #   }
    #   # If the user deleted something, remove those records.
    #   else if(input$visGraph_graphChange$cmd == "deleteElements") {
    #     for(node.id in input$visGraph_graphChange$nodes) {
    #       temp = graph_data$nodes
    #       temp = temp[temp$id != node.id,]
    #       graph_data$nodes = temp
    #     }
    #     for(edge.id in input$visGraph_graphChange$edges) {
    #       temp = graph_data$edges
    #       temp = temp[temp$id != edge.id,]
    #       graph_data$edges = temp
    #     }
    #   }
    # })
    
  
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
    # output$print1 <- renderPrint({
    #   graph_data$edges[,c("from","to","propagation")]
    # })
    
    # Render the table showing all the nodes in the graph.

    
    
    output$graphOutput_visEdges <- DT::renderDT(
      {
        # Strategy = input$Weighting_Strategy
        # num_hypotheses = as.numeric(input$num_alert)
        # graph_data = generate_data(graph_data,Weighting_Strategy=Strategy,
        #                     num_hypotheses)
        data.frame(graph_data$edges[,c("from","to","propagation")])
      },
      editable = TRUE,
      options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F)
    )
    
    output$graphOutput_visNodes <- DT::renderDT(
      {
        # Strategy = input$Weighting_Strategy
        # num_hypotheses = as.numeric(input$num_alert)
        # graph_data = generate_data(graph_data,Weighting_Strategy=Strategy,
        #                     num_hypotheses)
        data.frame(graph_data$nodes[,c("Test","weight","pvalue")])
      },
      editable = TRUE,
      options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F)
    )
    
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
 
# -----------------------------------------------------
    df_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = (1-diag(input$Number_Hypotheses))/(input$Number_Hypotheses-1),
             "Fixed sequence test" = dfcreate(input$Number_Hypotheses),
             "Fallback procedure" = dfcreate(input$Number_Hypotheses)
      )
    })
    wp_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = wpcreat(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = wpcreat(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = wpcreat(input$Number_Hypotheses,"Fallback procedure")
      )
    })
    
    output$uioutput_Tmatrix <- renderUI({
      num <- as.integer(input$Number_Hypotheses)
      df <- df_create()
      rownames(df) <- lapply(1:num, function(i) {
        paste0("H", i)
      })
      colnames(df) <- rownames(df)
      wp <- wp_create()
      box(width = 10,
          box(title = "Transition matrix",
              status = "primary", 
              solidHeader = TRUE,
              width = 6, 
              # collapsible = TRUE,collapsed = TRUE,
              helpText(""),
              matrixInput(inputId = "TransitionMatrixG",
                          value = df,class = "numeric",
                          cols = list(names = TRUE,extend = FALSE,
                            editableNames = TRUE,delta = 2),
                          rows = list(names = TRUE, extend = FALSE,
                            editableNames = TRUE,delta = 1),
                          copy = TRUE,paste = TRUE)),
          box(title = "Weights and P-values",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              # collapsible = TRUE,
              # collapsed = TRUE,
              matrixInput(inputId = "WeightPvalue",
                          value = wp,
                          cols = list(names = TRUE, extend = FALSE,
                                      editableNames = FALSE, delta = 2),
                          rows = list(
                            names = FALSE, extend = FALSE,
                            editableNames = TRUE, delta = 1),
                          copy = TRUE, paste = TRUE)
          )
      )
    })
    
    
    # output$uioutput_Tmatrix <- renderUI({
    #   num <- as.integer(input$Number_Hypotheses)
    #   df <- df_create()
    #   rownames(df) <- lapply(1:num, function(i) {
    #     paste0("H", i)
    #   })
    #   colnames(df) <- rownames(df)
    #   box(width = 10,
    #       box(title = "Transition matrix",
    #           status = "primary", 
    #           # solidHeader = TRUE,
    #           width = 10, 
    #           # collapsible = TRUE,collapsed = TRUE,
    #           helpText(""),
    #           matrixInput(inputId = "TransitionMatrixG",
    #                       value = df,class = "numeric",
    #                       cols = list(
    #                         names = TRUE,
    #                         # extend = FALSE,
    #                         editableNames = FALSE,
    #                         delta = 2),
    #                       rows = list(
    #                         names = TRUE, 
    #                         # extend = FALSE,
    #                         editableNames = FALSE,
    #                         delta = 1),
    #                       copy = TRUE,paste = TRUE)),
    #       box(title = "Weights and P-values",
    #           status = "primary",
    #           # solidHeader = TRUE,
    #           width = 10,
    #           # collapsible = TRUE,collapsed = TRUE,
    #           matrixInput(inputId = "WeightPvalue",
    #                       value = matrix(cbind(
    #                         (lapply(1:num, function(i) {
    #                           paste0("H", i)
    #                         })),rep(1/num,num),rep(0.01,num)),
    #                         nrow = num, ncol = 3,
    #                         dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
    #                       cols = list(names = TRUE, 
    #                                   # extend = FALSE,
    #                                   editableNames = FALSE, delta = 2),
    #                       rows = list(
    #                         names = FALSE, 
    #                         # extend = FALSE,
    #                         editableNames = FALSE, delta = 1),
    #                       copy = TRUE, paste = TRUE)
    #       ),
    #   )
    # })
    
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
    
    # ---------------- Tweak Page output ----------------
    
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
                                  geom_nodes(aes(x, y),color = "grey", size = 12) +
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
                                
                                res_adj <- data.frame("Hypotheses" = paste0("H", 1:input$Number_Hypotheses),
                                                      "Adjusted" = res$adjpvalues)
                              
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
                                  theme_blank()+ 
                                  annotation_custom(tableGrob(res_adj, rows=NULL), 
                                                    xmin=0.8, xmax=1, ymin=0.8, ymax=1)
                                ggarrange(a,b,
                                          ncol = 2, nrow = 1)
                              })
    
    output$ResultPlot <- renderPlot(
      twoPlots()
    )
    
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
            data.frame(Hypotheses = paste0("H", 1:input$Number_Hypotheses),
                       Weights = res$weights)
        })
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
             result <- data.frame(res$G)
             colnames(result) <- rownames(input$TransitionMatrixG)
             rownames(result) <- rownames(input$TransitionMatrixG)
             result
         }, caption = "Zero value means no trasition.", caption.placement = "bottom")
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

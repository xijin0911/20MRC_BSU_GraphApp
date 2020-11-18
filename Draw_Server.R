nodes <- reactive({
  # validate(
  #   need(input$num_alert != "",
  #        "Please specify the number of hypotheses for the nodes"),
  #   errorClass = "myClass")
  # get(
  switch(input$Weighting_Strategy,
         "Specify ..." = node_create(as.numeric(input$num_alert), "Specify ..."),
         "Bonferroni-Holm procedure" = node_create(as.numeric(input$num_alert),"Bonferroni-Holm procedure"),
         "Fixed sequence test" = node_create(as.numeric(input$num_alert),"Fixed sequence test"),
         "Fallback procedure" = node_create(as.numeric(input$num_alert),"Fallback procedure")
  )
  # )
})

edges <- reactive({
  validate(
    need(input$num_alert != "", 
         "Please specify the number of hypotheses for the edges"),
    errorClass = "myClass")
  (switch(input$Weighting_Strategy,
          "Specify ..." = edge_create(as.numeric(input$num_alert), "Specify ..."),
          "Bonferroni-Holm procedure" = edge_create(as.numeric(input$num_alert),"Bonferroni-Holm procedure"),
          "Fixed sequence test" = edge_create(as.numeric(input$num_alert),"Fixed sequence test"),
          "Fallback procedure" = edge_create(as.numeric(input$num_alert),"Fallback procedure")
  ))
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
  Strategy = input$Weighting_Strategy
  num_hypotheses = as.numeric(input$num_alert)
  generate_graph(graph_data,Weighting_Strategy=Strategy,
                 num_hypotheses)
})

observe({
  visNetworkProxy(("visGraph")) %>%
    visUpdateNodes(nodes = graph_data$nodes) %>%
    visUpdateEdges(edges = graph_data$edges)
})

observeEvent(input$visGraph_graphChange, {
  # If the user added a node, add it to the data frame of nodes.
  if(input$visGraph_graphChange$cmd == "addNode") {
    temp = bind_rows(
      graph_data$nodes,
      data.frame(
        id = input$visGraph_graphChange$id,
        label = input$visGraph_graphChange$label,
        shape = input$visGraph_graphChange$shape,
        title = input$visGraph_graphChange$title,
        Test = input$visGraph_graphChange$Test,
        weight = input$visGraph_graphChange$weight,
        pvalue = input$visGraph_graphChange$pvalue,
        stringsAsFactors = F)
    )
    graph_data$nodes = temp
  }
  # If the user added an edge, add it to the data frame of edges.
  else if(input$visGraph_graphChange$cmd == "addEdge") {
    temp = bind_rows(
      graph_data$edges,
      data.frame(
        id = input$visGraph_graphChange$id,
        from = input$visGraph_graphChange$from,
        title = "title",
        to = input$visGraph_graphChange$to,
        label = "label",
        propagation = "propagation",
        stringsAsFactors = F)
    )
    graph_data$edges = temp
  }
  # If the user edited a node, update that record.
  else if(input$visGraph_graphChange$cmd == "editNode") {
    temp = graph_data$nodes
    temp$label[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$label
    temp$shape[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$shape
    temp$title[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$title
    temp$Test[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$Test
    temp$weight[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$weight
    temp$pvalue[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$pvalue
    graph_data$nodes = temp
  }
  # If the user edited an edge, update that record.
  else if(input$visGraph_graphChange$cmd == "editEdge") {
    temp = graph_data$edges
    # temp$from[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$from
    temp$title[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$title
    temp$label[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$label
    # temp$to[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$to
    temp$propagation[temp$id == input$visGraph_graphChange$id] = input$visGraph_graphChange$propagation
    graph_data$edges = temp
  }
  # If the user deleted something, remove those records.
  else if(input$visGraph_graphChange$cmd == "deleteElements") {
    for(node.id in input$visGraph_graphChange$nodes) {
      temp = graph_data$nodes
      temp = temp[temp$id != node.id,]
      graph_data$nodes = temp
    }
    for(edge.id in input$visGraph_graphChange$edges) {
      temp = graph_data$edges
      temp = temp[temp$id != edge.id,]
      graph_data$edges = temp
    }
  }
  graph_data
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
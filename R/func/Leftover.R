output$ini_network <- renderVisNetwork({
  input$refreshGraph
if(input$Weighting_Strategy == "Specify ..."){
  netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                         width="100%", height="800px") %>%
    visExport() %>%
    visEdges(arrows = 'to') %>%
    visOptions(manipulation = list(enabled = T,
                                     editEdgeCols = c("propagation"),
                                     editNodeCols = c("Test", "weight", "pvalue")
                                     # addNodeCols = c("label","title","shape",
                                     #                 "Test", "weight", "pvalue"),
                                     # addEdgeCols=c("propagation","title","from","to")
                                   )) %>%
    visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                   dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
}
if(input$Weighting_Strategy == "Bonferroni-Holm procedure"){
  num <- 3
  names <- as.matrix(lapply(1:num, function(i) {
    paste0("H", i)
  }))
  nodes <- data.frame(id = names)
  nodes$label <- as.character(names)
  nodes$shape <- "circle"
  nodes$Test <- names
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
  edges$label <- edges$propagation
  edges$label <- as.character(edges$label)
  graph_data$edges <- edges

  netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                         width="100%", height="800px") %>%
    visExport() %>%
    visEdges(arrows = 'to',label = label) %>%
    visOptions(manipulation = list(enabled = T,
                                   editEdgeCols = c("label"),
                                   editNodeCols = c("Test", "weight", "pvalue"))) %>%
    visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                   dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
    visLayout(randomSeed = 8)
}
if(input$Weighting_Strategy == "Fixed sequence test"){
  num <- 3
  names <- as.matrix(lapply(1:num, function(i) {
    paste0("H", i)
  }))
  nodes <- data.frame(id = names)
  nodes$label <- as.character(names)
  nodes$shape <- "circle"
  nodes$Test <- names
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
  edges$label <- edges$propagation
  edges$label <- as.character(edges$label)
  edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
  graph_data$edges <- edges

  netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
             width="100%", height="800px") %>%
    visExport() %>%
    visEdges(arrows = 'to') %>%
    visOptions(manipulation = list(enabled = T,
                                   editEdgeCols = c("label"),
                                   editNodeCols = c("Test", "weight", "pvalue"))) %>%
   visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                  dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
    visLayout(randomSeed = 12)
}
      if(input$Weighting_Strategy == "Fallback procedure"){
        num <- 3
        names <- as.matrix(lapply(1:num, function(i) {
          paste0("H", i)
        }))
        nodes <- data.frame(id = names)
        nodes$Test <- names
        nodes$label <- as.character(names)
        nodes$shape <- "circle"
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
        edges$label <- edges$propagation
        edges$label <- as.character(edges$label)
        # edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
        graph_data$edges <- edges

        netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                               width="100%", height="800px") %>%
          visEdges(arrows = 'to',shadow = FALSE) %>%
          visOptions(manipulation = list(enabled = T,
                                         editEdgeCols = c("label"),
                                         editNodeCols = c("Test", "weight", "pvalue"))) %>%
          visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                         dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
          visExport() %>%
          visLayout(randomSeed = 12)
      }
      netplot
  })


observeEvent(input$ini_network_graphChange, {
  # If the user added a node, add it to the data frame of nodes.
  if(input$ini_network_graphChange$cmd == "addNode") {
    temp = bind_rows(
      graph_data$nodes,
      data.frame(id = input$ini_network_graphChange$id,
                 label = input$ini_network_graphChange$label,
                 title = input$ini_network_graphChange$title,
                 shape = input$ini_network_graphChange$shape,
                 Test = input$ini_network_graphChange$Test,
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
        title = input$ini_network_graphChange$title,
        to = input$ini_network_graphChange$to,
        label = input$ini_network_graphChange$label,
        propagation = input$ini_network_graphChange$propagation,
        stringsAsFactors = F)
    )
    graph_data$edges = temp
  }
  # If the user edited a node, update that record.
  else if(input$ini_network_graphChange$cmd == "editNode") {
    temp = graph_data$nodes
    temp$label[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$label
    temp$shape[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$shape
    temp$title[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$title
    temp$Test[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$Test
    temp$weight[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$weight
    temp$pvalue[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$pvalue
    graph_data$nodes = temp
  }
  # If the user edited an edge, update that record.
  else if(input$ini_network_graphChange$cmd == "editEdge") {
    temp = graph_data$edges
    temp$from[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$from
    temp$title[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$title
    temp$label[temp$id == input$ini_network_graphChange$id] = input$ini_network_graphChange$label
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
  # visNetworkProxy("ini_network") %>%
  #   visUpdateNodes(nodes = graph_data$nodes) %>%
  #   visUpdateEdges(edges = graph_data$edges)
})

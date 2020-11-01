modify_visNetwork <- function(event, graph_data) {

  graph_data_return <- graph_data
  ### Edit Edge --------------------------------------------------------------
  if (event$cmd == "editEdge") {
    changed_id <- event$id
    
    angle <- if (event$type == "null") pi else 2 * pi
    
    edges <- data.table(
      graph_data_return$edges
    )
    
    edges[
      id == changed_id,
      c(
        "from","to","title","label","propagation"
      ) :=
        .(event$from, event$to, event$title, event$label,
          event$propagation)
      ]
    
    graph_data_return$edges <- edges
  }
  
  ### Add Edge ---------------------------------------------------------------
  if (event$cmd == "addEdge") {
    graph_data_return$edges <- add_edge(
      edgedf = graph_data_return$edges, id = event$id,
      label = event$label, to = event$to, from = event$from,title = event$title,
      propagation = event$propagation, hidden = FALSE,
      color = "black"
    )
  }
  
  ### Delete Edge ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$edges) > 0)) {
    edges <- data.table(graph_data_return$edges)
    for (del_edge in event$edges) {
      graph_data_return$edges <- edges[id != del_edge]
    }
  }
  
  ### Add Node ---------------------------------------------------------------
  if (event$cmd == "addNode") {
    graph_data_return$nodes <- add_node(graph_data_return$nodes,
                                        id = event$id,
                                        label = event$label,
                                        title = events$title,
                                        shape = events$shape,
                                        Test = events$Test,
                                        weight = events$weight,
                                        pvalue = events$pvalues
    )
  }
  ### Edit Node --------------------------------------------------------------
  if (event$cmd == "editNode") {
    nodes <- data.table(graph_data_return$nodes)
    nodes[id == event$id, label := event$label,
          title := events$title,shape := events$shape,
          Test := events$Test,weight := events$weight,
          pvalue := events$pvalues]
    graph_data_return$nodes <- nodes
  }
  
  ### Delete Node ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
    nodes <- data.table(graph_data_return$nodes)
    for (del_node in event$nodes) {
      graph_data_return$nodes <- nodes[id != del_node]
    }
  }
  
  graph_data_return
}

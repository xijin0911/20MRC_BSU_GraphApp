modify_visNetwork <- function(event, graphdata_visNetwork, addNull = FALSE) {
  # CMD Check compatibility section
  `:=` <- NULL
  . <- NULL
  label <- NULL
  
  # End CMD compatibility section
  graphdata_visNetwork_return <- graphdata_visNetwork
  ### Edit Edge --------------------------------------------------------------
  if (event$cmd == "editEdge") {
    changed_id <- event$id
    
    angle <- if (event$type == "null") pi else 2 * pi
    
    edges <- data.table(
      graphdata_visNetwork_return$edges
    )
    
    edges[
      id == changed_id,
      c(
        "label", "from", "to", "title", "propagation"
      ) :=
        .(
          "", event$from, event$to, event$title, event$propagation,
        )
      ]
    
    graphdata_visNetwork_return$edges <- edges
  }
  
  ### Add Edge ---------------------------------------------------------------
  if (event$cmd == "addEdge") {
    graphdata_visNetwork_return$edges <- add_edge(
      edgedf = graphdata_visNetwork_return$edges, id = event$id,
      label = "std | 10", to = event$to, from = event$from,
      title = event$title, propagation = event$title,
      hidden = FALSE,
      color = "black"
    )
  }
  
  ### Delete Edge ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$edges) > 0)) {
    edges <- data.table(graphdata_visNetwork_return$edges)
    for (del_edge in event$edges) {
      graphdata_visNetwork_return$edges <- edges[id != del_edge]
    }
  }
  
  ### Add Node ---------------------------------------------------------------
  if (event$cmd == "addNode") {
    graphdata_visNetwork_return$nodes <- add_node(graphdata_visNetwork_return$nodes,
                                                  id = event$id, label = event$label
    )
    
    # If addNull is true, add a recursive null edge
    if (addNull) {
      graphdata_visNetwork_return$edges <- add_null_edge(
        edgedf = graphdata_visNetwork_return$edges, nodeid = event$id
      )
    }
  }
  ### Edit Node --------------------------------------------------------------
  if (event$cmd == "editNode") {
    nodes <- data.table(graphdata_visNetwork_return$nodes)
    nodes[id == event$id, 
          c(
            "label", "title", "shape", "Test", "weight","pvalue"
          ) :=
            .(
              "", event$title, event$shape, event$Test, event$weight,event$pvalue,
            )
          ]
    graphdata_visNetwork_return$nodes <- nodes
  }
  
  ### Delete Node ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
    nodes <- data.table(graphdata_visNetwork_return$nodes)
    for (del_node in event$nodes) {
      graphdata_visNetwork_return$nodes <- nodes[id != del_node]
    }
  }
  
  graphdata_visNetwork_return
}

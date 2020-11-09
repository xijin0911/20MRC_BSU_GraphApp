modify_visNetwork <- function(event, graphdata_visNetwork, addNull = FALSE) {
  # CMD Check compatibility section
  `:=` <- NULL
  . <- NULL
  label <- NULL
  
  # End CMD compatibility section
  graphdata_visNetwork_return <- graphdata_visNetwork
  if (!is.null(event$type)) {
    event$type <- tolower(event$type)
    # if (!(event$type %in% c("null", "std", "up", "down", "abs"))) {
    #   warning("Invalid 'type' parameter, returning unchanged data.")
    #   return(graphdata_visNetwork)
    # }
  }
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
        "id","label","from","to","title","propagation",
        # "label", "type", "parameter", "penalty", "K", "a", "min", "max",
        # "selfReference.angle", "selfReference.size"
      ) :=
        .(
          "", event$label, event$from, event$to, event$title,
          event$propagation, angle, 40
        )
      ]
    
    graphdata_visNetwork_return$edges <- edges
  }
  
  ### Add Edge ---------------------------------------------------------------
  if (event$cmd == "addEdge") {
    graphdata_visNetwork_return$edges <- add_edge(
      edgedf = graphdata_visNetwork_return$edges, id = event$id,
      label = "std", to = event$to, from = event$from,
      title = "std", propagation = "0", hidden = FALSE,
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
    nodes[id == event$id, label := event$label]
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

generate_graph <- function(graph_data,
                           Weighting_Strategy,
                           num_hypotheses){
  library(visNetwork)
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
  graph_data = list(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  if(Weighting_Strategy == "Specify ..."){
    netplot <-  visNetwork(graph_data$nodes, graph_data$edges,
                           width="100%", height="800px") %>%
      visExport() %>%
      visEdges(arrows = 'to') %>%
      visOptions(manipulation = list(enabled = T,
                                     editEdgeCols = c("propagation"),
                                     editNodeCols = c("Test", "weight", "pvalue")
      )) %>%
      visInteraction(navigationButtons = TRUE,hideEdgesOnDrag = TRUE,
                     dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
  }else
  if(Weighting_Strategy == "Bonferroni-Holm procedure"){
    num <- num_hypotheses
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
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
    edges$label <- round(edges$propagation,digits = 2)
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
      visLayout(randomSeed = 3)
  }else
  if(Weighting_Strategy == "Fixed sequence test"){
    num <- num_hypotheses
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
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$label <- round(edges$propagation,digits = 2)
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
  }else
  if(Weighting_Strategy == "Fallback procedure"){
    num <- num_hypotheses
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
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$label <- round(edges$propagation,digits = 2)
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
  netplot
}

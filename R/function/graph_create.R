

# the theme for the Grobtable in the final graph in Example Tab
grobtheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.9)
              # bg_params=list(alpha = rep(c(0.5,0.8), each=3))
              ),
  colhead = list(fg_params=list(cex = 0.9)),
  rowhead = list(fg_params=list(cex = 0.9)))





node_create <- function(num, method = "Specify ..."){
  names <- as.matrix(lapply(1:num, function(i) {paste0("H", i)}))
  if(method == "Specify ..."){
    nodes <- data.frame(id=character(),
                        label=character(),
                        title = character(),
                        shape = character(),
                        Test=character(),
                        weight=numeric(),
                        pvalue=numeric())
  }
  if(method == "Bonferroni-Holm procedure"){
    nodes <- data.frame(id=(names),
                        label=(names),
                        title = title_create(num,names),
                        shape = "circle",
                        Test=names,
                        weight=round(rep(1/num,num),digits = 2),
                        pvalue=rep(0.01,num),
                        stringsAsFactors=FALSE)
  }
  if(method == "Fixed sequence test"){
    nodes <- data.frame(id=(names),
                        label=(names),
                        title = title_create(num,names),
                        shape = "circle",
                        Test=(names),
                        weight=round(c(1,rep(0,num-1)),digits = 2),
                        pvalue=rep(0.01,num),
                        stringsAsFactors=FALSE)
  }
  if(method == "Fallback procedure"){
    nodes <- data.frame(id=(names),
                        label=(names),
                        title = title_create(num,names),
                        shape = "circle",
                        Test=names,
                        weight=round(rep(1/num,num),digits = 2),
                        pvalue=rep(0.01,num),
                        stringsAsFactors=FALSE)
  }
  return(nodes)
}

edge_create <- function(num, method = "Specify ..."){
  names <- as.matrix(lapply(1:num, function(i) {paste0("H", i)}))
  if(method == "Specify ..."){
    edges <- data.frame(from = character(),
                        to = character(),
                        title = character(),
                        label = character(),
                        propagation = numeric())
  }
  if(method == "Bonferroni-Holm procedure"){
    df <- (1-diag(num))/(num-1)
    rownames(df) <- names
    colnames(df) <- names
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
    edges$label <- round(edges$propagation,digits = 2)
    edges$label <- as.character(edges$label)
  }
  if(method == "Fixed sequence test"){
    df <- matrix(0, nrow = num, ncol = num)
    rownames(df) <- names
    colnames(df) <- names
    for (i in 1:(num-1)){
      df[i,i+1] <- 1
    }
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$label <- round(edges$propagation,digits = 2)
    edges$label <- as.character(edges$label)
    edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
  }
  if(method == "Fallback procedure"){
    df <- matrix(0, nrow = num, ncol = num)
    rownames(df) <- names
    colnames(df) <- names
    for (i in 1:(num-1)){
      df[i,i+1] <- 1
    }
    edges <- reshape2::melt(df)
    colnames(edges) <- c("from","to","propagation")
    edges <- edges[which(edges$propagation!=0),]
    edges$label <- round(edges$propagation,digits = 2)
    edges$label <- as.character(edges$label)
    edges$title <- paste0(edges$from, " -> ",edges$to, ":","<br>",edges$propagation)
  }
  return(edges)
}


title_create <- function(num,names){
  tit <- lapply(1:num, function(i) {
    paste(paste0(names[i],":"),
          paste0("weight= ", round(round(rep(1/num,num),digits = 2)[i],digits = 2)),
          paste0("p-value= ", rep(0.01,num)[i]),sep="<br/>")
  })
  as.matrix(tit)
}


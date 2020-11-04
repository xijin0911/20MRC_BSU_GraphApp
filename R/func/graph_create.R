

node_create <- function(num, method = "Specify..."){
  names <- lapply(1:num, function(i) {paste0("H", i)})
  if(method == "Specify..."){
    nodes <- data.frame(id=character(),
               label=character(),
               title = character(),
               shape = character(),
               Test=character(),
               weight=numeric(),
               pvalue=numeric(),
               stringsAsFactors=FALSE)
  }
  if(method == "Bonferroni-Holm procedure"){
    nodes <- data.frame(id=as.matrix(names),
               label=as.matrix(names),
               title = lapply(1:num, function(i) {
                 paste(paste0(names[i],":"),
                       paste0("weight= ", round(round(rep(1/num,num),digits = 2)[i],digits = 2)),
                       paste0("p-value= ", rep(0.01,num)[i]),sep="<br/>")
               }),
               shape = "circle",
               Test=as.matrix(lapply(1:num, function(i) {paste0("H", i)})),
               weight=round(rep(1/num,num),digits = 2),
               pvalue=rep(0.01,num),
               stringsAsFactors=FALSE)
  }
  if(method == "Fixed sequence test"){
    nodes <- data.frame(id=as.matrix(names),
               label=as.matrix(names),
               title = lapply(1:num, function(i) {
                 paste(paste0(names[i],":"),
                       paste0("weight= ", round(round(rep(1/num,num),digits = 2)[i],digits = 2)),
                       paste0("p-value= ", rep(0.01,num)[i]),sep="<br/>")
               }),
               shape = "circle",
               Test=as.matrix(names),
               weight=round(c(1,rep(0,num-1)),digits = 2),
               pvalue=rep(0.01,num),
               stringsAsFactors=FALSE)
  }
  if(method == "Fallback procedure"){
    nodes <- data.frame(id=as.matrix(names),
               label=as.matrix(names),
               title = lapply(1:num, function(i) {
                 paste(paste0(names[i],":"),
                       paste0("weight= ", round(round(rep(1/num,num),digits = 2)[i],digits = 2)),
                       paste0("p-value= ", rep(0.01,num)[i]),sep="<br/>")
               }),
               shape = "circle",
               Test=as.matrix(lapply(1:num, function(i) {paste0("H", i)})),
               weight=round(rep(1/num,num),digits = 2),
               pvalue=rep(0.01,num),
               stringsAsFactors=FALSE)
  }
  return(nodes)
  }


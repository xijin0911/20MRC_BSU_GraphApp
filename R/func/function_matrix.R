

f2n <- function(ff){
  library(gsubfn)
  calc <- function(s) {
    x <- c(if (length(s) == 2) 0, as.numeric(s), 0:1)
    x[1] + x[2] / x[3]
  }
  sapply(strapplyc(ff, "\\d+"), calc)
}


dfcreate <- function(num,test="Bonferroni-Holm procedure"){
  df <- matrix(0,ncol=num,nrow=num)
  name <- as.matrix(lapply(1:num, function(i) {
    paste0("H", i)
  }))
  if(test == "Bonferroni-Holm procedure"){
    df <- (1-diag(num))/(num-1)
  }
  if(test == "Fixed sequence test"){
    for (i in 1:num){
      df[i-1,i] <-  1
    }
  }
  if(test == "Fallback procedure"){
    for (i in 1:num){
      df[i-1,i] <-  1
    }
  }
  if(test == "Simple successive procedure")  #has the fixed number of hypothesis 
  {
    df = rbind(c(0,0.5,0.5,0),
                c(0.5,0,0,0.5),
                c(0,1,0,0),
                c(1,0,0,0))
  }
  if(test == "Parallel gatekeeping procedure")  #has the fixed number of hypothesis 
  {
    df = rbind(c(0,0,0.5,0.5),
               c(0,0,0.5,0.5),
               c(0,0,0,1),
               c(0,0,1,0))
  }
  
  colnames(df) <- name
  rownames(df) <- name
  return(df)
}

wpcreat <- function(num,test="Bonferroni-Holm procedure"){
  name <- as.matrix(lapply(1:num, function(i) {
      paste0("H", i)
  }))
    if(test=="Bonferroni-Holm procedure"){
      weight = (rep(1/num,num))
      pvalues = rep(0.01,num)
    }
   if(test=="Fixed sequence test"){
     weight = c(1,rep(0,num-1))
     pvalues = rep(0.01,num)
    }
  if(test == "Fallback procedure"){
    weight = (rep(1/num,num))
    pvalues = rep(0.01,num)
  }
  if(test == "Simple successive procedure"){
    weight = c(0.5,0.5,0,0)
    pvalues = rep(0.01,num)
  }
  if(test == "Parallel gatekeeping procedure"){
    weight = c(0.5,0.5,0,0)
    pvalues = c(0.097,0.015,0.005,0.006)
  }
  re <- data.frame(
                   "weights" = weight,
                   "pvalues" = pvalues, check.names = FALSE)
  rownames(re) <- name
  return(as.matrix(re))
}



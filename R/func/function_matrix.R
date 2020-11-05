# function for 

dfcreate <- function(num){
  df <- matrix(0,ncol=num,nrow=num)
  for (i in 1:num){
    df[i-1,i] <-  1
  }
  return(df)
}

wpcreat <- function(num,test="Bonferroni-Holm procedure"){
  name <- as.matrix(lapply(1:num, function(i) {
      paste0("H", i)
  }))
    if(test=="Bonferroni-Holm procedure"){
      weight = round(rep(1/num,num),digits=2)
      pvalues = rep(0.01,num)
    }
   if(test=="Fixed sequence test"){
     weight = c(1,rep(0,num-1))
     pvalues = rep(0.01,num)
    }
  if(test == "Fallback procedure"){
    weight = round(rep(1/num,num),digits=2)
    pvalues = rep(0.01,num)
  }
  re <- data.frame("Hypotheses" =name,
                   "Weights" = weight,
                   "P-values" = pvalues)
  return(as.matrix(re))
}



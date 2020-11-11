# function for 

dfcreate <- function(num,test="Bonferroni-Holm procedure"){
  df <- matrix(0,ncol=num,nrow=num)
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
  if(test == "Simple successive procedure"){
    weight = c(0.5,0.5,0,0)
    pvalues = rep(0.01,num)
  }
  re <- data.frame("Hypotheses" =name,
                   "Weights" = weight,
                   "P-values" = pvalues)
  return(as.matrix(re))
}



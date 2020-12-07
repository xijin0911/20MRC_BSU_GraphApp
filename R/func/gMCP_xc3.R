#' simplification for gMCP function , no graph as inputs
#'
#' @param matrix 
#' @param weights 
#' @param pvalues 
#' @param alpha 
#' @param fweights 
#'
#' @return
#' @export
#'
#' @examples
#' matrix <- rbind(H11 = c(0, 0.5, 0, 0.5, 0, 0),
#' H21 = c(1/3, 0, 1/3, 0, 1/3, 0),
#' H31 = c(0, 0.5, 0, 0, 0, 0.5),
#' H12 = c(0, 1, 0, 0, 0, 0),
#' H22 = c(0.5, 0, 0.5, 0, 0, 0),
#' H32 = c(0, 1, 0, 0, 0, 0))
#' weights <-  c(1/3, 1/3, 1/3, 0, 0, 0)
#' pvalues <- c(0.1, 0.008,0.005,0.15, 0.04, 0.006)
#' gMCP_xc3(matrix,weights,
#'          pvalues, alpha = 0.05,fweights = F) 
gMCP_xc3 <- function(matrix,weights, 
                     # trasition matrix and weights instead of graph
                     pvalues, alpha = 0.05,
                     fweights = F) 
{
  library(gMCP)
  G <- matrix
  n <- ncol(G)   # number of hypotheses
  h <- numeric(n) 
  names(h) <- paste0("H", 1:n)  # name of hypotheses
  a <- alpha * weights    # initial weights
  # update
  crit <- 0
  while (crit == 0) {
    test <- (pvalues <= a)  
    if (any(test)) {
      rej <- which.max(test)
      h[rej] <- 1
      Gtemp <- matrix(0, n, n)
      for (i in 1:n) {    # update step
        a[i] <- a[i] + a[rej] * G[rej, i]  # weight update
        if (G[i, rej] * G[rej, i] < 1) {   # transition weight update
          for (j in 1:n) {
            Gtemp[i, j] <- (G[i, j] + G[i, rej] * G[rej, 
                                                    j])/(1 - G[i, rej] * G[rej, i])
          }
        }
        Gtemp[i, i] <- 0
      }
      G <- Gtemp
      G[rej, ] <- G[, rej] <- 0 
      a[rej] <- 0
    }
    else {
      crit <- 1
    }
  }
  # adjusted p-values 
  # adjPValues <- p.adjust(pvalues,method="holm")
  adjPValues <- generatePvals(matrix,
                              weights,pvalues,cr=diag(n)) 
  
  # rejetions
  h <- as.logical(h)
  names(h) <- paste0("H", 1:n)
  # weights
  if (fweights) {
    list(# pvalues = pvalues, 
      adjpvalues =  round(adjPValues,digits = 4), # more outputs
      alpha = alpha, rejected =  (h),  
      weights = a/alpha,
      G = G)  # more outputs
  }
  else {
    list(pvalues = pvalues,
         adjpvalues =  round(adjPValues,digits = 4), # more outputs
         alpha = alpha, rejected =  (h), 
         weights = a/alpha,
         G = G)  # more outputs
  }
}

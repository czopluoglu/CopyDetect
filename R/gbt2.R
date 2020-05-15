#'
#'An internal function to compute the generalized binomial index 
#'for nominal response data
#'
#'@param form a dataset with N rows and k items. should only include 
#'nominal response data
#'
#'@param ip NRM item parameter matrix (first k columns are discrimination and 
#'last k columns are intercept)
#'
#'@param pa a vector of two numbers, 1st number is the row number for suspected copier, 
#' and the 2nd number is the row number for suspected source
#'
#'@param resp.options available nominal response options
#' 
#'@param thetas a vector or latent ability estimates
#'
#' @return A list with the following components
#' \itemize{
#'   \item ability1
#'   \item ability2
#'   \item probabilities1
#'   \item probabilities2
#'   \item prob.match
#'   \item exact.prob.dist
#'   \item p.value
#' }
#' 
#'@noRd
#'

gbt2 <- function(form,ip,pa,resp.options,thetas) { #start internal function
  
  theta.est1 <- thetas[pa[1]]  
  theta.est2 <- thetas[pa[2]]
  
  obs.match <- length(which(form[pa[1],]==form[pa[2],]))                                       
  
  probabilities1 <- irtprob2(ability=theta.est1,item.param=ip)
  
  colnames(probabilities1) <- resp.options
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(probabilities1) <- row.names
  
  probabilities2 <- irtprob2(ability=theta.est2,item.param=ip)
  
  colnames(probabilities2) <- resp.options
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(probabilities2) <- row.names
  
  Pi <- c()
  for(i in 1:ncol(form)){ Pi[i]=sum(probabilities1[i,]*probabilities2[i,])}
  
  Qi <-1-Pi
  Cpi <- cumprod(Pi)
  I <- length(Pi)
  
  M <- matrix(1,(I + 1),I)
  M[1,] <- cumprod(Qi)
  for(o in 1:I) {
    M[(o+1),o]<- Cpi[o]
  }
  
  for(m in 2:(I+1)) {
    for(o in 2:I) {
      if(m <= o)
        M[m,o] <- Qi[o]* M[m,(o-1)]+Pi[o]* M[(m-1),(o-1)]
      else M[m,o] <- M[m,o]
    }
  }
  
  GBT.p.value <- sum(M[(obs.match+1):(I+1),I])
  
  matchings <- c("Prob. of 0 Match","Prob. of 1 Match");for(i in 2:ncol(form)){matchings<- c(matchings,paste("Prob. of ",i," Matches",sep="")) }
  prob.dist.match <- as.data.frame(cbind(matchings,round(M[,I],6)))
  
  return(list(ability1 = theta.est1,
              ability2 = theta.est2,
              probabilities1 = probabilities1,
              probabilities2 = probabilities2,
              prob.match=Pi,
              exact.prob.dist=prob.dist.match,
              p.value=GBT.p.value
  ))
  
}#end internal function
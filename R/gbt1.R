#'
#'An internal function to compute the generalized binomial index for dichotomous
#'response data
#'
#'@param form a dataset with N rows and k items. should only include dichotomous 
#'response data
#'
#'@param ip item parameter matrix with 3 columns. First column is discrimination, 
#'second column is difficulty, and third column is guessing 
#'
#' @param pa a vector of two numbers, 1st number is the row number for suspected copier, 
#' and the 2nd number is the row number for suspected source
#' 
#' @param thetas a vector or latent ability estimates
#'
#' @return A list with the following components
#' \itemize{
#'   \item ability1
#'   \item ability2
#'   \item prob.cor
#'   \item prob.match
#'   \item exact.prob.dist
#'   \item p.value
#' }
#' 
#'@noRd
#'

gbt1 <- function(form,ip,pa,thetas) { 
  
  theta.est1 <- thetas[pa[1],]    
  theta.est2 <- thetas[pa[2],]        
  
  common.missing <- which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE)
  form[pa[1],common.missing]=0
  form[pa[2],common.missing]=0
  
  obs.match <- length(which(form[pa[1],]==form[pa[2],]))                                       
  
  prob.cor <- cbind(irtprob1(param=ip,theta=theta.est1),
                    irtprob1(param=ip,theta=theta.est2)) #probability correct
  
  colnames(prob.cor) <- c(paste("Examinee ",pa[1],sep=""),
                          paste("Examinee ",pa[2],sep=""))	
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(prob.cor) <- row.names
  
  prob.incor <- 1-prob.cor #probability incorrect
  
  colnames(prob.incor) <- c(paste("Examinee ",pa[1],sep=""),
                            paste("Examinee ",pa[2],sep=""))	
  rownames(prob.incor) <- row.names
  
  
  Pi <- (prob.cor[,1]*prob.cor[,2])+(prob.incor[,1]*prob.incor[,2]) #probability of matching
  
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
  
  return(list(ability1=theta.est1,
              ability2=theta.est2,
              prob.cor = prob.cor,
              prob.match=Pi,
              exact.prob.dist=prob.dist.match,
              p.value=GBT.p.value
  ))
  
}

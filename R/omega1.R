#'
#'An internal function to compute the omega index for dichotomous
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
#'   \item exp.match
#'   \item sd.match
#'   \item W.value
#'   \item p.value
#' }
#' 
#'@noRd
#'
#'@importFrom stats pnorm

omega1 <- function(form,ip,pa,thetas) { #start internal function
  
  theta.est1 <- thetas[pa[1],]      
  
  common.missing <- which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE)
  form[pa[1],common.missing]=0
  form[pa[2],common.missing]=0
  
  obs.match <- length(which(form[pa[1],]==form[pa[2],]))
  
  
  prob.cor <- cbind(irtprob1(param=ip,theta=theta.est1),
                    1-irtprob1(param=ip,theta=theta.est1))
  
  colnames(prob.cor) <- c(paste("Probability Correct for Examinee ",pa[1],sep=""),
                          paste("Probability Incorrect for Examinee ",pa[1],sep=""))		
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(prob.cor) <- row.names
  
  pvec <- c(prob.cor[which(form[pa[2],]==1),1],
            (1-prob.cor[which(form[pa[2],]==0),1]),
            (1-prob.cor[which(is.na(form[pa[2],])==TRUE),1])
  )
  
  exp.match <- sum(pvec)
  sd.match  <- sqrt(sum(pvec*(1-pvec)))
  
  w.value <- (obs.match-exp.match)/sd.match 
  p.value <- pnorm(w.value,0,1,lower.tail=FALSE)
  
  return(list(ability1=theta.est1,obs.match=obs.match,
              exp.match=exp.match,
              sd.match=sd.match,
              W.value=w.value,
              p.value=p.value))
}
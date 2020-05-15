#'
#'An internal function to compute the M4 index for dichotomous
#'response data
#'
#'@param form a dataset with N rows and k items. should only include dichotomous 
#'response data
#'
#'@param ip item parameter matrix with 3 columns. First column is discrimination, 
#'second column is difficulty, and third column is guessing 
#'
#'@param pa a vector of two numbers, 1st number is the row number for suspected copier, 
#' and the 2nd number is the row number for suspected source
#' 
#'@param thetas a vector or latent ability estimates
#'
#' @return A list with the following components
#' \itemize{
#'   \item ability1
#'   \item ability2
#'   \item prob.cor
#'   \item prob.incorr.match
#'   \item exact.prob.dist
#'   \item p.value
#' }
#' 
#'@noRd
#'

M4_1 <- function(form,ip,pa,thetas) { #start internal function
  
  gtd <- function(P,Q,m,n) {
    
    R <- 1-(P+Q)
    I=length(P)
    
    rec <- vector("list",I+1)
    rec[[1]]=matrix(0,nrow=I+1,ncol=I+1)
    rec[[1]][1,1] <- 1
    for(k in 2:(I+1)){
      rec[[k]] = R[k-1]*rec[[k-1]]+
        rbind(0,P[k-1]*rec[[k-1]])[-(I+2),]+
        cbind(0,Q[k-1]*rec[[k-1]])[,-(I+2)]
    }
    
    for(k in 1:(I+1)){ rec[[k]]=t(rec[[k]])}
    
    upper <- matrix(nrow=I+1,ncol=I+1)
    for(x in 1:(I+1)){
      for(y in 1:(I+1)) {
        upper[x,y] = sum(rec[[I+1]][x:(I+1),y:(I+1)])
      }
    }
    
    prob.table <- expand.grid(0:I,0:I)
    colnames(prob.table) <- c("IncorrectMatch","CorrectMatch")
    prob.table <- prob.table[which(rowSums(prob.table)<=I),]
    prob.table <- prob.table[order(prob.table[,1]),]
    prob.table <- cbind(prob.table,0,0,0,0)
    prob.table[,3] <- I-(rowSums(prob.table[,1:2]))
    for(i in 1:(nrow(prob.table))){
      x=prob.table[i,1]
      y=prob.table[i,2]
      prob.table[i,4] <- upper[x+1,y+1]
      prob.table[i,5] <- rec[[I+1]][x+1,y+1]
    }
    
    for(i in 1:(nrow(prob.table))){
      r = prob.table[i,4]
      marked = which(prob.table[,4] <= r)
      prob.table[i,6] <- sum(prob.table[marked,5])
    }
    
    colnames(prob.table)[3:6] <- c("NonMatch","Upper",
                                   "Probability","TailProbability")
    p = prob.table[which(prob.table[,1]==n & prob.table[,2]==m),6]
    list(prob.table[,-4],p)
  }
  
  theta.est1 <- thetas[pa[1],]    
  theta.est2 <- thetas[pa[2],]        
  
  P1 <- irtprob1(param=ip,theta=theta.est1) 
  P2 <- irtprob1(param=ip,theta=theta.est2) 
  P <- P1*P2
  Q <- (1-P1)*(1-P2)
  m <- length(which(form[pa[1],]==1 & form[pa[2],]==1))                                     
  n <- length(which(form[pa[1],]==0 & form[pa[2],]==0))                                     
  m4 = gtd(P=P,Q=Q,m=m,n=n)
  
  return(list(ability1=theta.est1,
              ability2=theta.est2,
              prob.corr.match=P,
              prob.incorr.match=Q,			
              exact.prob.dist=m4[[1]],
              p.value=m4[[2]]
  ))
}


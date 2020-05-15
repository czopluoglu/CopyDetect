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
#'@key correct response option
#' 
#'@param resp.options available nominal response options
#' 
#'@param thetas a vector or latent ability estimates
#'
#' @return A list with the following components
#' \itemize{
#'   \item ability1
#'   \item ability2
#'   \item prob.corr.match
#'   \item prob.incorr.match
#'   \item exact.prob.dist
#'   \item p.value
#' }
#' 
#'@noRd
#'

M4_2 <- function(form,ip,pa,key,resp.options,thetas) { #start internal function
  
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
  
  probabilities1 <- irtprob2(ability=theta.est1,item.param=ip)
  
  colnames(probabilities1) <- resp.options
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(probabilities1) <- row.names
  
  probabilities2 <- irtprob2(ability=theta.est2,item.param=ip)
  
  colnames(probabilities2) <- resp.options
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(probabilities2) <- row.names
  
  P <- c()
  for(i in 1:nrow(ip)) { P[i] = probabilities1[i,key[i]]*probabilities2[i,key[i]] }
  
  Q <- c()
  for(i in 1:nrow(ip)) { Q[i] = sum(probabilities1[i,-key[i]]*probabilities2[i,-key[i]]) }
  
  m <- sum(form[pa[1],]==key & form[pa[2],]==key,na.rm=TRUE)
  
  n <- sum((form[pa[1],]==form[pa[2],]) & (form[pa[1],]!=key),na.rm=TRUE)
  
  m4 = gtd(P=P,Q=Q,m=m,n=n)
  
  return(list(ability1=theta.est1,
              ability2=theta.est2,
              prob.corr.match=P,
              prob.incorr.match=Q,			
              exact.prob.dist=m4[[1]],
              p.value=m4[[2]]
  ))
}

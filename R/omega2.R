#'
#'An internal function to compute the omega index for nominal response data
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
#'@param key correct response option  
#' 
#'@param thetas a vector or latent ability estimates
#'
#' @return A list with the following components
#' \itemize{
#'   \item ability1
#'   \item exp.match
#'   \item obs.match
#'   \item sd.match
#'   \item W.value
#'   \item p.value
#' }
#' 
#'@noRd
#'
#'
#'
#'

omega2 <- function(form,ip,pa,resp.options,key,thetas) { #start internal function
  
  theta.est1 <- thetas[pa[1]]
  
  obs.match <- length(which(form[pa[1],]==form[pa[2],]))                                     
  
  probabilities <- irtprob2(ability=theta.est1,item.param=ip)
  
  colnames(probabilities) <- resp.options
  row.names <- c("Item 1");for(i in 2:ncol(form)){row.names <- c(row.names,paste("Item ",i,sep="")) }
  rownames(probabilities) <- row.names
  
  miss.items <- which(form[pa[2],]=="NA")
  if(length(miss.items)==0) { miss.items <- which(is.na(form[pa[2], ]) == TRUE)}
  
  for(i in miss.items){ form[pa[2], i] = resp.options[which(resp.options != key[i])][which.max(probabilities[i,which(resp.options != key[i])])] }
  
  pvec <- c()
  for(i in 1:ncol(form)){ pvec[i]=probabilities[i,which(resp.options==form[pa[2],i])] }
  
  exp.match <- sum(pvec)
  sd.match  <- sqrt(sum(pvec*(1-pvec)))
  
  w.value <- (obs.match-exp.match)/sd.match 
  p.value <- pnorm(w.value,0,1,lower.tail=FALSE)
  
  return(list(ability1=theta.est1,
              exp.match=exp.match,
              obs.match=obs.match,
              sd.match=sd.match,
              W.value=w.value,
              p.value=p.value))
}#end internal function

#'
#'An internal function to compute the K variants for dichotomous
#'response data
#'
#'@param form a dataset with N rows and k items. should only include nominal
#'response data
#'
#'@param form2 a dataset with N rows and k items. should only include corresponding
#'dichotomous response data
#'
#'@param pa a vector of two numbers, 1st number is the row number for suspected copier, 
#' and the 2nd number is the row number for suspected source
#' 
#'@resp.options available response options 
#' 
#'@return A list with the following components
#' \itemize{
#'   \item mean.iden.incorrect
#'   \item weighted.iden.correct
#'   \item subgroups
#'   \item pred1
#'   \item pred2
#'   \item pred3
#'   \item pred4
#'   \item K1.index
#'   \item K2.index
#'   \item S1.index
#'   \item S2.index
#' }
#' 
#'@noRd


ks12 <- function(form,form2,pa,resp.options) { #start internal function
  
  options(warn=-1)
  subgroups <- vector("list",ncol(form)+1)
  
  for(j in 1:(ncol(form)+1)){
    subgroups.ind  <- which(rowSums(form2==0,na.rm=TRUE)==j-1)
    subgroups.ind  <- subgroups.ind[subgroups.ind!=pa[2]]
    subgroups[[j]] <- subgroups.ind
  }
  
  
  wc  <- sum(form2[pa[1],]==0,na.rm=TRUE) 
  qc  <- wc/ncol(form)
  ws  <- sum(form2[pa[2],]==0,na.rm=TRUE) 
  incorrect.items <- which(form2[pa[2],]==0)
  m <- length(which(form[pa[1],incorrect.items]==form[pa[2],incorrect.items]))
  cm <- which(form2[pa[1],]==1 & form2[pa[2],]==1)
  
  pr     <- c()
  prob   <- matrix(nrow=(ncol(form)+1),ncol=ncol(form))
  weight <- matrix(nrow=(ncol(form)+1),ncol=ncol(form))
  pj <- c()
  
  g=1/length(resp.options)
  d2=-(1+g)/g
  
  for(j in 1:(ncol(form)+1)){
    
    if(length(subgroups[[j]])!=0) {
      
      incorrect.items <- which(form2[pa[2],]==0)
      smatrix1 <- as.data.frame(matrix(rep(as.matrix(form[pa[2],incorrect.items]),length(subgroups[[j]])),nrow=length(subgroups[[j]]),byrow=TRUE))
      smatrix2 <- as.data.frame(matrix(rep(as.matrix(form2[pa[2],]),length(subgroups[[j]])),nrow=length(subgroups[[j]]),byrow=TRUE))
      emp.agg <- rowSums(form[subgroups[[j]],incorrect.items]==smatrix1,na.rm=TRUE)
      pr[j] = mean(emp.agg,na.rm=TRUE)/ws 
      prob[j,] <- colMeans((form2[subgroups[[j]],]==1)&(smatrix2==1),na.rm=TRUE)
      weight[j,] <- (((1+g)/(1-g))*exp(1))^(prob[j,]*d2)
      pj[j] <-  mean(((form2[subgroups[[j]],]==1 & smatrix2==1)*1)%*%t(t(weight[j,])),na.rm=TRUE)
      
    } else 
      
      if(length(subgroups[[j]])==0) {
        pr[j]=NA 
        pj[j]=NA
      }
  }
  
  
  Qrs  <- (0:ncol(form))/ncol(form)
  Qrs2 <- Qrs^2
  Qrs3 <- 0:ncol(form)
  mm <- ceiling(sum(weight[wc+1,cm],na.rm=TRUE))+m
  
  pred1 <- predict(lm(pr~1+Qrs))
  pred.1 <- c()
  for(i in 1:(ncol(form)+1)){ 
    if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred1)))!=0){
      pred.1[i]=pred1[which(as.character(1:(ncol(form)+1))[i]==names(pred1))]} else pred.1[i]=NA
  }
  pred2 <- predict(lm(pr~1+Qrs+Qrs2))
  pred.2 <- c()
  for(i in 1:(ncol(form)+1)){ 
    if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred2)))!=0){
      pred.2[i]=pred2[which(as.character(1:(ncol(form)+1))[i]==names(pred2))]} else pred.2[i]=NA
  }
  
  pred3 <- exp(predict(glm(ws*pr ~ Qrs3 ,family=poisson())))
  pred.3 <- c()
  for(i in 1:(ncol(form)+1)){ 
    if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred3)))!=0){
      pred.3[i]=pred3[which(as.character(1:(ncol(form)+1))[i]==names(pred3))]} else pred.3[i]=NA
  }
  pred4 <- exp(predict(glm(ws*pr+ceiling(pj) ~ Qrs3 ,family=poisson())))
  pred.4 <- c()
  for(i in 1:(ncol(form)+1)){ 
    if(length(which(as.character(1:(ncol(form)+1))[i]==names(pred4)))!=0){
      pred.4[i]=pred4[which(as.character(1:(ncol(form)+1))[i]==names(pred4))]} else pred.4[i]=NA
  }
  
  
  p1 <- pred.1[which(Qrs==qc)]
  p2 <- pred.2[which(Qrs==qc)]
  s1 <- pred.3[which(Qrs==qc)]
  s2 <- pred.4[which(Qrs==qc)]
  
  if(is.na(p1)!=TRUE & p1>=1)  { p1=.999 };if(is.na(p1)!=TRUE & p1<=0) { p1=.001 }
  if(is.na(p2)!=TRUE & p2>=1)  { p2=.999 };if(is.na(p2)!=TRUE & p2<=0) { p2=.001 }
  if(is.na(s1)!=TRUE & s1>=ws) { s1=ws }  ;if(is.na(s2)!=TRUE & s2>=ncol(form)) { s2=ncol(form) }  ;
  
  if(is.na(p1)!=TRUE) { k1.index=1-pbinom(m-1,ws,p1) } else k1.index=NA
  if(is.na(p2)!=TRUE) { k2.index=1-pbinom(m-1,ws,p2) } else k2.index=NA
  if(is.na(s1)!=TRUE) { s1.index= (1-ppois(m-1,s1)) - (1 - ppois(ws,s1))       } else s1.index=NA
  if(is.na(s2)!=TRUE) { s2.index= (1-ppois(mm-1,s2)) - (1 - ppois(ncol(form),s2))     } else s2.index=NA
  
  
  return(list(mean.iden.incorrect = pr*ws,
              weighted.iden.correct=pj,
              subgroups=subgroups,
              pred1 = pred.1*ws,
              pred2 = pred.2*ws,
              pred3 = pred.3,
              pred4 = pred.4,
              K1.index =k1.index,
              K2.index =k2.index,
              S1.index =s1.index,
              S2.index =s2.index))
  
}#end internal function

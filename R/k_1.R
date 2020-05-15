#'
#'An internal function to compute the K index for dichotomous
#'response data
#'
#'@param form a dataset with N rows and k items. should only include dichotomous 
#'response data

#'@param pa a vector of two numbers, 1st number is the row number for suspected copier, 
#' and the 2nd number is the row number for suspected source
#' 

#'@return A list with the following components
#' \itemize{
#'   \item subgroup
#'   \item emp.agg
#'   \item k.index
#' }
#' 
#'@noRd
#'@importFrom stats pbinom

k_1 <- function(form,pa) { #start internal function
  
  wc  <- sum(form[pa[1],]==0,na.rm=TRUE) 
  ws  <- sum(form[pa[2],]==0,na.rm=TRUE)
  
  m   <- length(which(form[pa[1],]==0 & form[pa[2],]==0))+
    length(which(is.na(form[pa[1],])==TRUE & is.na(form[pa[2],])==TRUE))  #number of identical incorrect responses between two vectors
  
  subgroup <- which((rowSums(form==0,na.rm=TRUE)==wc))
  if(length(which(subgroup==pa[2]))!=0){
    subgroup = subgroup[!subgroup %in% pa[2]]
  }
  if(length(subgroup)!=0) {
    
    smatrix <- as.data.frame(matrix(rep(as.matrix(form[pa[2],]),length(subgroup)),nrow=length(subgroup),byrow=TRUE))
    
    emp.agg <- rowSums((form[subgroup,]==0)&(smatrix==0),na.rm=TRUE)+
      rowSums((is.na(form[subgroup,])==TRUE & is.na(smatrix)==TRUE),na.rm=TRUE)
    p=mean(emp.agg)/ws 
    
  } else p=NA
  
  if(is.na(p)!=TRUE) { k.index=1-pbinom(m-1,ws,p) } else k.index=NA
  
  return(list(
    subgroups=subgroup,
    emp.agg=emp.agg,
    k.index=k.index
  ))
  
  
}#end internal function


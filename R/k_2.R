#'
#'An internal function to compute the K index for dichotomous
#'response data
#'
#'@param form a dataset with N rows and k items. should only include nominal 
#'response data
#'
#'@param form a dataset with N rows and k items. should only include corresponding
#'dichotomous response data
#'
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

k_2 <- function(form,form2,pa) { #start internal function
  
  
  wc  <- sum(form2[pa[1],]==0,na.rm=TRUE)                    #number-incorrect score for copier
  ws  <- sum(form2[pa[2],]==0,na.rm=TRUE)                    #number-incorrect score for source
  
  incorrect.items <- which(form2[pa[2],]==0)
  m <- length(which(form[pa[1],incorrect.items]==form[pa[2],incorrect.items]))
  
  subgroup <- which(rowSums(form2==0,na.rm=TRUE)==wc)
  if(length(which(subgroup==pa[2]))!=0){
    subgroup = subgroup[!subgroup %in% pa[2]]
  }
  
  if(length(subgroup)!=0) {
    
    incorrect.items <- which(form2[pa[2],]==0)
    smatrix <- as.data.frame(matrix(rep(as.matrix(form[pa[2],incorrect.items]),length(subgroup)),nrow=length(subgroup),byrow=TRUE))
    emp.agg <- rowSums(form[subgroup,incorrect.items]==smatrix,na.rm=TRUE)
    p = mean(emp.agg,na.rm=TRUE)/ws 
    
  } else p=NA
  
  if(is.na(p)!=TRUE) { k.index=1-pbinom(m-1,ws,p) } else k.index=NA
  
  return(list(
    subgroups=subgroup,
    emp.agg=emp.agg,
    k.index=k.index
  ))
  
  
}#end internal function


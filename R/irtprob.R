irtprob <- function(param,theta,d=1){
  return( param[,3] + (1 - param[,3])/( 1 + exp(-d*param[,1]*(theta-param[,2]))))
}

#'a small function to compute the probability for
#'1PL-2PL-3PL model
#'
#'@param param three column item parameter matrix
#'first column is discrimination, second column is difficulty,
#'and third column is guessing
#'
#'@param theta ability parameter
#'
#'@param d constant for normal ogive or logistic metric.
#'
#'@noRd

irtprob1 <- function(param,theta,d=1){
  return( param[,3] + (1 - param[,3])/( 1 + exp(-d*param[,1]*(theta-param[,2]))))
}

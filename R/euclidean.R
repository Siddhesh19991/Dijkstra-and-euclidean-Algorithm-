#' The Euclidean Algorithm
#' @description
#' the Euclidean algorithm is an efficient method for computing the greatest common divisor of two integers, the largest number that divides them both without a remainder.
#'
#' @param value1 A numeric scalar or integer value.
#' @param value2 A numeric scalar or integer value.
#'
#' @return the greatest common divisor of two numbers i.e value1 and value2
#' @export
#'
#' 
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <-
function(value1,value2){
  
  if((is.numeric(value1)!= TRUE | is.integer(value1)!=TRUE) & length(value1)!=1){
    stop()
  }
  
  if((is.numeric(value2)!= TRUE | is.integer(value2)!=TRUE) & length(value2)!=1){
    stop()
  }

  
  if(value1>value2){
    a<-value1
    b<-value2
  }else{
    a=value2
    b<-value1
  }
  
  count=0
  while(b!=0){
    c<-b
    b<-a%%b
    a<-c
    
    count<-count+1
  }
  return(a)
}

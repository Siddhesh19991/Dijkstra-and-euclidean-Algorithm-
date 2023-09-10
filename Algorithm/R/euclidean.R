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

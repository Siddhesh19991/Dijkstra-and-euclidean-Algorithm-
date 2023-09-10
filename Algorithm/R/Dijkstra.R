dijkstra<-function(graph,init_node){
  
  if(is.numeric(init_node)!=TRUE | length(init_node)!=1){
    stop()
  }
  
  if(is.data.frame(graph)!=TRUE | ncol(graph)!=3){   #Clarify if this check is enough for the data frame structure 
    stop()
  }
  
  visted<-c()
  unvisted<-unique(graph$v1)
  final<-c()
  
  for (i in 1:length(unvisted)){
    final[i]<-1000
  }
  
  final[which(init_node==unvisted)]<-0
  
  visted[1]<-init_node
  unvisted<- unvisted[unvisted !=init_node]
  
  first<-graph[2][graph[1]==init_node]
  weight<-graph[3][graph[1]==init_node]
  
  j<-1
  for(i in first){
    if(final[i]>weight[j]){
      final[i]<-weight[j]
    }
    j<-j+1
  }
  
  ongoing<-which(min(final[-visted])==final)
  
  l<-1
  while ((length(unvisted)>1)){
    #ongoing<-which(min(final[-visted])==final)
    
    #neighbors<-graph[,c(2,3)][graph[1]==ongoing]
    if(length(ongoing)>1){
      ongoing<-ongoing[1]
    }
    
    primary<-graph[graph[1]==ongoing,c(2,3)]
    primary<-subset(primary,!(v2 %in% visted))
    neighbors<-primary$v2
    weight<-primary$w
    
    j<-1
    for(k in neighbors){
      if(final[k]>(final[ongoing]+weight[j])){
        final[k]<-final[ongoing]+weight[j]
      }else{
        final[k]<-final[k]
      }
      j<-j+1
    }
    
    visted[l+1]<-ongoing
    unvisted<- unvisted[unvisted !=ongoing]
    
    l<-l+1
    
    ongoing<-primary[min(primary$w)==primary$w,1]
  }
  
  return(final)
}

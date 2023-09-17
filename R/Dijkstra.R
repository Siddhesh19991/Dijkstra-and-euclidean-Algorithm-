#' The Dijkstra Algorithm
#' 
#' @description
#' Dijkstra's algorithm is used to find the shortest path between the two mentioned vertices of a graph by applying the Greedy Algorithm as the basis of principle. For Example: Used to find the shortest between the destination to visit from your current location on a Google map.
#' 
#'
#' @param graph It is  a data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node It is the starting node and is a numeric scalar value. 
#'
#' @return The shortest path to every other node from the starting node as a vector.
#' @export 
#' 
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @references https://www.youtube.com/watch?v=pVfj6mxhdMw&t=365s
#'
#'
#'
dijkstra<-function(graph,init_node){
  
  if(is.numeric(init_node)!=TRUE | length(init_node)!=1){
    stop()
  }
  
  if(is.data.frame(graph)!=TRUE | ncol(graph)!=3){   #Clarify if this check is enough for the data frame structure 
    stop()
  }
  
  if(init_node>max(unique(graph$v1))){
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
    neighbors<-primary[["v2"]]
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

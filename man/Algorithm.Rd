\name{Algorithm}
\alias{Algorithm}
\alias{Algorithm}
\docType{package}
\title{
\packageTitle{Algorithm}
}
\description{
\packageDescription{Algorithm}
}
\details{

The package contains only 2 functions i.e euclidean() and dijkstra() and they can be called if you would like to implement the Euclidian and Dijkstras Algorithms respectively. Each of these functions require input variables to work. For more information on both the algorithms, please refer to the documentation for each of those functions. 

}
\author{
\packageAuthor{Algorithm}

Maintainer: \packageMaintainer{Algorithm}
}
\references{
The referenecs for each function can be found in the functions documentation. 
}
\keyword{ package }

\examples{
euclidean(123612, 13892347912)

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
}

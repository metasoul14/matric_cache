# Creating a function which will cache matric inverse 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
# Using objects with <<- to store values in global environment.  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
# function to get matrix input
  get<-function() x
# function  storing solved matrix using  <<- object assignment globally
  setmatrix<-function(solve) m<<- solve
# function calling stored matrix  
  getmatrix<-function() m
       list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
# Creating a secod function cacheSolve
# cacheSolve function with matrix input
cacheSolve <- function(x=matrix(), ...) {
#storing global matrix in object m   
  m<-x$getmatrix()
# if clause rectifying whether m is not null which means it has matrix stored
  if(!is.null(m)){
# cached data is returned with a quoted message written under commas
    message("getting cached data")
    return(m)
  }
#  if cached data is found null get function with input matrix is called from makeCacheMatrix function
  matrix<-x$get()
# matrix inverse using solvefunction is stored in object m
  m<-solve(matrix, ...)
# caching matrix inverse data by calling setmatrix function to store in object m globally using super assignment   
  x$setmatrix(m)
# cached data is returned  
  m
}


a<-makeCacheMatrix()
a$set(matrix(1:4,2,2)
cacheSolve(a)

#Creating a function which will cache matric inverse 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
#Using objects with <<- to store values in global environment.  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
Creating a secod function 
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}


a<-makeCacheMatrix()
a$set(matrix(1:4,2,2)
cacheSolve(a)

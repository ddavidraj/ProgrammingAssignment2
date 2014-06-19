## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a matrix and takes a matrix input
# steps to initalize
# create a matrix testmat <- matrix(c(1,2,3,4),nrow=2,ncol=2)
# a<-makeCacheMatrix()
# a$set(testmat) - Sets the matrix to workspace
# a$get gets the matrix
# setmatrix is called by Solve to cache the inverse to workspace
# 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
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






## Write a short comment describing this function
# cache Solve tries to get the matrix from workspace (ie) cached
# If cached inverse is available it returns the inverse
# Else compute the inverse for the first time and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## steps to test
# testmat <- matrix(c(1,2,3,4),nrow=2,ncol=2)
# a<-makeCacheMatrix()
# a$set(testmat)
# cacheSolve(a)
# cacheSolve(a) execution for second time fetches from cache


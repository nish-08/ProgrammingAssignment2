##Matrix inversion is usually a costly computation and there is some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following are a pair of functions that caches the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse. 
##It returns a list containing functions to - 1)Get the matrix 2)Set the matrix 3)Get the inverse  4)Set the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  invmatrix<-NULL
  
  set<-function(y)
  {
    x<<-y
    invmatrix<<-NULL
    
  }
  get<-function()x
  
  setinverse<-function(solve) invmatrix<<-solve
  
  
  
  getinverse<-function()invmatrix
  
  list(set=set,
       get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix<- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinverse(invmatrix)
  invmatrix
}

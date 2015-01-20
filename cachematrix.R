##  This function creates a special "matrix" object that can cache its inverse.
##  It contains a list of function:
##  'set' sets the value of a matrix
##  'get' gets the value of a matrix
##  'setInverse' sets the value of the inverse
##  'getInverse' gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the 'mCache' variable for caching.
  mCache<-NULL
  set<-function(y){
    x<<-y ## Assign 'y' to 'x' in the parent environment
    mCache<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) mCache<<-inverse
  ## Assign 'inverse' to 'mCache' in the parent environment
  getInverse<-function() mCache
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mCache<-x$getInverse()
        ## If the Matrix has been cached, retrieve it from the cache.
  if(!is.null(mCache)){
    message("getting cached data")
    return(mCache)
  }
        ## Else, computer the inverse with the 'solve' function.
  data<-x$get()
  mCache<-solve(data,...)
        ## And cache it to mCache
  x$setInverse(mCache)
  mCache
}

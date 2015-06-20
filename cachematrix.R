
## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix return an list object which contains four 
## functions (set, get, setinv, getinv). 


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  
  setinv <- function(invMat) m <<- invMat
  getinv <- function() m
  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function return the inverse of the special "matrix" 
## created by makeCacheMatrix. The inverse is computed with R function solve 
## or it is returned from cache (if the matrix has not changed and the inverse
## was saved with makeCacheMatrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}

# Creates a special matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
 n <- NULL
  set <- function(x=matrix()) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set=set, get=get, setinverse=setinverse, getinverse= getinverse)
}


## Computes the inverse of the special matrix (makeCacheMatrix)
# If the inverse is already calculated then cacheSolve should retrieve the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  if(!is.null(n)) {
    message('getting cached matrix')
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n      
        
}

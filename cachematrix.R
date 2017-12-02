##The makeCacheMatrix and cacheSolve functions allow for caching an inverted matrix
##and, to subsequently retrieve that inverted matrix from cache rather than recomputing it


## This is the makeCacheMatrix function. It allows for the creation of a special matrix
## whose value can be set, retrieved, cached and retrieved from cache

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function utlizes the makeCacheMatrix function to 
## check if the desired value is cached already, and if so, retrieve it from cache
## rather than recompute it

cacheSolve <- function(x, ...) {
  ## Check if the inverse is already cached using the get function from the makeCachedMatrix function      
  i <- x$getinv()
  
  ## if it is not null (i.e. there is a value), retrieve it and end the function
  if(!is.null(i)) {
    message("getting cached inverted matrix")
    return(i)
  }
  
  ##otherwise, compute the inverse and store it in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}



A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)


b<-makeCacheMatrix(A)

cacheSolve(b)














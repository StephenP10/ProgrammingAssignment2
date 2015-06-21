## The below are a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse and returns a list of assignment and retrieval 
#functions as a list
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setInvMatrix <- function(invMatrix) {m <<- invMatrix}
  getInvMatrix <- function() m
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setInvMatrix(m)
  m
}

#This part shows an example of how the functions above works
x <- matrix(rnorm(16, mean = 30, sd = 1))
dim(x) <- c(4,4)
xx <- makeCacheMatrix(x)
xx$get()
xx$getInvMatrix()
xx$setInvMatrix(solve(x))
xx$get()
xx$getInvMatrix()

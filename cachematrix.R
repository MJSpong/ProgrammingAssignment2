## Define a function whose argument is a matrix to be cached, and whose output is a list of four functions.   
## The set and get functions set and print the cached matrix. The setsolve function allows you to cache an 
## inverse matrix, and the getsolve function prints it.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Define a function whose argument is the output list of the makeCacheMatrix function. It checks to see
## if there is already a cached inverse matrix. If not, it retrieves the original cached matrix and calculates 
## the inverse. It puts the inverse matrix into the cache, and then prints it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

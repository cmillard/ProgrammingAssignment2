## Two functions
## makCacheMatrix creates and stores a matrix
##cacheSolve - returns a matrix that is the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
##Create and store the matrix
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() {
          x
     }
     setSolve <- function(solve) {
          s <<- solve
     }
     getSolve <- function() {
          s
     }
     list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of x
     cacheSolve <- function(x, ...) {
          s <- x$getSolve()
          if(!is.null(s)) {
               message("fetching cached data")
               return(s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setSolve(s)
          s
     }
}

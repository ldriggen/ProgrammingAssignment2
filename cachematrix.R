## File: cachematrix.R
## Author: Larry Riggen
## Creation Date: 2015-08-20
## File Contents: functions makeCacheMatrix and cacheSolve
## Purpose:
##
##  makeCacheMatrix and cacheSolve work in tandem to cache a matrix inverse and
##  and reduce overhead by being able to recall the solution.
##
##
##  Sample usage (note: the the second cacheSolve(b) execution returns the inverse from cache):
## ------------------------------------------------------
## > b<-makeCacheMatrix(matrix(c(1,3,1,2),nrow=2,ncol=2))
## > cacheSolve(b)
## ------------------------------------------------------
##    
##       [,1] [,2]
## [1,]   -2    1
## [2,]    3   -1
##-------------------------------------------------------
## > cacheSolve(b)
##-------------------------------------------------------
## [1] "getting cached data"
##      [,1] [,2]
## [1,]   -2    1
## [2,]    3   -1


## makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to:
##
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the matrix inverse
##  4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}





## cacheSolve function calculates the inverse of the special "vector" created with the makeCacheMatrix function above.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}





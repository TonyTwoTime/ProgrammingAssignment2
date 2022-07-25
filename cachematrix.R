## Put comments here that give an overall description of what your
## functions do
## The first function,"makeCacheMatrix", creates a matrix which contains other functions: set, 
## get, setinv, and getinv.The second function, "cacheSolve", works in conjunction with 
## "makeCacheMatrix" and cache's the inverse of the matrix created by "makeCacheMatrix".

## Write a short comment describing this function
## makeCacheMatrix is the empty vessel for the inverse of its created matrix can be stored 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                ## Sets i to NULL to clear previous caches
  set <- function(y) {                     ## Below are the setters & getters for this function
    x <<- y                                ## allowing the matrix or the inverse to be called
    i <<- NULL                             ## upon.
  }
  get <- function() x
  setinv <- function(solve) i <<- solve   
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## This function is used to store the inverse of the matrix created by
## "makeCacheMatrix" by storing the result of the "Solve" function
## with the "setinv" function created in "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}



##These functions cache the result of inverting a matrix so the inversion
##need not take up so much computer crunching when the inversion
##is needed often.



##This function takes a matrix and returns a list containing functions
##relevant to the inversion and a data member that will hold NULL or
##the inversion.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  getfunction <- function() {x}
  setinversefunction <- function(inv=NULL) {inverse <<- inv}
  getinversefunction <- function() {inverse}
  list(get = getfunction,
       setinv = setinversefunction,
       getinv = getinversefunction)
}


## This function checks if the inverted matrix has been chached
## If it has, it returns the cached, inverted matrix
## If it hasn't, it solves the inversion, stores it, and returns it

cacheSolve <- function(x) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

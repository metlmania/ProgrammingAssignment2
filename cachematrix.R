## These two functions can be used to create a "matrix" that "contains" a
## cache of its own inverse. These functions are adapted from the
## makeVector and cachemean() functions provided with the assigment as
## examples.


## makeCacheMatrix() takes as its one argument a matrix (assumed to be
## invertible) and returns a list of four functions that are used to
## store and retrieve the values of that matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL ## The inverse of the matrix x is stored in i
  
  set <- function(y) { ## set() can be used to set the value of the matrix
    ## object x that was originally an argument to makeCacheMatrix();
    ## currently this set() function is unused by cacheSolve()
    x <<- y  ## Change the value of the matrix x that was originally
      ## submitted to makeCacheMatrix() even though x is out of the scope
      ## of set()
    i <<- NULL ## Reset the value of the inverse to NULL
  }
  
  get <- function() x ## Return the value of the matrix
  
  setinverse <- function(inv) i <<- inv  ## Store the inverse of the matrix
    ## in i
  
  getinverse <- function() i ## Return the inverse of the matrix
  
  ## The list of functions that constitutes the object returned by
  ## makeCacheMatrix()
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The argument x below is an object created by makeCacheMatrix();
## cacheSolve returns the inverse of the matrix that was submitted as an
## argument to makeCacheMatrix() when x (a list of functions) was
## originally created. When cacheSolved() is called with x as an argument,
## cacheSolved() checks if the inverse of the matrix corresponding to x
## has already been calculated.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## Try to get the inverse "stored" in x
  
  if(!is.null(i)) { ## If the inverse exists, return it
    message("getting cached data")
    return(i)
  }
  
  ## If the inverse does not exist yet, calculate, store, and return the
    ## inverse
  data <- x$get() ## Retrieve the matrix corresponding to the object x
  i <- solve(data, ...) ## Calculate the inverse of that matrix
  x$setinverse(i) ## Set the inverse value in x so that that inverse
    ## doesn't have to be calculated again
  i ## Return the inverse
}
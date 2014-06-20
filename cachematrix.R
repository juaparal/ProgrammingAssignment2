## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                          ##set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                           ##get the value of the matrix
  setinverse <- function(inverse) m <<- inverse ##set the value of the inverse
  getinverse <- function() m                    ##get the value of the 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()   ##check if it's necessary to calculate the matrix inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)  ##calculate de inverse of the matrix in case it isn't
  x$setinverse(m)
  m
  
}

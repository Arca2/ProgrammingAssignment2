
##This function creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 

makeCacheMatrix <- function(x = matrix()) {
  #necessary definitions
  nv <- NULL
  set <- function(y){
    x <<- y
    nv <<- NULL
  }
  get <- function() {x} #get the value of the matrix
  setInverse <- function(inverse) {nv <<- inverse} 
  getInverse <- function(){ nv }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function  returns the inverse of the matrix that is 
# returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

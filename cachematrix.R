## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(data_matrix = matrix()) {
  stored_inverse <- NULL
  set <- function(y) {
    data_matrix <<- y
    stored_inverse <<- NULL
  }
  get <- function() data_matrix
  setinverse <- function(inverse) stored_inverse <<- inverse
  getinverse <- function() stored_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated the cache solve retrieves the matrix from the cache.
cacheSolve <- function(made_matrix, ...) {
  
  local_inverse <- made_matrix$getinverse()
  if(!is.null(local_inverse)) {
    message("getting cached data")
    return(local_inverse)
  }
  data <- made_matrix$get()
  local_inverse <- solve(data, ...)
  made_matrix$setinverse(local_inverse)
  local_inverse 
}

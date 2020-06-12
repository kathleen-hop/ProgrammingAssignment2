## Together, these functions calculate the inverse of a matrix and store it in the cache.
## Once the inverse is calculated, it will be stored in the vector of type makeCacheMatrix,
## and will not need to be calculated again. It can be easily retrieved.

## This function takes an input matrix and creates a vector of the format makeCacheMatrix.
## This creates places for the inverse to be retrieved and stored when used with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x <<- y
    n <<- NULL
  }
  get <- function(){x}
  setinv <- function(n.local){
    n <<- n.local
  }
  getinv <- function(){
    n
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function solves for the inverse of the matrix in a vector of format makeCacheMatrix
## and stores the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getinv()
  if (!is.null(n)){
    message("getting cached data")
    n
  }
  data <- x$get()
  n <- solve(data)
  x$setinv(n)
  n
}

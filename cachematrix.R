## Caching the Inverse of a Matrix
## There are four functions in makeCacheMatrix function: 
## set matrix, get matrix, set inverse of the matrix and get inverse of the matrix
## The makeCacheMatrix function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function inputs an argument returned by makeCacheMatrix
## It outputs the inverse of the matrix and checks whether it is stored in the cache
## If the value is cached, it returns the cached output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# https-github.com-rdpeng-ProgrammingAssignment2
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) minv <<- solve
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )
}
cacheSolve <- function(x, ...) {
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  minv
}

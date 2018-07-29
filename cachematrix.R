## makeCacheMatrix sets the value of of the matrix in the global environment.
## It also gets the matrix, allows you to set and get the inverse, by storing them as a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the value of the inverse of the matrix and sets it in the makeCacheMatrix list.
## If the inverse is not null in the list of the makeCacheMatrix, then it just returns the existing value
## without additional calculations, hence speeding the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


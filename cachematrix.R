## The function below, makeCacheMatrix, creates a "special" matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set <- function(y){ ## In this sequesnce we begin by setting the value of the matrix.
    x <<- y 
    m <<- NULL
  }
  get <- function() x ## We the input a get operator to obatin the value of the set matrix above.
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m  ## The process above is then repeated for the inverse value of the matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function then computes the inverse of the "special" matrix returned by the makeCacheMatrix above. 
## Furthermore, if the inverse has already been computed then the function will return the cached matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {  ## First portion determines if the inverse is needed to be calculated and if not simply returns cached data.
    message("retrieving cached data")
    return(m)
  }
  mydata <- x$get() ## If the cached matrix is invertible then it will calculate and return the inverse.
  m <- solve(mydata, ...)
  x$setinverse(m)
  m
}


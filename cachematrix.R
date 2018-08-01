

## This function calculates the inverse of a matrix 
## then caches it locally for faster retrievial later
makeCacheMatrix <- function(x = matrix()) {
  ## Initializes the inverse matrix
  im <- NULL
  
  ## Defining the method to set the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## Defining the method to get the matrix
  get <- function() x
  
  ## Defining the method to inverse and set that as the inverse matrix
  setinverse <- function(solve) im <<- solve
  ## Defining the method to get the inverse matrix
  getinverse <- function() im
  
  ## Returns a list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets the inverse matrix from "makeCacheMatrix"
## If it has been calculated and the matrix hasn't changed then
## return the cached inverse
cacheSolve <- function(x, ...) {
  ## Store the inverse matrix to m
  m <- x$getinverse()
  
  ## If the cahced matrix inverse is set then get it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from x 
  data <- x$get()
  
  # Get the inverse of x
  m <- solve(data, ...)
  
  # Sets the inverse of x
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}


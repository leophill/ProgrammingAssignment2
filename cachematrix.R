## The code caches the Inverse of a Matrix (which is assumed to be invertible) to help avoid
## the costly operation of computing the Inverse each time, especially if it was calculated
## during a previous run.
## A typical call to get or set the cached Inverse of Matrix MX is : cacheSolve(makeCacheMatrix(MX))

## makeCacheMatrix creates a special "Matrix", which is a list containing four main functions to
## get or set the value of the Matrix, to get or set the value of the Inverse Matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize "inv" Matrix to null
  inv <- NULL
  
  ## Function to set the value of Matrix "x" to that of Matrix "y".
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get the value of Matrix "x"
  get <- function() x
  
  ## Function to assign the value of the inverse Matrix to Matrix "inv"
  setinv <- function(inverse) inv <<- inverse
  
  ## Function to get the cached value of Matrix "inv"
  getinv <- function() inv
  
  ## Build a list of all four functions above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve first checks to see if an Inverse Matrix has already been calculated for the special
## Matrix returned by makeCacheMatrix function above. If so, then it retrieves the cached value.
## If not, it calculates the Inverse Matrix of the special Matrix using the "solve" function.

cacheSolve <- function(x, ...) {
  ## Return a Matrix that is the Inverse of 'x'
  
  ## Get cached value for the Inverse of Matrix "x" if any
  inv <- x$getinv()
  
  ## Check and return the cached value if the Inverse of "x" already exists in the cache 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## No cached data. Save and find the inverse of Matrix "x" using "solve" function
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Store newly computed value into the cache
  x$setinv(inv)
  
  ## Return the value of the Inverse of Matrix "x"
  inv
}

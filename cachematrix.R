# The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  i <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    # Assign the input matrix 'y' to 'x' in the parent environment
    x <<- y
    # Reset the inverse property since the matrix is updated
    i <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Method to get the inverse of the matrix
  getinverse <- function() i
  
  # Return a list of methods to interact with the cached matrix and its inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse of the matrix
  i <- x$getinverse()
  
  # If the inverse is already cached, return it with a message
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If the inverse is not cached, calculate it
  data <- x$get() # Get the matrix data
  i <- solve(data, ...) # Compute the inverse of the matrix
  
  # Cache the newly calculated inverse
  x$setinverse(i)
  
  # Return the inverse
  i
}

# Example usage:
# Define a 2x2 matrix
B <- matrix(c(1, 2, 3, 4), 2, 2)

# Create a special matrix object that can cache its inverse
B1 <- makeCacheMatrix(B)

# Compute and cache the inverse of the matrix for the first time
cacheSolve(B1)

# Retrieve the cached inverse in subsequent calls
cacheSolve(B1)

## Takes the matrix as input and caches it
## Sets basic functions to help us use it later
# And returns them as a list

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL ## variable for the inverse matrix
  setMatrix<-function(y){   # Function to set the matrix value
    x<<-y  # cache the matrix (assigns y from parent environment)
    m<<-NULL # sets m in the parent environments to NULL
  }
  
  getMatrix<-function() x  # Get the matrix cached with setMatrix
  setInverse<-function(mat) m<<- mat  # save the cached value of inverse matrix in m
  getInverse<-function() m  # Get the saved value of inverse matrix m
  
  # list for the for functions  
  list (setMatrix=setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Returns the inverse matrix of the matrix
# Returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if (!is.null(m)){ #This means this function has already calculated the inverse
    message("Here comes inverse matrix (cached)!")
    return(m)
  }
  #If not, it calculates it now
  mat<-x$getMatrix() # Takes value of the input matrix
  x$setMatrix(mat) # Caches it
  m<-solve(mat,...) # Computes the inverse matrix
  x$setInverse(m) # Caches it
  m #returns the inverse matrix
}

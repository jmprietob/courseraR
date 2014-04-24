# makeCacheMatrix creates a "matrix". This "matrix" is a list of four functions:
# 1. setMatrix the value of the matrix
# 2. getMatrix the value of the matrix
# 3. setInverse the value of the matrix inverse
# 4. getInverse the value of the matrix inverse
makeCacheMatrix <- function(matriz = matrix()) {
  inversa <- NULL
  
  setMatrix <- function(ponerMatriz) {
    matriz <<- ponerMatriz
    inversa <<- NULL
  }
  
  getMatrix <- function() {
	matriz
	}
  
  setInverse <- function(ponerInversa) {
	inversa <<- ponerInversa
	}
  
  getInverse <- function() {
	inversa
	}
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve returns the inverse of the special "matrix" created by
# makeCacheMatrix.
# If the inverse has already been computed, it
# returns the cached inverse. 
# Otherwise, the matrix inverse is computed
# and stored in the cache using setInverse.
cacheSolve <- function(cachematrix, ...) {
		## Return a matrix that is the inverse of 'x' 
  inversa <- cachematrix$getInverse()
  
  if (!is.null(inversa)){
    message("Using cached inverse")
    return(inversa)
  }
  
  localMatrix <- cachematrix$getMatrix()
  inversa <- solve(localMatrix)
  cachematrix$setInverse(inversa)
  
  inversa
}
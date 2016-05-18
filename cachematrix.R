## Below are two functions that are used to create a special object that stores a matrix
## and caches its inverse if the stored matrix is square and invertible


## Creates a special "matrix"  object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mtrx <- NULL
  
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  
  get <- function() x
  
  setinvmatrix <- function(invmatrix) mtrx <<- invmatrix
  
  getinvmatrix <- function() mtrx
  
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
  
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinvmatrix()
  
  if(!is.null(im)) {
          message("getting cached data")
          return(im)
  }
  
  data <- x$get()
  
  #Check if matrix is not invertible
  if(det(data) == 0) {
          stop("matrix is not invertible")
  }
  
  im <- solve(data, ...)
  
  x$setinvmatrix(im)
  
  return(im)
  
}

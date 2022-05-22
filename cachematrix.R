##  Assignment: Caching the Inverse of a Matrix
## 

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL
  
  get <- function() x #return matrix
  
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  
  setinv <- function(inv_m) m_inv <<- inv_m
  
  getinv <- getmean <- function() m_inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m_inv <- x$getinv()
  
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  
  data <- x$get()
  
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
  
}

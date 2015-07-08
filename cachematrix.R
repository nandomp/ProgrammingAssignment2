#The first function, makeCacheMatrix creates a special "matrix", which is really a
#list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse


makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  set <- function(y) {
    mtx <<- y
    inv <<- NULL
  }
  get <- function() mtx
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#The following function calculates the inverse of the special "matrix" created with 
#the above function. However, it first #checks to see if the mean has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, #it calculates the mean of the data and sets the value of the mean in the
#cache via the setmean function.

cacheSolve <- function(mtx, ...) {
  inv <- mtx$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mtx$get()
  inv <- solve(data)
  mtx$setinv(inv)
  inv
}



##TEST EXAMPLE##
# source("CacheMatrix.R")
# m<-makeCacheMatrix()
# m$set(matrix(c(2,4,7,6),c(2,2)))
# cacheSolve(m)
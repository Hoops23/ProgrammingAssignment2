# Matrix inversion can be a time-consuming computation. 
# Because of that, it makes sense to cache the inverse of a matrix 
# so that we don't have to compute it every time.
# The following two functions can be used to compute and
# cache the inverse of an invertible matrix.

# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverted matrix
# 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# The following function calculates the inverse of the special 
# "matrix" created with the above function. However, it first 
# checks to see if the inverse has already been calculated. 
# If so, it gets the inverted matrix from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the 
# matrix and sets the inverted matrix in the cache via the 
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}

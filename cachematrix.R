## Two functions used to invert and return a square matrix while 
## potentially saving time by caching the matrix inverse for future calls. 
## makeCacheMatrix: Called first, takes the square matrix, 
## makes a list containing a function to set and get the matrix, 
## and set and get the matrix inverse.
## cacheSolve: Called second, use the name of list assigned from makeCacheMatrix.
## If already inverted matrix, gives message and returns cached matrix inverse.
## Otherwise, takes inverse, caches and returns it.


## makeCacheMatrix: given matrix, make list with matrix and either cached inverse,
## or indicate inverse not taken.
makeCacheMatrix <- function(x = matrix()) {
   	m <- NULL
   	set <- function(y) {
   				 x <<- y
   				 m <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## cacheSolve: given list, retrieve cached matrix inverse,
## or calculate matrix inverse and cache it. 
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
    		 message("getting cached matrix inverse")
    		 return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)	
     m
}

#makeMatrix(x) creates a special matrix, that caches an inverse matrix, then 
#cachesolve(x) is used to output cached inverse matrix or count one

#makeMatrix makes special matrix
makeMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s) #returns cached matrix
      }
      data <- x$get()
      s <- solve(data, ...) #solves the matrix
      x$setsolve(s) #saves solved matrix to the special matrix
      s #returns solved matrix
}
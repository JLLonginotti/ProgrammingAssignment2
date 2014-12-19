## makeCacheMatrix creates the cache stores so they are available
## once cacheSolve is used.
## This function sets the working matrix and stores it (set),
## allows for the matrix to be retrieved (get),
## creates the cache for the solved matrix (setinv),
## allows for the solved matrix to be retrieved (getinv)

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                                           #initiate inverse matrix
      
      set <- function(y){                                 #set matrix
            x <<- y
            inv <<- NULL
      }
      get <- function() x                                  #get matrix
      
      setinv <- function(solve){                          #set inverse
            inv <<- solve
      }
      getinv <- function() inv                               #get inverse
      
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Solve for the inverse of the matrix and cahce, if not already
cacheSolve <- function(x, ...) {
      inv <- x$getinv()                         #extract inverse from cache list
      
      if (!is.null(inv)) {                      #if inverse already exists, return
            return(inv)
      }

      #if not already solved, retrieve from cache, calculate using solve, cache, and return
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinv(inv)
      inv
}

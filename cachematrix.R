## Coursera R Programming Course, Programming Assigment #2:

## Note: some variables names are in spanish.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

      ## Set inverso as NULL.
      inverso <- NULL
            ## This sub_variable is to assign the value of the matrix to x.
            set <- function(a) {
                  x <<- a
                  inverso <- NULL
            }
            ## This sub_variable is to retrive the value of x.
            get <- function() x
            ## This sub_variable is to assign the matrix inversion of x to "inverso".
            set_inverso <- function() inverso <<- solve(x)
            ## This sub_variable is to retrive the value of "inverso".
            get_inverso <- function() inverso
            ## This is the list of sub_variables of "makeCacheMatrix".
            list(set = set, get = get, set_inverso = set_inverso, 
                 get_inverso = get_inverso)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## If inverso is not NULL then bring the value of inverso from the cache.
      inverso <- x$get_inverso()
            if(!is.null(inverso)){
                  message("Getting cached data.")
                  return(inverso)
            }
      ## If inverso value was NULL then recalculate the matrix inverse value.
      matriz <- x$get()
      inverso <- solve(matriz, ...)
      x$set_inverso(inverso)
      inverso
}

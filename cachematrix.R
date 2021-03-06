## This assignment uses the Scheme-like scoping rules of R to create a special object in
## the R environment to preserve a computation state and and its assiciated manipulations

## makeCacheMatrix creates a special matrix that is really an R list that has 4 elements. 
##  These elements of the list are used to manipulate a matrix within the environment,
## and store the inverse matrix
##
## Note that all matricies are assumed to be square and invertable - no error checking done

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # the initial inverse matrix does not yet exist
  
  set <- function(y) { # Here we "set" the value of a matrix
    x <<- y
    i <<- NULL
  }

  get <- function() x # Here we return the matrix
  
  setInverse <- function(inverse) i <<- inverse # this sets the inverse
  
  getInverse <- function() i # this returns the inverse
  
  list(set = set, # the magic sauce. we actually have a list of the 4 operations
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the given "matrix"
## If the inverse has already been calculated then get from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse() # see what's there
  
  if (!is.null(i)) { # we've already done it
    message("getting cached data")
    return(i)
  }
  
  # not already cached, so let's compute it
  mat <- x$get() # get the matrix that is to be inverted
  i <- solve(mat, ...) # invert it
  x$setInverse(i) # cache it now
  i # return it back to the user
}


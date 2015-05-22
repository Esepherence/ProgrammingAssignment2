## Functions to cache a matrix and it's inverse in a function list 
## and to call this list in order to retrieve cahced results 
## or solve for the inverse if not available

## Take input matrix and return list of functions for gloabal use by cacheSolve

makeCacheMatrix <- function(x = matrix()) { #Declare makeMatrix Function, input x, creates function list for matrix x
  inv <- NULL
  set <- function(y) {  #Define local set function, input y
    x <<- y   #Set parent x equal to input y
    inv <<- NULL #Set parent inv = NULL when set called
  }
  get <- function() x #Define local get function, returns x
  setsolve <- function(solve) inv <<- solve   #Define local setsolve function, input solve, sets inv to solve
  getsolve <- function() inv  #Set local getsolve function, returns inv
  return(list(set = set, get = get, #Build list of functions for return
       setsolve = setsolve,
       getsolve = getsolve))
}


## Take input as output from makeCacheMatrix, check if matrix inverse is in cache
## or solve and cache matrix inverse, return inverse

cacheSolve <- function(x, ...) { #Declare cachesolve function, input list(x) and optional arguments
  inv <- x$getsolve()   #Set inv equal to output of getsolve() within list(x)
  if(!is.null(inv)) {   #If getsolve is NOT null 
    message("getting cached data")
    return(inv)   #Then return inv
  }
  message("Solving for matrix inverse")
  data <- x$get()   #If is NULL then get the matrix from list(x)
  inv <- solve(data, ...)   #solve for inverse and set inv
  x$setsolve(inv)   #Set solved result within cached list(x)
  return(inv)  #Return solution inv
}

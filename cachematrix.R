## Put comments here that give an overall description of what your
## functions do

## Creating special matrix object so that it can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    u <- NULL
    set <- function(y){
    x <<- y
    u <<- NULL
  }
    get <- function() x
    setInverse <- function(j) u <<- j
    getInverse <- function() u
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##  computing the inverse

cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
    u <- x$getInverse()
    if(!is.null(u))
      {
          message("getting cached data")
          return(u)
      }
    data <- x$get()
    u <- solve(data)
    x$setInverse(u)
    u      
}
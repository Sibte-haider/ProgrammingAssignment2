## Cache the inverse of a matrix
##Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
  {
  i <- NULL
  set <- function(y)
    {
    x <<- y
    i <<- NULL
    }
  
  ##getter/setter for the matrix
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  }

##Compute the inverse of the "matrix"

cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i))
    {
    message("getting cached data")
    return(i)
    }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
  }

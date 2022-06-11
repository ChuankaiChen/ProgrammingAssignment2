## The functions are able to cache results computation-heavy operations, 
##like computing the inverse of a matrix, so that they can be retrieve when it si necessary.

## compute the inverse of a matrix and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set default inverse value i
  i <- NULL
  #create set function to assign y to x in the outmost environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # create get function to output x
  get <- function() x
  
  #create set inverse function to assign inverse value to i
  setinverse <- function(inverse) i <<- inverse
  
  #return i
  getinverse <- function() i
  
  #collect values as a list to return to the function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## check if there is a cached result, if not compute the inverse and cache the inverse

cacheinverse <- function(x, ...) {
  #check if the inverse has been computed and cached
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #else is not necessary here, becaseu return will stop the following code
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
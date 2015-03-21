## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix -> sets/gets the matrix, sets/gets the inverse matrix
## cacheSolve

## Write a short comment describing this function

##
makeCacheMatrix <- function(x = matrix()) {
  #store the inverse matrix value 
  inv <- NULL
  
  ##set the matrix   
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  ##get the matrix
  get <- function() x
  
  ##set the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ##get the inverse matrix, if it is null calculate and assign it         
  getinverse <- function() inv
  
  ##store the methods so they can be accessed using $
  list( get = get, set = set, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function
## cacheSolve either returns the cachedValue (i.e. the inverse of the matrix) 
## or if it has not been calculated before, it calculates it and then returns it
cacheSolve <- function(x, ...) {
  
  ## attempt to get cached value
  inv <- x$getinverse()
  
  ## return the value if it is not null i.e. it has been cached
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise compute it and return it
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)

  ## Return a matrix that is the inverse of 'x'
  inv
}

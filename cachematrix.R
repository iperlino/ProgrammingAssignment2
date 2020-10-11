## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               #I assume all matrix will be invertible
  set <- function(y){       #set value of the matrix
    x <<- y                 #double arrow assignment to have variables available 
    inv <<- NULL            # into in different levels
  }
  get <- function() (x)                             #get value of the matrix
  setInverse <- function(inverse) (inv <-- inverse) #set value of inverse matrix
  getInverse <- function() (inv)                    #get value of inverse matrix
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()     #inverses x and assigns to inv
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
  
        ## Return a matrix that is the inverse of 'x'


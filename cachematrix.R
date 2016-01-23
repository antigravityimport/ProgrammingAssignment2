## Put comments here that give an overall description of what your
## functions do

## First function, makeCacheMatrix(), creates a list of functions that allow store and retrieve matrix and store and retrieve 
## its inverse in the global envrionment (making those values "static")

## Second function, cacheSolve(), works with the oject returned by the first function and calculates (once)
## value of the inverse matric and stores it in the object 

## How to use:

## Create object with makeCacheMatrix -either by calling makeCacheMatrix or calling set on existing
## object makeCacheMatrix
## Then call cacheSolve to calculate inverse the first time or re-use previously calculated values on
## any subsequent call

## Test function runTest() below shows how to use functions and proves that second time calling cacheSolve does not
## take any time


runTest <-function() {
  set.seed(42)
  x<-matrix(runif(2000^2),2000,2000)
  mcM <- makeCacheMatrix(x)
  t0 <-proc.time()
  y <-cacheSolve(mcM)
  t1 <-proc.time()
  y2 <-cacheSolve(mcM)
  t2 <-proc.time()
  s <-sprintf('Ran first time, it took %f, ran second time, it took %f ',(t1[1]-t0[1]), (t2[1]-t1[1]))  
  s
}


## Write a short comment describing this function

## makeCacheMatrix takes matrix as an input parameter
## returns list of static operations on the matrix
## set, get, set inverse, get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function

## cacheSolve function takes results of application of makeCacheMatrix
## to a matrix and statically (once) caclulates inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Method to inverse the square invertible matrix
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
}

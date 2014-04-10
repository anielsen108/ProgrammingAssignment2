## This suite of functions comprises:
## 1) An object that can store a matrix and cache its inversion
## 2) A function that returns the cached inversion from such an object
## 3) A test harness to test the functions in this suite

## Define an object that 
## 1) Store a matrix
## 2) Cache its inversion
makeCacheMatrix <- function(x = matrix()) {

  ## initialize variable that will hold cached matrix inversion calculation
  m <- NULL
    
  ## 1) saves input matrix for later determination if inversion has been calculated
  ## 2) re-initializes variable holding cached matrix inversion calculation
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## returns original matrix data
  get <- function() x
  
  
  ## caches inversion calculation of matrix
  setinverse <- function(solve) m <<- solve
  
  ## returns cached inversion calculation of matrix
  getinverse <- function() m
  
  ## return is a list of the available functions of this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Purpose: 
##  cacheSolve caches the inversion calculation, 
##  and uses the cached inversion calculation if available
##  to save calculation time.
## inputs
##   x: a matrix
## returns: 
##  inverse of x
cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  
  ## try to get the cached inverse of the input matrix
  m <- x$getinverse()
  
  ## if succeeded, then return the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if failed, then get matrix, invert it, and cache it
  matrixToInvert <- x$get()
  m <- solve(matrixToInvert, ...)  
  x$setinverse(m)
  
  ## return the inverted matrix
  m
}

## Test Harness
testCacheSolve <- function() {
  
  ## create 2x2 matrix
  x<-c(1,4,5,3)
  dim(x) <-c(2,2)
  
  # create Cached Matrix object
  y<-makeCacheMatrix(x)
  
  print("original matrix")
  print(x)
  
  print("cached matrix object")
  print(y$get())
  
  cat("identical=",identical(x,y$get()),"\n")
  
  print("original matrix inversion")
  print(solve(x))
  
  print("cached matrix inversion")
  print(cacheSolve(y))
  
  cat("identical=",identical(solve(x),cacheSolve(y)),"\n")
  
}
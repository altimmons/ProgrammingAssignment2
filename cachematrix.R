## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
  #require(matrixcalc)
M <- NULL

  fset <- function(Y) {
              X <<- Y
              M <<- NULL
            }

  fget <- function() X
 
  fsetinv <- function(Minv) M <<- Minv
  
  fgetinv<- function() return(M)
  
  
list(set = fset, get = fget, setinv = fsetinv, getinv = fgetinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
   #makeCacheMatrix(x)
M <- x$getinv()
if(!is.null(M)){
  message("getting cached data")
  return(M)
  }
data <- x$get()
M <- solve(data)
x$setinv(M)
return(M)
}

###Andy Timmons
### Programming Assignment 2- R Programming 033
###10/25/15


## This creates a new object that is able to cache and store the inverse of the matrix.

##usage : A <- makeCacheMatrix(matrix)

# A$get() returns the matrix submitted
# A$set(matrix) sets A to a new matrix
# A$setinv() is an internal function and likely shouldnt be called
# A$getinv() returns the cached value of the inverse matrix of A$$matrix. 
#   -returns null if this has not been computed
#   -call cacheSolve to take advantage of the cached function and to compute a new inverse matrix.

makeCacheMatrix <- function(X = matrix()) {
  #require(matrixcalc)
M <- NULL


# function wrappers inside makeCacheMatrix --------------------------------


  fset <- function(Y) {
              X <<- Y
              M <<- NULL
            }

  fget <- function() X
 
  fsetinv <- function(Minv) M <<- Minv
  
  fgetinv<- function() return(M)
  
  
list(set = fset, get = fget, setinv = fsetinv, getinv = fgetinv)  #set the list of attributes that we can call,
#each attribute is actually a wrapper for a function
}

## This function checks to see if the inverse matrix solution exists, if so it returns that value
# if it does not exist, it calculates it, returns it and then stores it using the A$setinv() function



# Second Function- Cache Solve --------------------------------------------


cacheSolve <- function(input, ...) {
        ## Return a matrix that is the inverse of 'x
   #makeCacheMatrix(x)
M <- input$getinv()

#check for cached values and return out if found
if(!is.null(M)){
  message("getting cached data")
  return(M)
}

#otherwise, we'll run this, first get the information of the matrix we'll operate on
data <- input$get()  
#now solve the inverse matrix using the solve(matrix) command
M <- solve(data)
#now set the new value into our object
input$setinv(M)
#return the inverse matrix (same as above)
return(M)
}

#fin


## Functions below will enable efficient inversion of a matrix. 
## If the inverse already exists, the calculation of inverse will not
## be called thus realizing efficiency
## Two functions will realize this logic
## Function makeCacheMatrix will create a special object
## Function cacheSolve will calculate the inverse of the matrix presented to 



## First function
## An object will enclose a matrix, its inverse and utility functions
## to allow calculations

makeCacheMatrix  <- function(x = matrix()) {
  
  ## initialze a matrix to hold inverse of matrix
  
  inx <- matrix()
  
  ## Initiallize variable to hold matrix and its inverse
  ## in this function
  
  set <- function(y) {
    x <<- y
    inx <<- NULL                
  }
  
  ## Below function returns the non inversed matrix
  
  get <- function() x 
  
  ## Set value to the variable holding inverse of matrix
  
  setinverse <- function(inverse) inx <<- inverse
  
  ## returns the variable of the matrix
  
  getinverse <- function() inx
  
  list(set=set ,get=get , setinverse=setinverse, getinverse=getinverse)
  
  
}


## Second function will calculate inverse of a matrix only if it is not
## already calculated

cacheSolve <- function(x, ...) {
  
  ## get value of variable holding inverse of matrix
  
  i <- x$getinverse()
  
  ## check if inverse is already calculated
  ## if inverse is not calculated, the initialized 1 by 1 NA matrix will
  ## be present
  ## 
  if (nrow(i)==1&&ncol(i)==1&&is.na(i[1,1])) 
  {}
  else
  {
    ## Inverse is already available
    ## return already calculated 
    message ("getting catched data")
    return(i)
  }
  
  data <- x$get()
  
  y <- solve(data)
  
  x$setinverse(y)
  
  y
  ## Return a matrix that is the inverse of 'x'
}
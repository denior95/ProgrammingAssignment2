##This function creates a list that contains 4 member 
##functions: set, get, setinverse and getinverse
##it use the " <<- " operator to assign a value to an object 
##in an environment different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_1 <- NULL   ##inverse_1 is where the result of inversion is stored
  ## i begin by setting the inverse_1 to NULL 
  
  set <- function(y){
    ##the set function define a function to set the matrix x 
    ##to a new matrix y and reset the inverse_1 to NULL
    
    x <<- y
    inverse_1 <<- NULL
  }
  
  get <- function() x  ##the get function return the matrix x
  
  setinverse <- function(inverse){
    #this function will set "inverse" to "inverse_1"
    
    inverse_1 <<- inverse
  }
  
  getinverse <- function() inverse_1 ##the getinverse function return the inverse of x
  
  ##then, i returns the 'special matrix' containing all of 
  ##the functions defined above. So that i can use
  ## makeCacheMatrix object like these:
  ## x <- makeCacheMatrix(z_matrix)
  ## x$set(newmatrix) # to change matrix z_matrix to newmatrix
  ## x$get # to get the setted matrix
  ## x$setinverse # to set the inversed matrix
  ## x$getinverse # to get the inversed matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has notchanged), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inverse_2 <- x$getinverse() ##get the inversed matrix from object x
  
  ##test if the inverse of x has already been calculated
  
  if (!is.null(inverse_2)){
    
    ## get it from the cache. 
    
    message("getting cached data")
    
    ##and skips the computation with return function
    
    return(inverse_2)
  }
  
  ## otherwise,R will calculate the inverse of x
  
  data <- x$get() ## get the matrix object and assign it to "data" 
  
  ## calculate the inverse of the matrix x by using the solve function.
  
  inverse_2 <- solve(data, ...)
  
  ## sets the value of the inverse (which is inverse_2) in the cache via the setinverse
  ## function defined in MakeCacheMatrix function
  
  x$setinverse(inverse_2)
  
  ## return the inverse of the matrix x
  return(inverse_2)
}

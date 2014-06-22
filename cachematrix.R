## function to populate the vector of functions to set/get matrix object "x" and also set/get the inverse of original matrix
makeCacheMatrix <- function(x=matrix()){
  #initialize the inverse to null
  inverse <- NULL
  
  #function to set the matrix object and initialize inverse to null object
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  #function to retrieve the matrix object
  get <-function() x
  
  ##function to set the inverse of original matrix
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  #function to retrieve the inverse matrix object
  getInverse <-function() inverse
  
  #return the vector of all four functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

#function to demonstrate the caching of inverse matrix of the special vector makeCacheMatrix
cacheSolve <- function(x,...){

#retrieve the inverse matrix	
  inverse <- x$getInverse()
  #check whether it is already computed and return the computed inverse matrix if it exists
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  #else retrieve the data of matrix object
  matrixRaw <- x$get()
  #compute the inverse of the matrix
  inverse <- solve(matrixRaw)
  #set the inverse matrix object in the special vector
  x$setInverse(inverse)
  #return the computed inverse matrix
  inverse
  
}
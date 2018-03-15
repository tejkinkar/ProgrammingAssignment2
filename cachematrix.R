# function makeCacheMatrix will perform 4 operatons: 
#set the matrix (setMatrix()), get the matrix(getMatrix()), set inverse matrix(setIverseMatrix()), 
#get the inverse matrix(getInverseMatrix())

# makeCacheMatrix() will use getter and setter function to set the matrix values

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  setMatrix<-function(y){
    
    x<<- y
    inverse<<-NULL
  } 
  getMatrix<-function() x
  setInverseMatrix<-function(IM) inverse<<- IM
  getInverseMatrix<-function() inverse
  list(set=setMatrix, getMatrix=getMatrix, setInverse=setInverseMatrix, getInverse=getInverseMatrix)
}


# cacheSolve() will search for the matrix in cache if available, directly fork the inverse of matrix 
#otherwise calculate it.

cacheSolve <- function(x, ...) {
        
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data<-x$getMatrix()
  inverse<-solve(data)
  x$setInverse(inverse)
  inverse
}


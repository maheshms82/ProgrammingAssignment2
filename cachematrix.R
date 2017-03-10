## makeCacheMatrix takes x as Matrix and it has 4 functions as follows
## setMatrix - sets the matrix passed as input "x"
## getMatrix - displays the matrix 
## SetInverseOfMatrix - sets the matrix, when inverse is passed from cache, it will be set
## GetInverseOfMatrix - displays the inverse matrix

## This function creates a special "matrix" object that can cache its inverse. 
## The makeCacheMatrix function takes in x as Matrix

makeCacheMatrix <-function(x = matrix()) {
  Matrix_Inverse <- NULL
  setMatrix <- function (y){
    x <<- y
    Matrix_Inverse <<- NULL
  }
  
  getMatrix <- function() x
  
  SetInverseOfMatrix <- function(solve) Matrix_Inverse <<- solve
  
  GetInverseOfMatrix <- function() Matrix_Inverse
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       SetInverseOfMatrix = SetInverseOfMatrix,
       GetInverseOfMatrix = GetInverseOfMatrix)
}

## cacheSolve uses solve function to get the inverse of the matrix
## it calls the SetInverseOfMatrix of makeCacheMatrix to set the inverse of the matrix
## and then it displays the inverse
## is inverse is aleady available, it will be displayed and will send a message "getting cached data"

cacheSolve <- function(x, ...) {
  Matrix_Inverse <- x$GetInverseOfMatrix()
  if(!is.null(Matrix_Inverse)) {
    message("getting cached data")
    return(Matrix_Inverse)
  }
  data <- x$getMatrix()
  Matrix_Inverse <- solve(data)
  x$SetInverseOfMatrix(Matrix_Inverse)
  Matrix_Inverse
}
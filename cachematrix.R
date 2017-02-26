## Put comments here that give an overall description of what your
## functions do
## Programming Assignment 2

## The code below receives as input a square matrix and enables its
## inverse to be calculated.  We assume that the input matrix is always
## invertible.  Since calculating a matrix inverse is 
## computationally expensive, the calculated inverse is cached and may
## be quickly retrieved if the same matrix is submitted a second time.

## Write a short comment describing this function
## The function below initializes receives a matrix and initializes 
## its inverse as null. It also defines (through functions) the 
## behaviors of retrieving and altering matrix and matrix inverses 
## within the function.

makeCacheMatrix <- function(x = matrix()) {

i <- NULL
setmatrix <- function(y) {
   x <<- y
   i <<- NULL
  }
  
getmatrix <- function() x

setinverse <- function(inverse) i <<- inverse

getinverse <- function() i 

list (setmatrix = setmatrix, getmatrix = getmatrix,
      setinverse = setinverse,
      getinverse = getinverse)
  
}


## Write a short comment describing this function
## This function retrieves the matrix inverse from cache if it has already been
## calculated.  In case it has not, it is calculated.

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
  
##  Sample execution
##  In the session below two matrices are created and inverted.
##  For each matrix, the first time the cacheSolve function is called the 
##  inverse is calculated and displayed.   The second time the cacheSolve
##  function is called the inverse is retrieved and displayed directly from
##  cache, without repeating the computationally expensive matrix inversion
##  calculation.
          
## > m2 = matrix(c(4,1,3,1),nrow=2,ncol=2)
## > m1 = matrix(c(7,0,-3,2,3,4,1,-1,-2),nrow=3,ncol=3)
## > Matrix1 = makeCacheMatrix(m1)
## > Matrix1$getmatrix()
## [,1] [,2] [,3]
## [1,]    7    2    1
## [2,]    0    3   -1
## [3,]   -3    4   -2
##  > cacheSolve(Matrix1)
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## > cacheSolve(Matrix1)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## > Matrix2 = makeCacheMatrix(m2)
## > Matrix2$getmatrix()
## [,1] [,2]
## [1,]    4    3
## [2,]    1    1
##> cacheSolve(Matrix2)
## [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4
## > cacheSolve(Matrix2)
## getting cached data##
## [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4
## > cacheSolve(Matrix1)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
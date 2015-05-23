## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function
## Assignment2: Caching the Inverse of a Matrix

## Write a pair of functions that cache the inverse of a matrix.



##################################################################

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

## the function returns a list of 4 functions.
## this will enable to call the assigned variable using the function name in the list,

## To make it more clear, conside the example...
## if we declare a variable...
## TestMatrix <- makeCacheMatrix(matrix(rnorm(16), nrow=4, ncol=4))
## we can call the values of TestMatrix as...
## TestMatrix$get()


makeCacheMatrix <- function(x = matrix()) 
{
     ## matrixInverse is the variable to store the inverse of the matrix
     matrixInverse <- NULL
     
     ## this function will return the matrix
     get <- function() 
     {
          x
     }
     
     ## this function will set the value of matrix
     set <- function(arg1) 
     {
          x <<- arg1
          matrixInverse <<- NULL
     }
     
     ## this function will set the value of inverse of matrix
     setInverse <- function(argInverse)
     {
          matrixInverse <<- argInverse
     }
     
     ## this function will return the inverse of the matrix
     getInverse <- function()
     {
          matrixInverse
     }
     
     ## return a list with 4 functions
     list(set = set, 
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
          
}

##################################################################

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
     ## Return a matrix that is the inverse of 'x'
     matrixInverse <- x$getInverse()
     
     if(!is.null(matrixInverse))
     {
          ## if the matrixInverse is not null, 
          ## then return the already cached value.
          message("getting the cached data")
          return (matrixInverse)
     }
     else ## that is, if the matrix is null
     {
          ## we assign the matrix values to a tempMatrix
          tempMatrix <- x$get()
          
          ## we find the inverse of tempMatrix and assign the value to matrixInverse
          matrixInverse <- solve(tempMatrix)
          
          ## using the setInverse function we assign the inverse value
          x$setInverse(matrixInverse)
          
          ## finally we return the inverse value.
          return (matrixInverse)
     }          
     
     
}


##################################################################








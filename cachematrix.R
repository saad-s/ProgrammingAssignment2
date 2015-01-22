## These functions show how to cache results of computationally 
## extensive calculations in R Programs
## Given example caches the results of inverse of a matrix 
## NOTE: It is assumed that Matrix is inversible
## TODO: Add support for Matrix inversiblility check using det(Matrix)

## USAGE EXAMPLE: 
## m <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
## m_vec <- makeCacheMatrix(m)
## cacheinverse(m_vec)
## cacheinverse(m_vec)

## NOTE: the double arrow operator can modify variables in parent levels.
## REF: http://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r

## This function takes a inversible matrix and 
## creates a list providing functions
## 1. set () on every call this sets the value of matrix and 
##      changes cached inv value to NULL
## 2. get () returns the value of matrix for further calculations
## 3. setinverse () stores the value of inverse of marix
## 4. getinveerse returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # define get for matrix
  get <- function() x
  # define setinverse 
  setinverse <- function(inverse) inv <<- inverse
  # define getinverse
  getinverse <- function() inv
  # create a list of all functions and return it..
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes list returned from above function 
## as input and checks if inv has cached value or not.. 
## On NULL it calculates value of inverse using solve() and 
## and saves it using setinverse()

cacheinverse <- function(x, ...) {
  # check if we already have cached value 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # found cached value... return value
    message("getting cached data")
    return(inv)
  }
  # get data and calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  # save inverse as a cache
  x$setinverse(inv)
  # return inv
  inv
}

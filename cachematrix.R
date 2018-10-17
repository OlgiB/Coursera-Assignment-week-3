##Coursera R Assignment 2
##Olgi Beller
##catching the inverse of a matrix

##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## creating a CacheMatrix, which compute the inverse of our special matrix
## for that we are following the given steps, namely 
## 1. set the value of our matrix
## 2. get the value of our matrix
## 3. set the value of our inverse
## 4. get the value of our inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computing the inverse of a square matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
My test
>source("week_3/Coursera_week_3_peer_assignment.R")
> matrix_a<-makeCacheMatrix(matrix(1:4,2,2))
> matrix_a$get()
    [,1] [,2]
[1,]    1    3
[2,]    2    4
> matrix_a$geti()
NULL
> cacheSolve(matrix_a)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL                             #assuming supplied matrix is invertible
  set <- function(y){                     #Setting the value of the matrix using another function
    x <<- y                               #Double arrow is capable of modifying variable in the parent func
    inv <<- NULL
  }
  get <- function() {x}                   #To get the value of the matrix
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get =get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
cachesolve <- function(x, ...){
  inv <- x$getInverse()                    #Returns a matrix that is inverse of x and assigns it to inv
  if(!is.null(inv)){                       #If inverse is already been calculated, then get the result 
    message("getting cached data")         #from the cache and skip the computation
    return(inv)
  }
  mat <- x$get()                           #Otherwise compute inverse of the matrix and assign it to inv
  inv <- solve(mat, ...)                   #To compute inverse of a matrix use solve
  x$setInverse(inv)                        #Set the inverse value in the cache using the setInverse function
  inv
}

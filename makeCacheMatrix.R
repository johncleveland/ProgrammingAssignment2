## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

### Part 1

## This is the first part of a two part exercise. In this part we construct
##a special matrix object as a list with 4 components. The 1st component, set(), is a
## function which allows one to set  the values of the elements of the matrix. 
## The 2nd, get(), allows one to retrieve the  matrix. The 
##3rd and 4th, setInverse() and getInverse() allow one to set and get the inverse
## of the matrix.



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Part 2 

## Here we construct a function which return the inverse of the  "matrix" object created in 
## makeCacheMatrix above. It first checkes whether the inverse has already been computed.
## If affirmative it returns that value along with a message indicating this. If not, then it computes the inverse and then returns
## the computed value.

cacheSolve <- function(x, ...) {
  
inv <- x$getInverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}





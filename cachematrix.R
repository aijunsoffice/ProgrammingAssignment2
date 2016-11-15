## This R function is for cache the time consuming coputations
## It helps for saving time when you do calculation for a huge matrices

## makeCacheMatrix function to store a 'special' matrix in the cache by using command:
## e.g. abc <- makeCacheMatrix(matrix(1:4,2,2))

## 1. set the value of the matrix which we want to solve x
## 2. get the matrix x
## 3. set the inverse of x
## 4. get the solution of the inverse of x

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


## The following function returns the inverse of matrix x 
## which is definded with the makeCacheMatrix function above
## 1. check if inverse has already been calculated
## 2. if inverse exist show "getting cached data"
##    if not, calculate the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

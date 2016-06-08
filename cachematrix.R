## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## The following pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        ## 1. Setting the value of the matrix
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }
        ## 2. Getting the value of the matrix
        get <- function() x
        ## 3. Setting the value of the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        ## 4. Getting the value of the inverse of the matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse had already been calculated (and the matrix has not changed), 
## then the cachesolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
## Testing makeCacheMatrix Function in R
## > x<-matrix(c(4,1,3,1),2,2)
## > m<-makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    4    3
## [2,]    1    1
## Testing the cacheSolve Function in R
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4
## Retrieving from cache the second time cacheSolve(m) is run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]    1   -3
## [2,]   -1    4



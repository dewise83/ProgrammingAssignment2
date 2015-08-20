## To avoid repeating large calculations of inverses on matrices, these functions 
## will cache inverses in other environments. 


## this first function will return a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This next function calculates the inverse of the matrix. It first checks if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

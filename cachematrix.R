## It creates a list that contains a lits of functions to 
## 1. set the value of the matrix, it deletes the cached value of the inverse
## 2. get the value of the matrix
## 3. set the invers
## 4. get the inverse

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(m) {
        mat <<- m
        inv <<- NULL
    }
    get <- function () mat
    setinverse <- function (inverse) inv <<- inverse
    getinverse <- function () inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the "special" matrix, that is a list created 
## by the above function. It gets the cached value of the inverse and return is the inverse is not null, otherwise
## computes the inverse, stores the computed value in the "special" matrix and return the computed value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
            return (inv)
        }
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
}



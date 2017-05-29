## Programming assignment 2
##
##
## The following functions will first cache the 
## inverse of a matrix and then display those results.
##
## This function will take a square matrix and
## store the matrix and the inverse of the matrix
## into a list "y" for later use.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        y <- list(set = set, get = get,
                  setmatrix = setmatrix,
                  getmatrix = getmatrix)
}


## After running the "makeCacheMatrix" fuction this 
## function will check to see if the matrix is already
## cached. If it is it will tell the user that it is
## already stored. If not it will still run the 
## appropriate code to inverse the matrix.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
}

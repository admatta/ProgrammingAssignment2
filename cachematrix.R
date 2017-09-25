## This function makeCacheMatrix crates a list containging function to
## 1. Get the value of the matrix
## 2. Set the inverse of the matrix
## 3. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    getMatrix <- function() x
    setInverseMatrix <- function(inverse) m <<- inverse
    getInverseMatrix <- function() m
    list(getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, 
         getInverseMatrix = getInverseMatrix)
}


## This function cacheSolve checks if the inverse of the square invertible
## matrix has been computed. If yes then it returns the inverse from the cached
## value else it computes the inverse using solve function and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data)
    x$setInverseMatrix(m)
    return(m)
}

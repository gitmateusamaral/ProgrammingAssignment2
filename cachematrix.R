#Mateus Amaral do Nascimento

#Create a list with the values to be used by cacheSolve:
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverseM <- function(inverseM) m <<- inverseM
    getinverseM <- function() m
    list(set = set, get = get,
         setinverseM = setinverseM,
         getinverseM = getinverseM)
}

#Create the inverse of a square matrix
cacheSolve <- function(x, ...) {
    m <- x$getinverseM()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverseM(m)
    m
}

#Creting a simple matrix:
matrixtest = matrix(1:4,2,2)

#Testing the Functions:
matrix1 <- makeCacheMatrix(matrixtest)
cacheSolve(matrix1)
cacheSolve(matrix1)
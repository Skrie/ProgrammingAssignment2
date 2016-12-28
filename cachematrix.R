## The functions makeCacheMatrix and cacheSolve are used to create a matrix, calcualte the inverse of that matrix
## and then caches the inverse of that matrix. makeCacheMatrix provides a list of functions that can be used to get 
## and set a cached matrix and its inverse. cacheSolve retrieves and returns an inverse matrix, if the inverse matrix has 
## been cached by the makeCacheMatrix function, or calculates and caches a new inverse matrix if one has not been cached by 
## the makeCacheMatrix already.

## makeCacheMatrix receives a matrix as an argument and caches the matrix in the variable x. The function provides a get and set
## method to retrieve and cache the matrix. The function also provides 2 additional functions to get and set a cached inverse of 
## that matrix stored in the variable m. The inverse matrix is calculated in a seperate function named cacheSolve. makeCacheMatrix
## makes available the get and set functions for the cached matrix and its cached inverse through a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(matrix) m <<- matrix
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## cacheSolve accepts a variable x as an argument, variable x has had the function makeCacheMatrix assigned to it. cacheSolve
## the retrieves an inverse matrix from x and assigns the value to the variable m. If m is not null then cacheSolve retrieves
## and returns the cached inverse matrix assigned to m. If m is null then cacheSolve retrieves the cached matrix stored in x, 
## calculates the inverse of that matrix and stores the result in the variable m, where the inverse matrix will be cached for 
## future use, and returns the inverse matrix. 

cacheSolve <- function(x) {
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverseMatrix(m)
        m        
}

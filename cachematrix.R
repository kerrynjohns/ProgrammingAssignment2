# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. Computing the inverse of a square matrix can be done with the solve function in R.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {                    
                x <<- y
                m <<- NULL
        }
        get <- function() x                     
        setsolve <- function(solve) m <<- solve    
        getsolve <- function() m                
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


##Test the functions

#Create a square matrix
test.sq.matrix <- matrix(c(3,1,2,1),nrow=2,ncol=2)

#Apply the makeCacheMatrix function
test.matrix.cache<- makeCacheMatrix(test.sq.matrix)

#Retrieve the inverse matrix from the cache, or solve the inverse matrix if it has not been cached
cacheSolve(test.matrix.cache)

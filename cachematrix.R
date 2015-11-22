## Submisstion for the Coursera R Programming Coures November edition. 
## makeCacheMatric is a function that create a new object 
#  that has the property of storing a matrix and its inverse when calculated.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL  # Set initially the inverse matrix to null
        set <- function(y) {
           x <<- y  # if the method "set"  is invocated the Matrix argument 'y' is stored in the variable x
           inverseMatrix <<- NULL # The Inverse matrix value is reset to NULL
          }    
        get <- function() x  # Get Method just returns the Matrix
        setinverse <- function(inverse) inverseMatrix <<- inverse  # setinverse can be used to write the inverse matrix
        getinverse <- function() inverseMatrix  # getinverse returns the inverse matrix cached.
    
        list(set = set, get = get,   # Returns 4 functions that can be used as method to access the new object
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve is a function that operate on the new Matrix object,
## calculate the inverse and store it in the cache variable "inverseMatrix"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()  # Call the method to get the inverse matrix cached
        if(!is.null(inverseMatrix)) {  # If the cached version is not NULL the fuction returns the cached version and ..
                message("You are getting cached data") # displays a message allerting that is a cached data.
                return(inverseMatrix) # Returns the the inverves matrix
        }
        data <- x$get()    # If there's no cached version it takes the data from the matrix...
        inverseMatrix <- solve(data, ...)  # Calculates the inverse matrix and...
        x$setinverse(inverseMatrix)  # Stores it in the cache variable
        return(inverseMatrix)  # Returns the the inverves matrix
}


    
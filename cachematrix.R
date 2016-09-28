## The following function makeCacheMatrix returns a list of functions that 
## get/set the value of the matrix and get/set the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {   
        m <- NULL # set the value of m to NULL 
        set <- function(y) { # function to set the value of the matrix
                x <<- y # caches the matrix to x
                m <<- NULL # set the value of m to NULL and cache it
        }
        get <- function() x # function to get the value of the matrix and store it 
        setsolve <- function(solve) m <<- solve # function to set the value of the inverse and store it to cache
        getsolve <- function() m # function to get the value of the inverse and store it
        # the output of this function is a list of the created functions above 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function cacheSolve returns the inverse. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setsolve function.

cacheSolve <- function(x, ...) { # returns a matrix that is the inverse of x
        m <- x$getsolve() # set the value of m to the inverse using getsolve function
        if(!is.null(m)) { # if m is NOT NULL, because the inverse already exists, then skip the computation, print the message AND return the inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if m is NULL, set the value of data to the matrix using get function
        m <- solve(data, ...) # set the inverse in the cache using setsolve function
        x$setsolve(m) # set the inverse using setsolve function
        m # return the inverse
}


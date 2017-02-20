## The assignment is to create a invertible matrix which can be stored and inverted.  If the inverse has already been created
## return the inverse matrix, if not, inverse it and return it

## makeCacheMatrix is a function which creates a list of functions which can set the matrix, get the matrix,
## set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        invmat <<- NULL         ##initializes object to store the matrix inverse values
        set <- function(y){
                x <<-y          
                invmat <<- NULL ##clears any values that may have been stored from prior runs of the function cacheSolve
    
        }
        get <- function() x     ##passes the variable from the function call
        setinv <- function(inverse) invmat <<-inverse   ##invmat object gets assigned to the inverse function
        getinv <- function() invmat
        list(set=set,           
             get=get,
             setinv=setinv,
             getinv=getinv)     ##at the completion of the function, a list is returned
}


## The cacheSolve function returns the inverse from cache if it was previously determined and states that it came from cache.
## If not, it applies the solve function which inverts the matrix and returns the value.

cacheSolve <- function(x, ...) {
        invmat <- x$getinv()     
        if(!is.null(invmat)){           ##checks to see if the inverse was previously calculated
                message("Getting cached data")
                return (invmat)         ## returns the cached value
        }
        data <- x$get()
        invmat <- solve(data, ...)      ##inverse not calculated previously, inverts the matrix
        x$setinv(invmat)                
        return(invmat)                  ## returns the calculated inverse
}

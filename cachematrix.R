## A pair of functions that will cache the inverse of a invertible matrix.
## The code is heavily based on example code provided for this assignment

makeCacheMatrix <- function(mat = matrix(data=c(1), nrow=1, ncol=1)) {
        ## The first function, makeVector creates a special "vector", which is
        ## really a list containing a function to
        ##   1.  set the value of the matrix
        ##   2.  get the value of the matrix
        ##   3.  set the value of the inverse
        ##   4.  get the value of the inverse
        
        #  Initialise the variable that will store the inverse
        inv <- NULL
        
        #  'set' will copy the value of its parameter to the variable mat and
        #  initialise the variable inv.
        #  Note that the superassignment will not create local values of mat and
        #  inv, but will track back through the calling environments to find
        #  them.
        
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        
        #  'get' will simply return the value of mat in the current environment
        get <- function() mat
        
        #  'setinv' is a function that will take its parameter, invertedMat and
        #  superassign it to the variable inv (hunting inv down in calling
        #  environments.)
        setinverse <- function(invertedMat) inv <<- invertedMat
        
        #  'getinv' is a carbon copy of 'get': it just returns the value of inv
        #  which hopefully contains the cached mean.
        getinverse <- function() inv
        
        #  Lastly, we construct a list of the functions that we've defined so
        #  far.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  This function calculates the mean of the special "vector" created with the
##  above function. However, it first checks to see if the mean has already been
##  calculated. If so, it gets the mean from the cache and skips the
##  computation. Otherwise, it calculates the mean of the data and sets the
##  value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        
        #  Get the value of the inverse
        inv <- x$getinverse()
        
        #  If it's defined, i.e. not NULL, then we return the cached value,
        #  namely, inverse.  The function exits at this point.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #  If we get to this point, then inv is not yet defined (otherwise we
        #  would have exited with the return).  So data is now given the value
        #  x.  I think the thing to learn here is that x is being used in two
        #  ways: as the name of the list of functions, and, because of the get
        #  function, also the matrix that we are trying to get the inverse of.
        data <- x$get()
        
        #  data now has the matrix, so we can calculate the inverse and put
        #  it in the local variable inv.  The problem is that the local version
        #  of inv will be destroyed when the function exits so...
        inv <- solve(data, ...)
        
        #  ...we use the function setinv to save the inverse of data in one of
        #  the calling environments.
        x$setinverse(inv)
        
        #  lastly, we return the inverse
        inv
}
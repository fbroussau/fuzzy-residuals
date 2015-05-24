## The function makeCacheMatrix and cacheSolve are used to cache the result of the inverse of a matrix
## and to return the cached value if it exists.

## The function makeCacheMatrix creates a list containing a function to 
## (set) set the value of the matrix
## (get) get the value of the matrix
## (setInverse) set the value of the inverse of the matrix
## (getInverse) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,  
		 get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

## The function cacheSolve looked up in the cache to check if the inverse of the matrix has alredy been computed 
## and returns this value if it exists. If not, the inverse of the matriix is computed and returned.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
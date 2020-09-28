## Two functions that caches a given matrix and its inverse and then compute
## its inverse (Lexical Scoping Exercise)

## Creates special matrix that caches its inverse as a list of functions that
## can be called to construct the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Set default values of 'x' and 'm' cached in another environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Retrieve the value of 'x'
        get <- function() x
        
        ## Set inverse matrix as 'm'
        set_inverse <- function(solve) m <<- solve
        
        ## Retrieve the value of 'm'
        get_inverse <- function() m
        
        ## Returns list of functions defined above
        list (set = set, get = get,
              set_inverse = set_inverse,
              get_inverse = get_inverse)
}


## Computes inverse of matrix created by "makeCacheMatrix()"

cacheSolve <- function(x, ...) {
        ## Assign m into function "get_inverse()" in previously returned list
        m <- x$get_inverse()
        
        ## Check to see if already obtained inverse and return 'm' if already 
        ## obtained inverse
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If inverse is not obtained, obtain matrix by calling "get()" from list
        ## 'x'
        data <- x$get()
        
        ## Assign inverse of "data" to 'm'
        m <- solve(data, ...)
        
        x$set_inverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        return(m)
}


################################################################################
## Below are testing codes for myself

## c <- matrix(c(-3, 5, 1, 0), 2, 2)
## g <- makeCacheMatrix(c)
## inverse <- cacheSolve(g)

## inverse

## Sanity check 
## c %*% inverse

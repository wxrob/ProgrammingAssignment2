## Implements cached matrix

## Makes a matrix (x argument) "cachable" by providing setters and getters both for the matrix itself (set and get functions) and for the inverse (set.inverse and get.inverse)

makeCacheMatrix <- function( x = matrix() ) {
    # Implemented analogously to the "cache mean" example
    i <- NULL # Set "local" i variable with NULL
    set <- function( y ) {
        x <<- y     # Set x variable in the global scope with the matrix (y) using the superassignment operator
        i <<- NULL  # Inverse is unknown, so set it to NULL
    }
    get <- function( ) {
        x   # Return the matrix set with the "set" function
    }
    set.inverse <- function( inverse ) {
        i <<- inverse # Set the inverse
    }
    get.inverse <- function( ) {
        i   # Return the inverse set with the "set.inverse" function
    }
    list( set = set, 
          get = get,
          set.inverse = set.inverse,
          get.inverse = get.inverse ) # makeCacheMatrix effectively returns a list of "object methods" that hide the internal storage (i and x variables)
}

## Returns the inverse (i) of matrix x (input argument)
cacheSolve <- function( x, ... ) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get.inverse()
    if( !is.null( i ) ) {
        message( "getting cached data" )    # i has a "non-null" value (presumably correct)
    } else {    # i is NULL so, calculate the inverse using the solve function
        data <- x$get()
        i <- solve( data, ... ) # Relay additional variables from the cacheSolve call using '...'
        x$set.inverse( i ) # Store the calculated inverse
    }
    
    # Return i:
    i
}

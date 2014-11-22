## Implements cached matrix

## Makes a matrix (x argument) "cachable" by providing setters and getters both for the matrix itself (set and get functions) and for the inverse (set.inverse and get.inverse)

makeCacheMatrix <- function( x = matrix() ) {
    i <- NULL
    set <- function( y ) {
        x <<- y
        i <<- NULL
    }
    get <- function( ) {
        x
    }
    set.inverse <- function( inverse ) {
        i <<- inverse
    }
    get.inverse <- function( ) {
        i
    }
    list( set = set, 
          get = get,
          set.inverse = set.inverse,
          get.inverse = get.inverse )
}

## Returns the inverse (i) of matrix x (input argument)
cacheSolve <- function( x, ... ) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get.inverse()
    if( !is.null( i ) ) {
        message( "getting cached data" )
    } else {
        data <- x$get()
        i <- solve( data, ... )
        x$set.inverse( i )
    }
    
    # Return i:
    i
}

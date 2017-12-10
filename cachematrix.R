## these functions work together to store inverted matrices
## in the environment of the function, also include functions that
## call and retrieve the inverted matrices

## makeCacheMatrix takes an invertible matrix (x) and stores it as an
## object that caches its inverse. It also contains functions that can set
## and get the matrices and their inverses

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## default value for inverse is null
        set <- function(y){
                x <<-y
                inv <- NULL
        }
        get <- function() x ## value of x is taken from parent environment
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv 
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
        
}


## cacheSolve performs the inversion of the matrix from makeCacheMatrix.
## if already computed, will pull from cache. 

cacheSolve <- function(x, ...) {
       inv <- x$get_inverse()   
       if(!is.null(inv)){       ##evaluates whether inverse has been computed
               message("retrieving inverted matrix from cache") 
               return(inv) ##if so, returns the inverted matrix without computing again
       }
       
       ## if inv is NULL (not yet calculated), proceeds to calculate inverse
       
       input_matrix <- x$get() 
       inv <- solve(input_matrix,...) ## calculates inverse of input_matrix
       
       x$set_inverse(inv) ## sets the inverse in the cache using set_inverse
       inv ## prints inverse
}

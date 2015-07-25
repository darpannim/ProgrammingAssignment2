## Functions should allow the users to set and get the values of the matrix and inverse 
## For efficiency sake value of the inverse should be set to cache first it is computed and retrieved from cache
## if there are no changes to the matrix value

## makeCacheMatrix Function returns a vector of functions which describes 
## set function, get function, set inverse function, and get inverse function

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set_matrix <- function(y) {
    x <<- y                      ##Set value of the matrix
    mat_inverse <<- NULL         ##Set inverse of the matrix back to Null
  }
  get_matrix <- function() x     ##Get the matrix
  set_inverse <- function(mat_inverse_val) mat_inverse <<- mat_inverse_val   ##Set the value of the matrix inverse
  get_inverse <- function() mat_inverse        ## Get the value of the matrix inverse
  ## Function returns a vector of functions which describes set function, get function, set inverse function, and get inverse function
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve will check if the value of matrix inverse is present in the cache; if yes, the value from cache is returned
## if no, then the value of matrix inverse is computed, saved in cache and returned 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$get_inverse()
  if(!is.null(mat_inverse)) { ## Check if the mat_inverse is already present in Cache
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get_matrix()
  mat_inverse_val <- solve(data)     ## Compute the inverse of the matrix
  x$set_inverse(mat_inverse_val)
  mat_inverse_val
}

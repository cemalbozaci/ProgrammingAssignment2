## this function creates a special "matrix", which is really a list containing a function to
## set matrix, get matrix, set inverse of matrix, get inverse of matrix
## function creates an error message if input is not a matrix

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)){
    message("please make sure your input in the form of matrix(c(2,3,1,4),nrow=2,ncol=2)")
  }
  i = NULL
  assign_catched = function(catched) i <<- catched
  get_catched = function() i 
  get_data = function() x
  list(assign_catched = assign_catched, get_catched = get_catched,
       get_data = get_data)
}


## this function solve a matrix(get inverse of it), 
## this function also checks if inverse of matrix already calculated if so get inverse from cache
## this function assumes matrix is invertable

cacheSolve <- function(x, ...) {
      
i = x$get_catched()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get_data()
  i <- solve(data, ...)
  x$assign_catched(i)
  i
}

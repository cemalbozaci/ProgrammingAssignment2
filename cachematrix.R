## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

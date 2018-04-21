############################## Assignment 2 ##################################

# This function creates a special matrix object that can cache its inverse.
# for this example let's create a special matrix called 'ab'
# ab <- makeCacheMatrix(r)
# r is a matrix
#----------creating the function-------------------------------
makeCacheMatrix <- function(b = matrix()) {
  a <- NULL
  set <- function(z) {
    b <<- z    # setthing the value
    a <<- NULL # clearing the cache
  }
  #--------function for gettng the value of matrix--------------
  get <- function() b
  #--------function for setting inverse-------------------------
  setinverse <- function(inverse) a <<- inverse
  #--------function for getting inverse------------------------
  getinverse <- function() a
  # ------A list of all the four ------------------------
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#------- end of function to cache its inverse------------------------------


## This next function will inverse the matrix 'ab' created from the above makeCacheMatrix function-------

#-- This will use 'getinverse' from the matrix 'b' created in the previus function-------

cacheSolve <- function(b, ...) {
  
  #-- This will use 'getinverse' from the matrix 'x' created in the previus function---------
  a <- b$getinverse()
  
  #--- 'if' statment to check if 'a' is null and return the value of 'a'---------
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  
  # if the value of 'a' is empty from the previous if statement, than we need to compute the matrix
  # -------retreving the value of 'get' from the previous function------  
  
  getting_data <- b$get()
  
  ##------------ using the solve function to inverse the matrix---------
  a <- solve(getting_data, ...)
  
  ##------------- Caching the value of the returned matrix 'a'-----------------
  b$setinverse(a)
  
  #-------final inverse of the matrix --------------------------
  a
}
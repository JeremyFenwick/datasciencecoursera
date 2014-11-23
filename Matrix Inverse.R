makeCacheMatrix <- function(x = numeric()) { # Create the function makeCacheMatrix
      s <- NULL # "s" is the result of solve(), set to NULL. This way when a new vector is created 
                # with the same name "s" is reset
      get <- function() { x } # Function that stores the matrix "x"
      setsolve <- function(solve)  { s <<- solve } # Function that superassigns the value "s"
      getsolve <- function() { s } # Function that returns "s"
      
      list(get = get,
           setsolve = setsolve,
           getsolve = getsolve) # Names of the functions, type in makeCacheMatrix() to see the three functions
}
      
cacheSolve <- function(x, ...) { # Create the function cacheSolve
    s <- x$getsolve() # Calls the function getsolve and stores the value in "s"
    if(!is.null(s)) { # If "s" is anything other than NULL
      message("getting cached data") # Prints "getting cached data"
      return(s) # Returns the solved matrix
  }
      thematrix <- x$get() # Otherwise use the get function to store the matrix in "thematrix"
      s <- solve(thematrix) # Solve "thematrix"
      x$setsolve(s) # Stores the calculation in function setsolve
      print(s) # Prints the solved matrix, "s"
}

# Example test
# newmat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
# cacheSolve(newmat)
# then repeat to check if the calculation was stored
# cacheSolve(newmat)
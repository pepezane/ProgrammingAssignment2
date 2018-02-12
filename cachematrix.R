
##For this Assignment, which allows me to cache a time consuming computation such as the 
## inverse of a matrix, so as to avoud re-calculations for fast operations.

##this function creates a special matrix object that can cache an inverse
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  ##this sets the value of the matrix
  setter <- function(param)
            {
              x <<- param
              cache <<- NULL
  }
  ##this retrives the value of the matrix
   getter <- function(){x}
   
   ##this sets the value of the inverse of the matrix
   setcache <- function(param)
             {
               cache <<- param
   }
   
   ##this gets the value of the inverse of the matrix
   getcache <- function() {cache}
   
   list(setter = setter,getter = getter, setcache = setcache,getcache = getcache)
}


## This function computes the inverse of a matric returned, it checks if the invverse exists
## or has been calculated, retrieves it if it has and computes the inverse if it has not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  output <- x$getcache()
  
  if(!is.null(output))
  {
    message("getting cached data")
    return(output)
  }
  
  data <- x$getter()
  output <- solve(data,...)
  x$setcache(output)
  output
}

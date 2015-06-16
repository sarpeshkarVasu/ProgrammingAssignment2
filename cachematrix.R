## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
#################################################################
## A special "matrix" object is created here to cache its inverse.
#################################################################
makeCacheMatrix <- function(x= matrix()){
    
                  m <- NULL
        
                  set <- function(y) {
                 
                  x <<- y
                   
                  m <<- NULL
                   
              }
           
                 get <- function() x
             
                 setInverse <- function(matrix) m <<- matrix    
               
                 getInverse <- function()m
                 
                # return a list of functions
                list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
                 
                   
                   
      }
 
  
## Write a short comment describing this function
#############################################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Also If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
#############################################################################################
 
cacheSolve <- function(x,...){
         
              m <- x$getInverse()
           
              #check if the inverse has already been calculated
              if(!is.null(m)) {
                   
                  message("getting cached data")
                   
                  return(m)
                }
           
            # return the inverse matrix if one is not already calculated
            data <- x$get()
             
            m <- solve(data,...)
               
            x$setInverse(m)
               
            m
               
  }
##################################################################
# How this works:
# 1. Create a test matrix: m <- matrix(c(1,2,3,4),nrow=2,ncol=2)
# 2. Create a special object: so <- makeCacheMatrix(m)
# 3. Compute inverse of special object: iso<- cacheSolve(so)
# 4. Print iso to see the inverse matrix: iso
# 5. Repeat step 3 to get cached inverse instead of recalculation
####################################################################
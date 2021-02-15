#optimize computational resources 
#caching the inverse of a given "matrix" 
#for further uses
#using the double arrow operator to modify variables in the parent function 
#environment




  
  ## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) #the default argument is an empty matrix
{
  
    inversematrix <- NULL
  
      set <- function(y)
      {
    
        x <<- y
        
        inversematrix <<- NULL
    
    
      }
  
          get <- function() x
          
          
          setinversematrix <- function(inverse) inversematrix <<- inverse
          
          
          getinversematrix <- function() inversematrix
          
          
          list(set =set, get=get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
  
}


#This function computes the inverse of the special "matrix" created by 
#makeCacheMatrix function above. 
#If the inverse has already been calculated (and the 
# matrix has not changed)
#then it will retrieve the value already computated and cached

cacheSolve <- function(x, ...) 
  {
      
      inversematrix <- x$getinversematrix()  ## return cached matrix inverse if it's been already computed
      
            if(!is.null(inversematrix))
            {
       
                message("checking the memory banks")
          
                return(inversematrix)
            
            }
          
  
                   data <- x$get()  # compute inverse of matrix for a new value of x
  
                   inversematrix <- solve(data, ...) 
  
                   x$setinversematrix(inversematrix) # cache inverse
  
                   return(inversematrix) #return the inverted matrix
                  
  
  }


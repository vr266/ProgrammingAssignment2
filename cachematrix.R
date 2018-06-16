## Put comments here that give an overall description of what your
## functions do
#The two functions are used to find the inverse of an input matrix, x. They are heavily derived from the functions given as examples, with only minor changes
#They make use of caching; The first stores values into a list, and the second checks that list to see whether it needs to calculate
#the inverse, or if it can simply draw from the given value.


## Write a short comment describing this function
#This function takes a mtrix input, and produces a list containing a function to set and get the matrix input and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL#this is the inverse matrix; it is set to null initially to avoid errors
  set <- function(y) {
    x <<- y
    i <<- NULL
  }#sets the input value
  get <- function() x #the get function
  setinverse <- function() i <<- solve(x)# finds the inverse using the solve function
  getinverse <- function() i#gets the inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)#list that contains the functions and values
}


## Write a short comment describing this function
#This function takes a matrix input, and should the inverse of the matrix have been cached in the previous function,
#will simply use that value. If not, it will calcuate the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()# i is taked from the first matrix
  if (!is.null(i)) {#checks if the inverse is not null, or if it exists from "makeCacheMatrix"
    message("getting cached data")
    return(i)
  }#should the inverse for the matrix be found in the cache, it will be returned. If not, the next steps
  mati <- x$get()#takes the value of x, or input matrix from the other environment. "mati" is equated to it, to prevent confusion
  i <- solve(mati, ...)#finds the inverse
  x$setinverse(i)#uses the setinverse function from  above
  i#produces the inverse
}

## Neena Davies/19 August 2014
## 
## These functions are for Coursework 002 of ProgramR with coursera.
## Please note that I use both <- and = for assignment based on experience
## in other projects.  They have the same properties. 

## Brief Description of makeCacheMatrix
## makeCacheMatrix performs set, get, setInverse and getInverse actions 
## on the matrix passed in.  Note that y is the variable that can retain
## it's value via lexical scoping.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    #set y to x and assign m as null, not the <<- operator here
    #this means that a value can be assigned from a different environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get function will return x
    get <- function() x
    
    #This is the inverse matrix method solve
    setInverse <- function(matrix) m <<- solve(matrix)
    
    #Get inverse returns m
    getInverse <- function() m
    
    #The last command is what is returned when the function is called.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Brief Description of cacheSolve
## cacheSolve checks that the matrix doesn't exist already and then 
## creates it.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    # First check if the matrix is cached already
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #If m isn't cached then call the get function to get the value
    # and setInverse 
    data <- x$get()
    m <- data
    x$setInverse(m)
    
    m
}

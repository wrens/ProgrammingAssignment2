## Put comments here that give an overall description of what your
## functions do
## 
## functions:
## makeCacheMatrix
## inputs: this function takes a matrix as an input
## outputs: there is no return from this function, it is invisible however
##          there are subfunctions(methods) that set and change variables
## variables:
##          the_matrix - this is a GlobalEnv to hold a matrix and test for matrix identicality
##                       so we dont run a bunch of code needlessly
##          x - this is a matrix passed into the function
##          inverted_matrix - this is a matrix of the inversion of a matrix
## methods:
##          set_matrix - sets the passed in matrix to be inverted
##          get_matrix - gets the stored matrix to be inverted
##          set_inverse - sets the inverse of the stored matrix
##          get_inverse - gets the stored inverted matrix
##
## cacheSolve
## inputs: this function takes a makeCacheMatrix object
## outputs: this function returns an inverted matrix
## variables:
##          the_matrix - this is a GlobalEnv to hold a matrix and test for matrix identicality
##          m - this is a variable holding the contents of get_inverse(), a method from a
##              makeCacheMatrix object
##          data - this is a variable holding the contents of get_matrix(), a method from a
##          makeCacheMatrix object

#the_matrix <- NULL

## Write a short comment describing this function
#   this function takes in a matrix and creates 4 methods to access the object

makeCacheMatrix <- function(x = matrix()) {
    
    inverted_matrix <- NULL
   # if(is.null(the_matrix)){
    #    the_matrix<<-x
    #}
    set <- function(y) {
        message("setting cached matrix data")
        x <<- y
        inverted_matrix <<- NULL
        
    }
    
    get <- function() x
    
    setinverse <- function(y) {
        inverted_matrix <<- y  
    }
    
    getinverse <- function() inverted_matrix
    
    list(set_matrix = set,get_matrix = get,set_inverse = setinverse,get_inverse = getinverse)
}


## Write a short comment describing this function
#   this function calculates the inverse matrix of a makeCacheMAtrix object passed into it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 
    ## tests ##
    ## the purpose of these are to test the methods of function makeCacheMatrix
    ##
    ## instantiate thus:
    ## c=rbind(c(1, -1/4), c(-1/4, 1))
    ## a<-makeCacheMatrix(c)
    ## cacheSolve(a)
    ##
    ## creates matrix:
    ##       [,1]  [,2]
    ## [1,]  1.00 -0.25
    ## [2,] -0.25  1.00 
    ##
    ## inverse is:
    ##            [,1]      [,2]
    ## [1,] 1.0666667 0.2666667
    ## [2,] 0.2666667 1.0666667
    ##
    ## test online at: http://matrix.reshish.com/inverCalculation.php or solve(c)
    
    #     my_matrix<-x$get_matrix() # get matrix
    #     print(my_matrix) # print matrix
    #     print(the_matrix)
    #     my_inverted_matrix<-x$get_inverse() # get inverse, should be NULL to start with
    #     print(my_inverted_matrix) # print inverse 
    #     x$set_inverse(solve(my_matrix))  # set inverse
    #     my_inverted_matrix<-x$get_inverse() # get inverse again
    #     print(my_inverted_matrix) # print inverse
    
    m<-x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get_matrix() 
    #the_matrix<-x$get_matrix()
    m <- solve(data)
    x$set_inverse(m)
    m 
} 

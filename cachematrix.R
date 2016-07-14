## Here are two functions that work together to find the inverse of a matrix,
## not calculating it each time requested, instead if it is calculated, it
## is cached and when requested the cached inverse is returned.

## makeCacheMatrix accepts as a parameter a matrix, and returns a list of functions 
## to : i)set the matrix, ii)return the matrix, iii) set cached inverse of the matrix
## and iv) get cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x                         #return the matrix
        setcachedinverse <- function(s) m <<- s           #set the inverse
        getcachedinverse <- function() m                  #return the inverse
        list(setmatrix = setmatrix,                       # list of functions 
             getmatrix = getmatrix,                       # which are returned
             setcachedinverse = setcachedinverse,         # by makeCacheMatrix
             getcachedinverse = getcachedinverse)
}


## Function cacheSolve checks if the inverse of the requested matrix has 
## allready been calculated and cached, and if so, returns the cached result.
## Otherwise it calculates(solves) the requested inverse and caches it. 
## In order to achieve this, the parameter passed, should be a special object 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getcachedinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data)
        x$setcachedinverse(m)
        m
}


###Examples of usage and verification:
# source("ProgrammingAssignment2/cachematrix.R")
###create an inversible matrix for the test :
# c=rbind(c(1, -1/4), c(-1/4, 1))
###create x, the "special" parameter object that will be passed into cacheSolve :
# x<-makeCacheMatrix(c)
###Now, we can either execute cacheSolve to see the inverse
# cacheSolve(x)
###Or calculate the inverse and perform a matrix multiplication to see if we get an
###Identity matrix
# rc<-cacheSolve(x)
# c%*%rc

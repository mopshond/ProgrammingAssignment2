## The first function, makeCacheMatrix, creates a set of functions and returns them in a list.
## These functions set the value of the matrix, gets it values, set its invers and gets its inverse, respectively. 

makeCacheMatrix <- function(x = matrix()) {
    
    mat <- NULL

    set <- function(y){
        ##Assign to outside of function environment
            mat<<- NULL            
            x <<- y
        }
        get <- function() x

        setinverse <- function(solve) mat<<- solve
        getinverse <- function() mat
        
        ##Return list of these functions
        list( set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


##This function, cacheSolve, recieves the output of the obave function as an input.
##It checks the environment to see whether a a previous calculation cache exists, if it does not, 
##then the "solve" calculation is performed. If the solution has been perfomred previously, then the 
## stored inverse will be returned form the "mat" variable. 

cacheSolve <- function( x, ...){
        mat<- x$getinverse()
        if(!is.null(mat)) {                
            return(mat)
        }
    
        store <-x$get()

        mat<- solve(store, ...)

        x$setinverse(mat)

        mat
        ## Return a matrix that is the inverse of 'x'
}

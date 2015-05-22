# assignment_2

## The first function receives a square matrix as its input and creates a special 'vector', which is a list of matrices and functions
## Then, this list is passed as the input to the second function, which will calculate the inverse of the original matrix 'x' or 
## retrieve it from cache in case it have already been calculated. If the inverse is calculated, then after this, the second function
## will store the inverse in the cache.

## The input of this function is a square matrix. This function creates a list of matrices
## as defined below line by line.

makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL                                 ## Reserving memory space for the variable mat and asigning it the value NULL
  set_matrix<-function(y){                  ## This function asigns the value "y" to "x" and the value NULL to "mat".
    x<<-y                                   ## Both "x" and "mat" are located at the environment of the parent function
    mat<<-NULL                              ## "makeCacheMatrix".
  }
  get_matrix<-function(){                   ## This function stores the value of "x" in "get_matrix".
    x
  } 
  set_inversemat<-function(inversemat){   ## This function asigns "inversemat" to "mat". But "mat" is in the upper environment
    mat<<-inversemat                      ## that is why <<- must be used.
  }
  get_inversemat<-function(){             ## This function stores the value of "mat" in get_inversemat
    mat
  }
  list(set_matrix=set_matrix, get_matrix=get_matrix,
       set_inversemat=set_inversemat, get_inversemat=get_inversemat)

}


## Write a short comment describing this function
## This function takes the output of makeCacheMatrix and does one of the following:
## if the inverse of the original matrix 'x' hasn´t been calculated, then this functions does it.
## if the inverse of the original matrix 'x' has already been calculated, then this function retrieve its inverse
## matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat<-x$get_inversemat()             ## This line assigns the value stored in 'get_inversemat' to 'mat'
  if(!is.null(mat)){                  ## if 'mat' is not empty, meaning that its inverse matrix has already been calculated 
      message("Getting cached data")  ## then the message is shown and the inverse matrix returned, finishing the code.
      return(mat)
  }                                   ## If 'mat' is empty
  data_matrix<-x$get_matrix()         ## the value of the original matrix 'x' is asigned to 'data_matrix'  
  mat<-solve(data_matrix,...)         ## The inverse of 'data_matrix' is calculated and stored in 'mat'
  x$set_inversemat(mat)               ## This calls the function to cache the inverse matrix
  mat                                 ## This returns the inverse matrix, 'mat'
}

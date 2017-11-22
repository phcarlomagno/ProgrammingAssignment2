# R Programming Assignment: Week 3
# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a 
# matrix.
# Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can 
#                  cache its inverse.
# cacheSolve     : This function computes the inverse of the special "matrix" 
#                  returned by makeCacheMatrix above. If the inverse has already
#                  been calculated (and the matrix has not changed), then the 
#                  cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) returns 
# its inverse.
# 
# Author : Carlomagno Anastacio
# Version: 1.0
# Date   : 22 Nov 2017


################################################################################
# Program Assumptions:                                                         # 
# 1. For this assignment, assume that the matrix supplied is always invertible.#
# 2. The functions would only execute square matrices                          #
# 3. This function does not compute left or right inverse                      #
################################################################################

################################################################################
# For testing matrices, use http://matrix.reshish.com/inverCalculation.php     #
################################################################################

################################################################################
# This function creates an object instance that has access to the elements
# Input : x - a matrix, initialized to an empty one
# Output: a matrix object, containing the set, get, setInverseMatrix,
#         getInverseMatrix
# Throws: Error if passing an non-matrix value as argument
################################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  #Error checking if the argument passed is a matrix
  if(!is.matrix(x)){
    message("please input a matrix as an argument.")
    return()
  }
  
  
  #Error checking if the argument passed is not a square matrix
  if(nrow(x) != ncol(x)){
    message("please input a square matrix as argument.")
    return()
  }
  
  
  #assignment for cached variable, 
  #default to NULL upon call to makeCacheMatrix() 
  inCacheMatrix = NULL
  
  
  # Allow setting of a matrix directly to an existing makeCacheMatrix object
  # Input : matrixInput - a valid matrix
  # Output: parent variable x is assigned with matrixInput value
  #         parent variable inCacheMatrix is reset to NULL due to a new matrix
  # Throws: None
  set <- function(matrixInput){
    x  <<- matrixInput
    inCacheMatrix <<- NULL
  }
  
  
  # Retrieves and displays the current value of the parent variable x
  # Input : None
  # Output: Display contents of parent variable x, NULL if none
  # Throws: None
  get <- function(){
    x
  }
  
  
  # Sets the cached inverse matrix based on argument from cacheSolve().  Can 
  # be used to directly set the cached value, if needed.
  # Input : invMatrix - a valid square matrix
  # Output: parent variable inCacheMatrix is set with the invMatrix argument
  # Throws: None
  setInverseMatrix <- function(invMatrix){
    inCacheMatrix <<- invMatrix
  }          
  
  
  # Displays the cached inverse matrix
  # Input : None
  # Output: displays concents of parentVariable inCacheMatrix; NULL if none
  # Throws: None
  getInverseMatrix <- function(){
    inCacheMatrix
  }
  
  
  # Create a list of functions available to the user after creating the
  # makeCacheMatrix object
  list(get = get, 
       set = set,
       getInverseMatrix = getInverseMatrix,
       setInverseMatrix = setInverseMatrix)
}


################################################################################
# This function takes a makeCacheMatrix object and get its matrix inverse.  The
# function will return the inverse matrix if it is already cached, with a
# message it was retrieved instead of computed; else it would calculate for the 
# inverse of the matrix and display it.
# Input : x - a valid makeCacheMatrix object
# Output: retrieve the cached value, display it, and inform the user that
#         a cached inverse matrix exists; calculate the inverse of the matrix in
#         x and display if it does not yet exist
# Throws: None
################################################################################
cacheSolve <- function(x, ...) {
  
  #Check if an existing cached matrix exists and store in a local variable
  cachedVal <- x$getInverseMatrix()   
  
  #Check if a cached value already exists using the previous line of code
  if(!is.null(cachedVal)){            
    message("Getting cached data:")    #inform the user cached matrix exists
    return(cachedVal)                  #display the existing cached matrix
  }else{
    dataToCache <- x$get()             #if none cached, get the input matrix
    #from the parameter argument
    cachedVal <- solve(dataToCache,...)#calculate its inverse.  From ?solve,
    #the input needs to be a square
    x$setInverseMatrix(cachedVal)      #store it in cache
    cachedVal                          #display the resulting inverse matrix
  }
}
#End of File
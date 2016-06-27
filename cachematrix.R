##Programming Assigment 2: Caching the Inverse of a Matrix
## makeCacheMatrix is a function that returns a special "matrix" 
##This special "matrix" is a list that contains a function to:
## set, and get the value of the matrix
## set, and get the value of the inverse matrix

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL
        set<-function(a){
                x<<-a
                inv<<-NULL
        }
        get<-function () x
        setInverse<- function(inverse) inv <<-inverse
        getInverse<- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## The function "solve" returns the inverse of the matrix (created through makeCacheMatrix)
## Using "cacheSolve" we can store the inverse matrix. This caching reduce the computation costs.

cacheSolve<-function(x,...){
        inv<-x$getInverse()
        if(!is.null(inv)){
                message ("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setInverse(inv)
        inv
}

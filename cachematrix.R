

# Lists containing matrixs and inverse calculated
listCachedMatrix<-list()
listCachedResults<-list()

## Return the inverse of 'matrix' and save the results in a list for future uses
makeCacheMatrix <- function(matrix) {
    Inverse<-solve(matrix)
    listCachedMatrix[[length(listCachedMatrix) + 1]] <<- matrix
    listCachedResults[[length(listCachedResults) + 1]] <<- Inverse
    Inverse
}

cacheSolve <- function(x) {
    compareMatrixs<-function(matrix1,matrix2){ #Compare matrixs if there are identicals or not 
        areTheyIdentical<-matrix1==matrix2
        falseOrTrue<-which(areTheyIdentical==FALSE) #If there are one "FALSE"--> no identicals
        if(length(falseOrTrue) == 0){
            TRUE
        } else {
            FALSE
        }
    }
    searchCachedMatrix<-function(){ # Search cached matrix in a list of matrixs "listCachedMatrix"
        position<-0
        searchPosition<-0
        for(matrizCached in listCachedMatrix){
            position<-position+1
            m<-compareMatrixs(matrizCached,x)
            if(m==TRUE){
                searchPosition<-position
                break
            }  
        }
        searchPosition     
    }

    searchPosition<-searchCachedMatrix() # If no cached matrix the inverse is calculated, if not the result saved in the results list "listCachedResults" is given
    if(searchPosition==0){
        #print("calculating...")
        makeCacheMatrix(x)
    }else {
        #print("result cached")
        listCachedResults[searchPosition]
    }
}

# Examples of matrices
# matrix1<-matrix(1:4,2,2)
# matrix2<-matrix(2:5,2,2)

# print(cacheSolve(matrix2))
# print(cacheSolve(matrix2))
# print(cacheSolve(matrix1))
# print(cacheSolve(matrix1))
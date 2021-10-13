#' @title From test statistics to p-values
#' @description Use permutation distribution of a test statistic to get p-values.
#' @param Test_matrix matrix where columns represent the B permutations and rows the m tests statistic. 
#' The observed test statistic is in the first column
#' and the permutation distribution in the remaining columns
#' @param alternative character string referring to the alternative hypothesis (\code{"greater"}, \code{"lower"}, or \code{"two.sided"}). 
#' @param exact logical value, \code{TRUE} to compute p-values by permutation distribution. Default @TRUE.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.
#' @param gdl numerical value. Degree of freedom.
#' @return matrix of p-values where columns represent the B permutations and rows the m tests statistic. 
#' @export
#' @importFrom stats pt
#' @importFrom matrixStats rowRanks
#' 
t2p <- function(Test_matrix, alternative, exact = TRUE, permReturn, gdl = NULL){

  if(!exact & is.null(gdl)){warning("Please insert the degree of freedom")}
  if(permReturn){
    
    if(!exact){
      
      pv_matrix <- switch(alternative, 
                          "two.sided" = 2*(pt(abs(Test_matrix), df = gdl,  lower.tail=FALSE)),
                          "greater" = pt(Test_matrix, df = gdl,  lower.tail=FALSE),
                          "lower" = 1-pt(Test_matrix, df = gdl,  lower.tail=FALSE)) 
      
    }else{
      
      pv_matrix <- switch(alternative, 
                          "two.sided" = rowRanks(-abs(Test_matrix)) / ncol(Test_matrix),
                          "greater" = rowRanks(-Test_matrix) / ncol(Test_matrix),
                          "lower" = rowRanks(Test_matrix) / ncol(Test_matrix))
      
      
    }
    
    out <- list(Test = Test_matrix, pv = pv_matrix)
  }else{
    
    if(!exact){
      
      pv_matrix <- switch(alternative, 
                          "two.sided" = 2*(pt(abs(Test_matrix[,1]), df = gdl,  lower.tail=FALSE)),
                          "greater" = pt(Test_matrix[,1], df = gdl,  lower.tail=FALSE),
                          "lower" = 1-pt(Test_matrix[,1], df = gdl,  lower.tail=FALSE)) 
      
    }else{
      
      pv_matrix <- switch(alternative, 
                          "two.sided" = rowRanks(-abs(Test_matrix)) / ncol(Test_matrix),
                          "greater" = rowRanks(-Test_matrix) / ncol(Test_matrix),
                          "lower" = rowRanks(Test_matrix) / ncol(Test_matrix))
      
      
    }
    
    out <- list(Test = Test_matrix[,1], pv = pv_matrix[, 1])
  }
  
  return(out)
}
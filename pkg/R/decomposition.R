#' @title Decomposition Result
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate the result of accounting multipier decomposition multiply by injection matrix
#' @param I injection matrix generate by getMatriksInjeksi function in injeksi.R
#' @param Ca Closed Loop matrix generate by getCa function in ClosedLoop.R
#' @param Oa Open Loop matrix generate by getOa function in OpenLoop.R
#' @param Ta Transfer Loop matrix generate by getTa function in Transfer.R
#' @author Tiara Dewi
#' @export


dekomposisi <- function(I,Ca,Oa,Ta) {
  Ca <- Ca%*%I
  Oa <- Oa%*%I
  Ta <- Ta%*%I
  result <<- cbind(Transfer=Ta,OpenLoop=Oa,ClosedLoop=Ca)

  for (i in 1:nrow(result)){
    for (j in 1:ncol(result)){
      result[i,j] <- round(result[i,j],5)
    }
  }
  return (result)

}

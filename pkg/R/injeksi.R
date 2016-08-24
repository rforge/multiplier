#' @title Injection Matrix
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate injection matrix
#' @param A average prospensity matrix generate by adjustedmatrix function in averagprospensitymatrix.R
#' @param baris_start number containing start row generate from user input
#' @param baris number containing row number that will be injected generate from user input
#' @param EXP list containing row numbers of exogen accounts from user input
#' @param injeksi value that will be injected in specified row
#' @author Tiara Dewi
#'
getMatriksInjeksi<- function(injeksi,baris,EXP,baris_start,A){
  n <- nrow(A)
  k <- 0
  for (i in 1:length(EXP)){
    if (baris>EXP[i]){
      k <- k+1
    }
  }
  In <- matrix (0,n,1)
  In [baris-k-baris_start+1] <- injeksi
  print("################################# In ##############################")
  print(In)
  return(In)
}

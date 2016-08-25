#' @title Average Prospensity Matrix
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to generate the average prospensity matrix from data
#' @param baris1 number containing start row generate from user input
#' @param kolom1 number containing start column generate from user input
#' @param newfsam data that contain endogen accounts only from getendogen.R
#' @author Tiara Dewi
#' @export


adjustedmatrix <-function(newfsam,baris1,kolom1){
  #Bikin adjusted matrix
  n <- nrow(newfsam)-baris1
  A <- matrix(0,n,n)
  total <- nrow(newfsam)

  for (i in baris1:(nrow(newfsam)-1)){
    for (j in kolom1:(ncol(newfsam)-1)){
      A[(i-baris1+1),(j-kolom1+1)]=newfsam[i,j]/newfsam[total,j]
    }
  }
  return(A)
}

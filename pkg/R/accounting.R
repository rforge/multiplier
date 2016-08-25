#' @title Accounting Multiplier
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to generate multiplier
#' @param Af prospensity matrix
#' @author Tiara Dewi
#' @export

accountingmultiplier <- function(Af) {
  #Menghitung Matriks Pengganda Neraca (Ma)
  n <- nrow(Af)
  I <- diag(x=1,n,n)
  Matemp <- I-Af
  Ma <- solve(Matemp)
  for (i in 1:n){
    for (j in 1:n){
      Ma[i,j] <- round(Ma[i,j],5)
    }
  }
  #Matriks Pengganda Neraca sudah didapat
  return (Ma)
}

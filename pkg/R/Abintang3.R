#' @title A***
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Abintang2 matrix for accounting multipier decomposition.
#' @param Abintang A* matrix that generate by ABintang function in Abintang.R
#' @param Abintang2 A** matrix that generate by ABintang2 function in Abintang2.R
#' @author Tiara Dewi

getABintang3 <-function (Abintang,Abintang2){
  Abintang3 <- Abintang2 %*% Abintang
  return (Abintang3)
}

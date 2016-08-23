#' @title A**
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Abintang2 matrix for accounting multipier decomposition.
#' @param Abintang A* matrix that generate by ABintang function in Abintang.R
#' @author Tiara Dewi


getABintang2 <- function (Abintang){
  Abintang2 <- Abintang %*% Abintang
  return (Abintang2)
}

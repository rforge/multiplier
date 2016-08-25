#' @title Ma3
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Ma3 matrix for accounting multipier decomposition.
#' @param I identity matrix
#' @param Abintang A* matrix that generate by ABintang function in Abintang.R
#' @param Abintang2 A** matrix that generate by ABintang2 function in Abintang2.R
#' @author Tiara Dewi
#' @export


getMa3 <-function (I, Abintang, Abintang2){
  Ma3 <- I + Abintang + Abintang2
  return (Ma3)
}

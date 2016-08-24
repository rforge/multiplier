#' @title Ma2
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Ma2 matrix for accounting multipier decomposition.
#' @param I identity matrix
#' @param Abintang3 A*** matrix that generate by ABintang3 function in Abintang3.R
#' @author Tiara Dewi


getMa2 <- function (I, Abintang3){
  Ma2 <- solve(I-Abintang3)
  return (Ma2)
}

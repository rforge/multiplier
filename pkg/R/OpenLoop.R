#' @title Open Loop
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Oa matrix for accounting multipier decomposition.
#' @param I identity matrix
#' @param Ma1 Ma1 matrix that generate by getMa1 function in Ma1.R
#' @param Ma2 Ma2 matrix that generate by getMa2 function on Ma2.R
#' @author Tiara Dewi


getOa <- function (Ma1,Ma2,I){
  Xoa <- (Ma2-I)
  Oa <- Xoa %*% Ma1
  return (Oa)
}

#' @title Transfer Loop
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Ta matrix for accounting multipier decomposition.
#' @param I identity matrix
#' @param Ma1 Ma1 matrix that generate by getMa1 function in Ma1.R
#' @author Tiara Dewi

#Ta (Pengganda transfer)
getTa <- function (Ma1,I){
  Ta <- Ma1-I
  return (Ta)
}

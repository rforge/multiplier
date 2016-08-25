#' @title Closed Loop
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Ca matrix for accounting multipier decomposition.
#' @param I identity matrix
#' @param Ma1 Ma1 matrix that generate by getMa1 function in Ma1.R
#' @param Ma2 Ma2 matrix that generate by getMa2 function in Ma2.R
#' @param Ma3 Ma3 matrix that generate by getMa3 function in Ma3.R
#' @author Tiara Dewi
#' @export


getCa <-function (Ma1,Ma2,Ma3,I){
  Ca <- (Ma3-I) %*% (Ma2 %*% Ma1)
  return (Ca)
}

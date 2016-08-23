#' @title Endogen Accounts
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to exclude exogen accounts from data
#' @param baris1 number containing start row generate from user input
#' @param kolom1 number containing start column generate from user input
#' @param fsam data
#' @param cinta list containing row numbers of exogen accounts from user input
#' @author Tiara Dewi


getEndogen <- function (fsam,cinta,baris1,kolom1){
  newfsam <- fsam
  #menghilangkan baris eksogen
  for (i in 1:length(cinta)){
    newfsam <- newfsam[-(cinta[i]+1-i),]
  }
  #menghilangkan kolom eksogen
  selisih <- kolom1-baris1
  for (i in 1:length(cinta)){
    newfsam <- newfsam[,-(cinta[i]+selisih+1-i)]
  }
  return(newfsam)
}

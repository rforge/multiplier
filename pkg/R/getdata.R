#' @title Open File
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible because user choose Open File in FSAM at GUI Multiplier.
#' @details the data must be in Microsoft Excel form ".xlsx" and saved by name sam in global environment
#' @author Tiara Dewi
#' @import tcltk2
#' @import tcltk
#' @import utils
#' @export

#fungsi untuk mendapatkan data SAM
getsam <- function() {
  name <- tclvalue(tkgetOpenFile(
    #filetypes = "{ {XLSX Files} {.xls .xlsx} } { {All Files} * }"))
    filetypes = "{ {CSV Files} {.csv} } { {All Files} * }"))
  if (name == "")
    return(data.frame()) # Return an empty data frame if no file was selected
  #

  #data <- read.xlsx(name, sheetIndex = 1,header=FALSE)
  sam <<- read.csv (name, header=FALSE, sep=";")
  # importIntoEnv(self, data, .GlobalEnv, "sam")
  # assign("sam", data, envir = .GlobalEnv)
  cat("The imported data are in sam\n")
}

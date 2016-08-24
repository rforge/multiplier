#' @title Display Result
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible after user had choose either function of Accounting Multiplier SAM, Accounting Multiplier Decomposition SAM, Fixed Price Multiplier SAM or Accounting Multiplier FSAM.
#' @author Tiara Dewi
#' @import tcltk2
#' @param Ma Mutlipier matrix (accounting or fixed price both SAM or FSAM)
#' @param judul string indicate the title
#' @param kode 1 for accounting or fixed price, 2 for decomposition
#' @export

displayResult <- function (Ma,judul,kode) {
  tclRequire("BWidget")
  win1 <- tktoplevel(width="540",height="540",background="#ffdead")
  tktitle(win1) <- "Output"
  framejudul <- tk2frame(win1,height="40")
  frameresult <- tk2frame(win1,width="540",height="500")
  tkgrid(framejudul,sticky="nw")
  subjudul <- tclVar(judul)
  fontHeading <- tkfont.create(family = "Calibri", size = 14,weight = "bold" )
  framejudul$env$judul <- tk2label(framejudul, textvariable = subjudul, font=fontHeading,background="#ffdead")
  tkgrid(framejudul$env$judul, padx=10, pady=10, sticky="ew", columnspan=2)
  tkgrid(frameresult,sticky="nw")

  if (kode<=1){
    coln <- c(1:length(Ma))
    Ma <<- rbind(coln,Ma)
    rown <- c("", c(1:length(Ma)))
    Ma <<- cbind(rown,Ma)
  }
  else {
    #dimnames (Ma) <- list( 1:length(Ma), c("Ta","Oa","Ca"))
    colnames(Ma) <- c("Ta","Oa","Ca")
    #Ma <<- rbind(coln,Ma)
    #rownames(Ma) <-  c(1:length(Ma))
    #Ma <<- cbind(rown,Ma)
  }

  tclTable <- tclArray()
  for (i in 1:nrow(Ma))
    for (j in 1:ncol(Ma)){
      tclTable[[i-1, j-1]] <- Ma[i,j]
    }


  win1$env$table1 <- tk2table(frameresult, variable = tclTable, rows = nrow(Ma), cols = ncol(Ma),selectmode = "extended",
                              colwidth = 15 ,height = 15, width = 6, background = "white" ,xscrollcommand=function(...) tkset(xcsr,...),
                              yscrollcommand=function(...) tkset(ycsr,...))
  xcsr <- tk2scrollbar(frameresult,orient="horizontal", command=function(...) tkxview(win1$env$table1,...))
  ycsr <- tk2scrollbar(frameresult,orient="vertical", command=function(...) tkyview(win1$env$table1,...))
  tkgrid(win1$env$table1, ycsr)
  tkgrid.configure(ycsr, sticky = "nsw")
  tkgrid(xcsr, sticky = "new")
  simpan <- function (){
    filename <- tclvalue(tkgetSaveFile())
    if (!nchar(filename)) {
      tkmessageBox(message = "No file was selected!")
    } else {
      write.csv(x = as.data.frame(Ma),file = paste(filename,".csv"), col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
      #write.xlsx(x = as.data.frame(Ma),file = paste(filename,".xlsx"), sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
      tkmessageBox(message = paste("Output ", filename," successfully saved"))
    }
  }

  onClose <- function(){
    tkdestroy(win1)
  }

  frameresult$env$butSave <- tk2button(frameresult, text = "Save", width = -6, command = simpan)
  frameresult$env$butClose <- tk2button(frameresult, text = "Close", width = -6, command = onClose)
  tkgrid(frameresult$env$butClose,frameresult$env$butSave, padx=10,pady=10,sticky="e")

  if (kode>=2){
    urutan <- ("*The order of column : Transfer Loop, Open Loop, Closed Loop")
    frameresult$env$urutan <- tk2label(frameresult, textvariable = urutan, background="#ffdead")
    tkgrid (frameresult$env$urutan, padx=10,pady=10, sticky="w")
  }

  tcl("ttk::style", "configure", "TFrame", background="#ffdead")
  tcl("ttk::style", "configure", "TLabel", background="#ffdead")

}

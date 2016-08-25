#' @title  GUI Multiplier
#' @description this function to create GUI that call all the functions in this package
#' @details this package was build to calculate the multiplier of Social Accounting Matrix (SAM) and Financial Social Accounting Matrix (FSAM)
#' @author Tiara Dewi
#' @import tcltk2
#' @import tcltk
#' @export
gui <- function(){
  win2 <- tktoplevel(width="540",height="540")
  tktitle(win2) <- "Multiplier Gen"
  tkgrid.propagate(win2,F)
  tk2frame(win2,width="540",height="50")-> framemenu
  tkgrid(framemenu,sticky="nw")
  tkgrid.propagate(framemenu,F)
  tk2frame(win2,width="540",height="490")-> frameisi
  tkgrid(frameisi)
  tkgrid.propagate(frameisi,F)
  win2$env$menu <- tk2menu(framemenu)
  tkconfigure(win2, menu = win2$env$menu)
  win2$env$menuFile <- tk2menu(win2$env$menu, tearoff = FALSE)
  win2$env$menuAnalyze <- tk2menu(win2$env$menu, tearoff = FALSE)
  win2$env$menuHelp <- tk2menu(win2$env$menu, tearoff = FALSE)

  tkadd(win2$env$menuFile, "command", label = "Open File", command = function () getsam())
  tkadd(win2$env$menuFile, "command", label = "Exit", command = function() tkdestroy(win2))
  tkadd(win2$env$menu, "cascade", label = "File", menu = win2$env$menuFile)

  win2$env$menuAnalyzeSAM <- tk2menu(win2$env$menuAnalyze, tearoff = FALSE)
  tkadd(win2$env$menuAnalyzeSAM, "command", label = "Accounting Multiplier", command = function() getposisi())
  tkadd(win2$env$menuAnalyzeSAM, "command", label = "Accounting Multiplier Decomposition", command = function() getinjeksi())
  tkadd(win2$env$menuAnalyzeSAM, "command", label = "Fixed Price Multiplier", command = function() getFixedprice ())
  tkadd(win2$env$menuAnalyze, "cascade", label = "SAM", menu = win2$env$menuAnalyzeSAM)

  win2$env$menuAnalyzeFSAM <- tk2menu(win2$env$menuAnalyze, tearoff = FALSE)
  tkadd(win2$env$menuAnalyzeFSAM, "command", label = "Accouting Multiplier", command = function() getposisifsam())
  tkadd(win2$env$menuAnalyze, "cascade", label = "FSAM", menu = win2$env$menuAnalyzeFSAM)
  tkadd(win2$env$menu, "cascade", label = "Analyze", menu = win2$env$menuAnalyze)

  win2$env$menuPanduan <- tk2menu(win2$env$menu, tearoff = FALSE)
  win2$env$menuAbout <- tk2menu(win2$env$menu, tearoff = FALSE)
  tkadd(win2$env$menuHelp, "command", label = "Help", command = function() tkmessageBox(message =
                                                                                          "For using this program\nPlease upload file first\nThen choose Matriks Pengganda Neraca", icon = "info"))
  tkadd(win2$env$menuHelp, "command", label = "About", command = function() tkmessageBox(message="Program ini dibuat oleh\nTiara Ratna Dewi", icon = "info"))
  tkadd(win2$env$menu, "cascade", label = "Help", menu = win2$env$menuHelp)

  judul <- tclVar("WELCOME")
  subjudul <- tclVar("this package was created for\nAccounting multipler SAM\nFixed price multiplier SAM\nDecompositon of accounting multiplier SAM in 4 accounts\nAccounting multiplier FSAM")
  fontHeading <- tkfont.create(family = "Lucida", size = 24,weight = "bold" )
  fontHeadingsub <- tkfont.create(family = "Calibri", size = 14,slant="italic")

  frameisi$env$judul <- tk2label(frameisi, textvariable = judul, font=fontHeading)
  frameisi$env$subjudul <- tk2label(frameisi, textvariable = subjudul, font=fontHeadingsub)
  mulai <- tclVar("to start click\nFile >> Open File >> SAM\nor\nFile >> Open File >> FSAM")


  frameisi$env$mulai <- tk2label(frameisi, textvariable= mulai)

  tkgrid (frameisi$env$judul, padx=10, pady=10,sticky = "ew")
  tkgrid (frameisi$env$subjudul, padx=10, pady=10,sticky = "ew")
  tkgrid(frameisi$env$mulai, padx = 10, pady=c(100,1), sticky ="w")



  tcl("ttk::style", "configure", "TFrame", background="#ffdead")
  tcl("ttk::style", "configure", "TLabel", background="#ffdead")
  tcl("ttk::style", "configure", "TLabel", font="Calibri")
  tcl("ttk::style", "configure", "TButton", font="Calibri")
  tcl("ttk::style", "configure", "TButton", background="#ffdead")

}







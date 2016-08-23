#' @title Input User for Accounting Multiplier Decomposition of SAM
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible because user choose Accounting Multiplier Decomposition in SAM at GUI Multiplier
#' @author Tiara Dewi
#' @import tcltk2
getinjeksi <- function(){
  win1 <- tktoplevel(width="360",height="400", background = "#ffdead")
  injeksi <- tclVar("")
  baris <- tclVar("")
  win1$env$entInjeksi <-tk2entry(win1, width = "25", textvariable = injeksi)
  win1$env$entBaris <-tk2entry(win1, width = "25", textvariable = baris)

  labelText <- tclVar("Please input th injection")
  fontHeading <- tkfont.create(family = "Comic Sans", size = 18,weight = "bold")
  win1$env$label <- tk2label(win1, textvariable = labelText, font=fontHeading, background = "#ffdead")
  tkgrid(win1$env$label, padx=20, pady=20, sticky="ew")
  tkgrid(tk2label(win1,text="Injection",justify = "left", background = "#ffdead"), win1$env$entInjeksi, padx = 10, pady = 10)
  tkgrid(tk2label(win1,text="In row number",justify = "left", background = "#ffdead"), win1$env$entBaris, padx = 10, pady = 10)
  tktitle(win1)<- "Accounting Multiplier Decomposition of SAM"

  onOK <- function() {
    injeksi <- as.numeric(tclvalue(injeksi))
    baris <- as.numeric(tclvalue(baris))
    In <- getMatriksInjeksi(injeksi,baris,eksogen,baris_start,A)
    # print ("Matriks injeksi")
    # print (In)
    Ma1<-getMa1(baris_start,kolom_start,faktor_prod,institusi,eksogen,keg_prod,A)
    print(Ma1)
    Abintang <- getABintang (baris_start,faktor_prod,institusi,eksogen,keg_prod,A,Ma1)
    Abintang2 <- getABintang2 (Abintang)
    Abintang3 <- getABintang3 (Abintang, Abintang2)

    x<-nrow(A)
    I <-diag(x)

    Ma2 <- getMa2 (I, Abintang3)
    Ma3 <- getMa3 (I, Abintang, Abintang2)

    Ta <- getTa (Ma1,I)

    Oa <- getOa (Ma1,Ma2,I)

    Ca <- getCa (Ma1,Ma2,Ma3,I)

    result <- dekomposisi(In,Ca,Oa,Ta)
    print(result)
    tkdestroy(win1)
    displayResult(result, "Accounting Multiplier Decomposition of SAM", 2)
  }
  onCancel <- function(){
    tkdestroy(win1)
  }

  win1$env$butOK <-tk2button(win1, text = "Submit", width = -6, command = onOK)
  win1$env$butCancel <-tk2button(win1, text = "Cancel", width = -6, command = onCancel)
  tkgrid(win1$env$butOK, win1$env$butCancel, padx = 10, pady = c(10, 15))
  tkbind(win1$env$entName, "<Return>", onOK)
  tkfocus(win1)

}

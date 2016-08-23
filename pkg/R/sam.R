#' @title Input User for Accounting Multiplier SAM
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible because user choose Accounting Multiplier in SAM at GUI Multiplier
#' @author Tiara Dewi
#' @import tcltk2
getposisi <-function(){
  win1 <- tktoplevel(width="360",height="400", background = "#ffdead")
  kolom1 <- tclVar("")
  baris1 <- tclVar("")
  baris_t <- tclVar("")
  name <- tclVar("")
  institusi <- tclVar("")
  faktor <- tclVar("")
  kegiatan <- tclVar("")
  win1$env$entKolom1 <-tk2entry(win1, width = "25", textvariable = kolom1)
  win1$env$entBaris1 <-tk2entry(win1, width = "25", textvariable = baris1)
  win1$env$entBarisT <-tk2entry(win1, width = "25", textvariable = baris_t)
  win1$env$entName <-tk2entry(win1, width = "25", textvariable = name)
  win1$env$entInstitusi <-tk2entry(win1, width = "25", textvariable = institusi)
  win1$env$entFaktor <-tk2entry(win1, width = "25", textvariable = faktor)
  win1$env$entKegiatan <-tk2entry(win1, width = "25", textvariable = kegiatan)
  labelText <- tclVar("INPUT")
  fontHeading <- tkfont.create(family = "Comic Sans", size = 18,weight = "bold")
  win1$env$label <- tk2label(win1, textvariable = labelText, font=fontHeading, background = "#ffdead")
  tkgrid(win1$env$label, padx=20, pady=20,columnspan=2)
  tkgrid(tk2label(win1,text="Kolom Pertama",justify = "left",background = "#ffdead"), win1$env$entKolom1, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1,text="Baris Pertama",justify = "left",background = "#ffdead"), win1$env$entBaris1, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1,text="Baris Terakhir Faktor Produksi",justify = "left",background = "#ffdead"), win1$env$entFaktor, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1,text="Baris Terakhir Institusi",justify = "left",background = "#ffdead"), win1$env$entInstitusi, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1,text="Baris Terakhir Kegiatan Produksi", justify = "left",background = "#ffdead"), win1$env$entKegiatan, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1,text="Baris Total", justify = "left",background = "#ffdead"), win1$env$entBarisT, padx = 10, pady = 5,sticky="w")
  tkgrid(tk2label(win1, text = "Masukan neraca - neraca eksogen \n(jika lebih dari 1,pisahkan dengan koma):", justify = "left",background = "#ffdead"),
         win1$env$entName, padx = 10, pady = 5,sticky="w")
  tktitle(win1)<- "Matriks Pengganda Neraca SAM"

  onOK <- function() {
    nameVal <- as.numeric(tclvalue(name))
    baris1 <- as.numeric(tclvalue(baris1))
    assign("baris_start", baris1, envir = .GlobalEnv)
    kolom1 <- as.numeric(tclvalue(kolom1))
    assign("kolom_start", kolom1, envir = .GlobalEnv)
    baris_t <- as.numeric(tclvalue(baris_t))
    assign("baris_total", baris_t, envir = .GlobalEnv)
    eksogen <- tclvalue(name)
    faktor_prod <- as.numeric(tclvalue(faktor))
    assign("faktor_prod", faktor_prod, envir = .GlobalEnv)
    institusi <- as.numeric(tclvalue(institusi))
    assign("institusi", institusi, envir = .GlobalEnv)
    keg_prod <- as.numeric(tclvalue(kegiatan))
    assign("keg_prod", keg_prod, envir = .GlobalEnv)
    EXP <- as.numeric(strsplit(eksogen, ",")[[1]])
    assign("eksogen", EXP, envir = .GlobalEnv)
    newsam <- getEndogen(sam,EXP,baris_start,kolom_start)
    if (baris_t==nrow(sam)){
      #if(!exists("")){
      #   stop("no output string was specified")
      # } else {
        A <- adjustedmatrix(newsam,baris_start,kolom_start)
        #print ("adjusted dapat broh :")
        assign("A", A, envir = .GlobalEnv)
        print (A)
        Ma <<- accountingmultiplier(A)
        assign("Ma", Ma, envir= .GlobalEnv)
        tkdestroy(win1)
        displayResult(Ma, "Accounting Multiplier Matrix of SAM", 1)
      #}
    }
    else {
      tkmessageBox(message = "Wrong Input", icon = "error")
      tkdestroy(win1)
    }
  }
  onCancel <- function(){
    tkdestroy(win1)
  }
  # onHelp <- function(){
  #   tkdestroy(win1)
  # }

  win1$env$butOK <-tk2button(win1, text = "Submit", width = -6, command = onOK)
  win1$env$butCancel <-tk2button(win1, text = "Cancel", width = -6, command = onCancel)
  tkgrid(win1$env$butOK, win1$env$butCancel, padx = 10, pady = c(10, 15))
  bintangsatu <- tclVar("* exogen account for SAM are\ngoverment, asset dan rest of the world(ROW)")
  #bintangdua <- tclVar("**untuk melihat manual, \nlihat Bantuan pada halaman awal")
  #win1$env$butHelp <-tk2button(win1, text = "Help", width = -6, command = onHelp)
  win1$env$satu <- tk2label(win1, textvariable = bintangsatu,justify = "left",background = "#ffdead")
  #win1$env$dua <- tk2label(win1, textvariable = bintangdua,justify = "left",background = "#ffdead")
  tkgrid(win1$env$satu,padx = 10, pady=c(10,1), sticky ="w")
  #tkgrid(win1$env$dua,padx=10,pady=c(1,1), sticky="w")
  #tkgrid(win1$env$butHelp,padx=10,pady=c(1,10), sticky="w")
  tkbind(win1$env$entName, "<Return>", onOK)
  tkfocus(win1)

}

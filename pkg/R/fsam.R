#' @title Input User for Accounting Multiplier FSAM
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible beacuse user choose Accounting Multiplier in FSAM at GUI Multiplier
#' @author Tiara Dewi
#' @import tcltk2
#' @export

getposisifsam <- function (){
  win1 <- tktoplevel(width="360",height="400", background = "#ffdead")
  kolom1 <- tclVar("")
  baris1 <- tclVar("")
  baris_t <- tclVar("")
  name <- tclVar("")
  win1$env$entKolom1 <-tk2entry(win1, width = "25", textvariable = kolom1)
  win1$env$entBaris1 <-tk2entry(win1, width = "25", textvariable = baris1)
  win1$env$entBarisT <-tk2entry(win1, width = "25", textvariable = baris_t)
  win1$env$entName <-tk2entry(win1, width = "25", textvariable = name)
  labelText <- tclVar("INPUT")
  fontHeading <- tkfont.create(family = "Comic Sans", size = 18,weight = "bold")
  win1$env$label <- tk2label(win1, textvariable = labelText, font=fontHeading, background = "#ffdead")
  tkgrid(win1$env$label, padx=20, pady=20, columnspan=2)
  tkgrid(tk2label(win1,text="Start Column",justify = "left", background = "#ffdead"), win1$env$entKolom1, padx = 10, pady = 5, sticky="w")
  tkgrid(tk2label(win1,text="Start Row",justify = "left", background = "#ffdead"), win1$env$entBaris1, padx = 10, pady = 5, sticky="w")
  tkgrid(tk2label(win1,text="Total", justify = "left",background = "#ffdead"), win1$env$entBarisT, padx = 10, pady = 5, sticky="w")
  tkgrid(tk2label(win1, text = "Input exogen accounts \nSeparate with comma:", background = "#ffdead",justify = "left"),
         win1$env$entName, padx = 10, pady = 5, sticky="w")
  tktitle(win1)<- "Accounting Multiplier FSAM"

  onOK <- function() {
    nameVal <- as.numeric(tclvalue(name))
    baris1 <<- as.numeric(tclvalue(baris1))
    assign("baris_start", baris1, envir= .GlobalEnv)
    kolom1 <<- as.numeric(tclvalue(kolom1))
    assign("kolom_start", kolom1, envir= .GlobalEnv)
    baris_t <- as.numeric(tclvalue(baris_t))
    assign("baris_total", baris_t, envir= .GlobalEnv)
    eksogen <<- tclvalue(name)
    EXP <<- as.numeric(strsplit(eksogen, ",")[[1]])
    assign("EXP", EXP, envir= .GlobalEnv)
    #print(EXP)
    newfsam <<- getEndogen(sam,EXP,baris1,kolom1)
    assign("sam", newfsam, envir= .GlobalEnv)
    if (baris_t==nrow(sam)){
      Af <- adjustedmatrix(newfsam,baris1,kolom1)
      #print ("adjusted dapat broh :")
      #print (Af)
      Maf <<- accountingmultiplier(Af)
      assign("Ma", Maf, envir= .GlobalEnv)
      print(Maf)
      tkdestroy(win1)
      displayResult(Maf,"Accounting Multiplier of FSAM",1)
    }
    else {
      tkmessageBox(message = "Wrong Input", icon = "error")
      tkdestroy(win1)
    }
  }
  onCancel <- function(){
    tkdestroy(win1)
  }
  onHelp <- function(){
    tkdestroy(win1)
  }

  win1$env$butOK <-tk2button(win1, text = "Submit", width = -6, command = onOK)
  win1$env$butCancel <-tk2button(win1, text = "Cancel", width = -6, command = onCancel)
  tkgrid(win1$env$butOK, win1$env$butCancel, padx = 10, pady = c(10, 15))
  bintangsatu <- tclVar("* exogen account for FSAM are taxes,\ngoverment instruments, and rest of the world(ROW)")
  bintang <- tclVar("** row whose total 0 (zero) \nadded to exogen accounts \nbecause it won't be included")
  #bintangdua <- tclVar("***untuk melihat manual, \nlihat Bantuan pada halaman awal")
  #win1$env$butHelp <-tk2button(win1, text = "Help", width = -6, command = onHelp)
  win1$env$satu <- tk2label(win1, textvariable = bintangsatu,justify = "left",background = "#ffdead")
  win1$env$bintanf <- tk2label(win1, textvariable = bintang,justify = "left",background = "#ffdead")
  #win1$env$dua <- tk2label(win1, textvariable = bintangdua,justify = "left",background = "#ffdead")
  tkgrid(win1$env$satu, padx = 10, pady=c(10,1), sticky ="w")
  tkgrid(win1$env$bintanf, padx = 10, pady=c(10,1), sticky ="w")
  #tkgrid(win1$env$dua, padx=10,pady=c(1,1), sticky="w")
  #tkgrid(win1$env$butHelp,padx=10,pady=c(1,10), sticky="w")
  tkbind(win1$env$entName, "<Return>", onOK)
  tkfocus(win1)

}

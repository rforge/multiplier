#' @title Input User for Fixed Price Multiplier SAM
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible beacuse user choose Accounting Multiplier in FSAM at GUI Multiplier
#' @author Tiara Dewi
#' @import tcltk2
getFixedprice <- function(){
  win1 <- tktoplevel(width="360",height="400", background = "#ffdead")
  labelText <- tclVar("Input Elasticty Matrix")
  fontHeading <- tkfont.create(family = "Comic Sans", size = 18,weight = "bold")
  win1$env$label <- tk2label(win1, textvariable = labelText, font=fontHeading, background = "#ffdead")
  tkgrid(win1$env$label, padx=20, pady=c(10,1), columnspan=2)
  #kalau ada eksogen di tengah - tengah institusi
  # d<-0
  # for (i in 1:length(EXP)){
  #   if (institusi>=EXP[i]){
  #     d <- d+1
  #   }
  # }
  # print(d)
  # p <- institusi-faktor_prod-d

  #dimensi <- tclVar(paste(p,"x",p))
  #win1$env$dimensi <- tk2label(win1, textvariable = dimensi, font=fontHeading, background = "#ffdead")
  #tkgrid(win1$env$dimensi, padx=20, pady=c(1,20), columnspan=2)

  onNext <- function(){
    ei <<- data.frame()
    ei <<- as.matrix(edit(ei))
    # switch(ncol(ei),
    #        ncol(ei)=p,
    #        stop("Wrong column input"))
    # switch(nrow(ei),
    #        nrow(ei)=p,
    #        stop("Wrong row input"))
     if (ncol(ei)>p){
         tkmessageBox(message="Cek jumlah kolom\nmatriks elastisitas yang diinput", icon="error")
         tkdestroy(win1)
     }
     else if (ncol(ei)<p){
       tkmessageBox(message="Cek jumlah kolom\nmatriks elastisitas yang diinput", icon="error")
       tkdestroy(win1)
     }
     else if (nrow(ei)<p){
       tkmessageBox(message="Cek jumlah baris\nmatriks elastisitas yang diinput", icon="error")
       tkdestroy(win1)
     }
     else if (nrow(ei)<p){
       tkmessageBox(message="Cek jumlah baris\nmatriks elastisitas yang diinput", icon="error")
       tkdestroy(win1)
    }
     else{
    print(ei)
    Cn <- fixedprice(A,baris_start,faktor_prod,institusi,keg_prod,eksogen,ei)
    #print("########################################################")
    #print("matriks pengganda tetap :")
    #print(Cn)
    displayResult(Cn,"Fixed Price Multiplier of SAM",1)
    tkdestroy(win1)
    }
  }
  onCancel <- function(){
    tkdestroy(win1)
  }

  win1$env$butNext <-tk2button(win1, text = "Input", width = -6, command = onNext)
  win1$env$butCancel <-tk2button(win1, text = "Cancel", width = -6, command = onCancel)
  tkgrid(win1$env$butNext,win1$env$butCancel, padx=20,pady=20, sticky="ew")
  bintangsatu <- tclVar("*Matriks elastisitas rumah tangga\ndapat menggunakan model Working Lesser")
  #bintangdua <- tclVar("*untuk melihat manual, \nlihat Bantuan pada halaman awal")
  win1$env$satu <- tk2label(win1,textvariable = bintangsatu,justify = "left",background = "#ffdead")
  #win1$env$dua <- tk2label(win1,textvariable = bintangsatu,justify = "left",background = "#ffdead")
  tkgrid(win1$env$satu,padx=10,pady=c(10,1), columnspan=2,sticky="w")
  #tkgrid(win1$env$dua,padx=10,pady=c(1,1), columnspan=2,sticky="w")
  tktitle(win1)<- "Fixed Price Multiplier of SAM"
  tkfocus(win1)

}

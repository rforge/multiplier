#' @title Fixed Price Multiplier
#' @description support function to GUI Multiplier function. Not intended to be called directly. Visible after user input elasticity matrix.
#' @details this function is to calculate matrix C (marginal prospensity matrix)
#' @param A average prospensity matrix generate by adjustedmatrix function in averagprospensitymatrix.R
#' @param baris_start number containing start row generate from user input
#' @param faktor_prod number containing end row of production factors generate from user input
#' @param institusi number containing end row of institutions generate from user input
#' @param keg_prod number containing end row of economic activities generate from user input
#' @param EXP list containing row numbers of exogen accounts from user input
#' @param ei elasticiy matrix that user has input in getFixedprice function in getfixedprice.R
#' @author Tiara Dewi

fixedprice <- function (A,baris_start,faktor_prod,institusi,keg_prod,EXP,ei){

  #karena ada eksogen sebelum keg_prod
  e<-0
  for (i in 1:length(EXP)){
    if (keg_prod>EXP[i]){
      e <- e+1
    }
  }
  # print("e")
  # print(e)

  #kalau ada eksogen di tengah - tengah institusi
  d<-0
  for (i in 1:length(EXP)){
    if (institusi>=EXP[i]){
      d <- d+1
    }
  }
  # print("d")
  # print(d)

  x <- nrow(A)
  #i 17-32
  #j 10-16
  a = institusi + e - baris_start+1
  b = faktor_prod-baris_start+1+1
  c = institusi-d-baris_start+1
  A32 <- matrix(0,(x-a+1),(c-b+1))
  for(i in a:x)
  { for(j in b:c)
  {
    A32[i-(a-1),j-(b-1)]=A[i,j]
  }
  }
  #print(A32)

  C32 <- A32%*%ei
  C <- A
  for(i in a:x)
  { for(j in b:c)
  {
    C[i,j]=C32[i-(a-1),j-(b-1)]
  }
  }

  Cn <- accountingmultiplier(C)
  #print("Berikut ini \nMatriks Pengganda Tetap :")
  #print(Cn)
  return (Cn)
}

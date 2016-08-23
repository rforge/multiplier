#' @title A*
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate A* matrix for accounting multipier decomposition.
#' @param A average prospensity matrix generate by adjustedmatrix function in averagprospensitymatrix.R
#' @param Ma1 Ma1 matrix generate by getMa1 function in Ma1.R
#' @param baris_start number containing start row generate from user input
#' @param faktor_prod number containing end row of production factors generate from user input
#' @param institusi number containing end row of institutions generate from user input
#' @param keg_prod number containing end row of economic activities generate from user input
#' @param EXP list containing row numbers of exogen accounts from user input
#' @author Tiara Dewi


#Abintang matrix
#Abintang = Ma1 * Matriks A21 A13 A32
getABintang <- function (baris_start,faktor_prod,institusi,EXP,keg_prod,A,Ma1){
  #Buat Matriks A21 A13 A32 dulu
  x <- nrow(A)

  #kalo ada eksogen di tengah tengah institusi
  e<-0
  for (i in 1:length(EXP)){
    if (institusi>EXP[i]){
      e <- e+1
    }
  }

  #A21 institusi x faktor produksi
  #institusi 10-16
  #faktor produksi 1-9
  a = faktor_prod-baris_start+1 +1
  b = institusi- e -baris_start+1

  A21 <- matrix(0,x,x)
  for(i in a:b)
  { for(j in 1:a-1)
  {
    A21[i,j]=A[i,j]
  }
  }

  #kalau ada eksogen sebelum baris terakhir kegiatan produksi
  d<-0
  for (i in 1:length(EXP)){
    if (keg_prod>EXP[i]){
      d <- d+1
    }
  }

  #A13 faktor produksi x kegiatan produksi
  #faktor produksi 1-9
  #keg produksi 17-32
  a = faktor_prod-baris_start+1
  b = keg_prod-baris_start+1-d
  c = institusi-baris_start+1-e+1
  A13 <- matrix(0,x,x)
  for(i in 1:a)
  { for(j in c:b)
  {
    A13[i,j]=A[i,j]
  }
  }

  #A32 kegiatan produksi x institusi
  #keg_prod berarti dari 17-32
  #institusi berarti dari 10-16


  a = institusi + e - baris_start+1#16
  b = faktor_prod-baris_start+1+1#10
  c = keg_prod-baris_start+1-d#32
  A32 <- matrix(0,x,x)
  for(i in (a+1):c)
  { for(j in b:a)
  {
    A32[i,j]=A[i,j]
  }
  }


  C <- A13 + A21 + A32
  #write.xlsx(C,"D:/SKripsi/YOUR SKRIPSWEAT/validasi/C.xlsx", sheetName = "Sheet1")
  #print (Matriks C)
  Abintang <- Ma1%*%C
  #write.xlsx(Abintang,"D:/SKripsi/YOUR SKRIPSWEAT/validasi/ABintang.xlsx", sheetName = "Sheet1")
  return (Abintang)
}

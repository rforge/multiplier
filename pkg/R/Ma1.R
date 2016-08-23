#' @title Ma1
#' @description support function to GUI Multiplier function. Not intended to be called directly.
#' @details this function is to calculate Ma1 matrix for accounting multipier decomposition.
#' @param A average prospensity matrix generate by adjustedmatrix function in averagprospensitymatrix.R
#' @param baris_start number containing start row generate from user input
#' @param kolom_start number containing start column generate from user input
#' @param faktor_prod number containing end row of production factors generate from user input
#' @param institusi number containing end row of institutions generate from user input
#' @param keg_prod number containing end row of economic activities generate from user input
#' @param EXP list containing row numbers of exogen accounts from user input
#' @author Tiara Dewi

getMa1 <- function(baris_start,kolom_start,faktor_prod,institusi,EXP,keg_prod,A){
  #Matriks Ma1
  #bikin matriks I

  x <- nrow(A)
  I <- diag(x)

  #jika ada eksogen dalam institusi
  c <- 0
  for (i in 1:length(EXP)){
    if (institusi>EXP[i]){
      c <- c+1
    }
  }
  #jika ada eksogen sebelum kegiatan produksi
  d <- 0
  for (i in 1:length(EXP)){
    if (keg_prod>EXP[i]){
      d <- d+1
    }
  }
  #A22 yaitu matriks institusi x institusi
  #institusi 10-16
  a = faktor_prod-baris_start+1 +1
  b = institusi- c -baris_start+1
  A22 <- matrix(0,x,x)
  for(i in a:b)
  { for(j in a:b)
  {
    A22[i,j]<-A[i,j]
  }
  }


  #A33 yaitu keg produksi x keg produksi
  #keg produksi 17-32
  print(c)
  print(d)
  e = institusi+1-baris_start-c
  f = keg_prod+1-baris_start-d
  A33 <- matrix(0,x,x)
  for(i in e:f)
  { for(j in e:f)
  {
    A33[i,j]<-A[i,j]
  }
  }


  B <- A22 + A33
  #write.xlsx(B,"D:/SKripsi/YOUR SKRIPSWEAT/validasi/B.xlsx", sheetName = "Sheet1")
  Ma1 <- solve(I-B)
  #write.xlsx(Ma1,"D:/SKripsi/YOUR SKRIPSWEAT/validasi/Ma1.xlsx", sheetName = "Sheet1")
  #print("################################# Ma1 ##############################")
  #print(Ma1)
  return (Ma1)
}

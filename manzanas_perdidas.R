manzanas_perdidas <- function(manz_cuestionario,manz_INE,n){
  
  conjunto <- unique(manz_cuestionario)
  count <- 0
  count_no <- 0
  no <- c()
  si_estan <- c()
  for (i in 1:as.integer(n)){
    if(conjunto[i] %in% manz_INE){
      print(paste(conjunto[i], ", si esta", sep =""))
      count <- 1 + count
      si_estan <- c(si_estan, conjunto[i])
      
    }else{
      print(paste(conjunto[i], ", no esta", sep =""))
      count_no <- 1 + count_no
      no <- c(no, conjunto[i])
      
    }
    
  }
  return(no)
  
}
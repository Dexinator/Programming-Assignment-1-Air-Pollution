formnom<-function(num){
  corr_number<-c()
  for (j in num){
    if (nchar(j)==1) {
      corr_number<-append(corr_number,paste("00",as.character(j),sep = ""))
    }else if (nchar(j)==2){
      corr_number<-append(corr_number,paste("0",as.character(j),sep = ""))
    }else{
      corr_number<-append(corr_number,as.character(j))
    }
  }
  return(corr_number)
}

observ_amount<-function(directory='A:/Downloads/rprog_data_specdata/specdata/',id=1:332){
  real_IDS<-formnom(id)
  monitor_ID<-c()
  nobs<-c()
  for (i in real_IDS){
    archivo<-read.csv(paste(x,formnom(i),".csv",sep="")) ##Asignamos los datos de ese monitor
    
    monitor_ID<-append(monitor_ID,i)
    nobs<-append(nobs,length(archivo$sulfate[!is.na(archivo$sulfate)]))
    }
  data.frame(monitor_ID,nobs)
}
resultado<-observ_amount(id=c(2, 4, 8, 10, 12))
resultado
str(resultado)

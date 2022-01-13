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
formnom(c(2,5,100))

x<-'A:/Downloads/rprog_data_specdata/specdata/'
num_monitor<-5
archivo<-read.csv(paste(x,formnom(num_monitor),".csv",sep=""))
mean(archivo$nitrate, na.rm = TRUE)

pollutantmean<-function(directory='A:/Downloads/rprog_data_specdata/specdata/',pollutant,id){
  archivo<-read.csv(paste(x,formnom(id),".csv",sep="")) ##Asignamos los datos de ese monitor

    if (pollutant=="sulfate"){
    mean(archivo$sulfate, na.rm = TRUE)
  }else if(pollutant=="nitrate"){
    mean(archivo$nitrate, na.rm = TRUE)
  }
}
pollutantmean(pollutant = "nitrate",id=5)


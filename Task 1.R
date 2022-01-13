formnom<-function(num){
  if (nchar(num)==1) {
    return( paste("00",as.character(num),sep = ""))
  }else if (nchar(num)==2){
    return( paste("0",as.character(num),sep = ""))
  }else{
    return (as.character(num))
  }
  
}


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

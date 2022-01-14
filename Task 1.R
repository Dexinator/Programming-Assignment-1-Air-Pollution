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

pollutantmean<-function(directory='A:/Downloads/rprog_data_specdata/specdata/',pollutant,id=1:332){
  real_IDS<-formnom(id)
  suma<-0
  n<-0
    for (i in real_IDS){
  archivo<-read.csv(paste(x,formnom(i),".csv",sep="")) ##Asignamos los datos de ese monitor

    if (pollutant=="sulfate"){
      suma<-suma+sum(archivo$sulfate, na.rm = TRUE)
      n<-n+length(archivo$sulfate[!is.na(archivo$sulfate)])
  }else if(pollutant=="nitrate"){
    suma<-suma+sum(archivo$nitrate, na.rm = TRUE)
    n<-n+length(archivo$nitrate[!is.na(archivo$nitrate)])
  }}
  suma/n
}
pollutantmean(pollutant = "nitrate",id=c(1:332))


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





corr_number
chartest<-c("hola", "a", "hambe")
nchar(chartest)==1
ij<-0
for (i in chartest){
    ij<-ij+1
}
num3<-c()

append(num3,"500")
num2<-c(5,2,30)
longnum2<-length(num2)
longnum2
formnom(num2)


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


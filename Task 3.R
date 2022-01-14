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
    archivo<-read.csv(paste(directory,formnom(i),".csv",sep="")) ##Asignamos los datos de ese monitor
    x3<-0
    monitor_ID<-append(monitor_ID,i)
    for (p in 1:length(archivo$Date)){
      x3<-append(x3,!is.na(archivo$sulfate[p]) & !is.na(archivo$nitrate[p])) #verificamos que ninguno de los dos valores sea NA
    }
    
    nobs<-append(nobs,sum(x3))
  }
  data.frame(monitor_ID,nobs)
}

corr<-function(directory='A:/Downloads/rprog_data_specdata/specdata/',threshold){
  test_results<-c()
  for (i in 1:332){
    observ<-observ_amount(id=i)
    if (observ$nobs>=threshold){
      archivo<-read.csv(paste(directory,formnom(i),".csv",sep=""))
      archivo2<-archivo[!is.na(archivo$sulfate) & !is.na(archivo$nitrate),c(2,3)]
      test_results<-append(test_results,cor(archivo2$sulfate,archivo2$nitrate))  
    }
  }
if (length(test_results)>0){
return (test_results)  
}else{
  return(0)
}
  }
cr<-corr(threshold=150)
head(cr)



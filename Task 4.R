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


find_corr<-function(directory='A:/Downloads/rprog_data_specdata/specdata/',threshold){
  conjunto<-data.frame(sulfate=numeric(),numeric())
  ID<-0
  while (length(conjunto[,1])<threshold){
    ID<-ID+1
    x<-read.csv(paste(directory,formnom(ID),".csv",sep=""))
    x2<-x[!is.na(x$sulfate) & !is.na(x$nitrate),c(2,3)]
    conjunto<-rbind(conjunto,x2)
  }

  conjunto
}

cr<-find_corr(threshold=400)
cor(cr)
cor(cr$sulfate[1],cr$nitrate[1])
directory='A:/Downloads/rprog_data_specdata/specdata/'
test_results<-c()
limit<-1095
for (i in 1:332){
  observ<-observ_amount(id=i)
if (observ$nobs>=limit){
  archivo<-read.csv(paste(directory,formnom(i),".csv",sep=""))
  archivo2<-archivo[!is.na(archivo$sulfate) & !is.na(archivo$nitrate),c(2,3)]
  test_results<-append(test_results,cor(archivo2$sulfate,archivo2$nitrate))  
}
  
}
test_results



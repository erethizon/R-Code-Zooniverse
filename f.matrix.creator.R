#diagnose f.matrix.creator and get to work for our data.

f.matrix.creator<-function(data){
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sampling units
  cams<-unique(data$Sampling.Unit.Name)
  cams<-sort(cams)
  rows<-length(cams)
  
  
  
  data$Start.Date<-data$Date_Out
  data$End.Date<-data$Use_End_Date
  #start and end dates of sampling periods
  
  min<-min(data$Start.Date)
  max<-max(data$End.Date)
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by=1)
  mat_colnames<-as.character(date.header)
  mat<-matrix(NA,rows,cols,dimnames=list(cams,mat_colnames))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$Start.Date),data$Sampling.Unit.Name,unique)
  end.dates<-tapply(as.character(data$End.Date),data$Sampling.Unit.Name,unique)
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==start.dates[[j]])
    hi<-which(date.header==end.dates[[j]])
    indx<-seq(from=low,to=hi)
    mat[j,indx]<-0
  }
  mat.template<-mat
  #get the species
  species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$Photo.Date[indx]
    cameras<-data$Sampling.Unit.Name[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}


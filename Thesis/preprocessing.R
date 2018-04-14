transform <-function(f)
{
  comp(f)
  mypath=file.path("C:", "Users", "stavros", "Documents","διπλωματική","data","2nd part")
  ####################################
  #### TRANFORM
  ###################################
  lgcl<-estimateLogicle(f,colnames(f)[3:12])
  f.trans<-transform(f,lgcl)
  #f.trans<-flowTrans(f.clean, "mclMultivArcSinh",colnames(f.clean)[3:12], n2f=FALSE, parameters.only=FALSE)$result
  #asinhTrans <- arcsinhTransform(transformationId="ln-transformation", a=1, b=1, c=1)
  #f.trans<-f.clean
  #biexpTrans <- flowJoTrans(channelRange=4096, maxValue=262144 , pos=4.5,neg=0, widthBasis=-10)
  #tf <- transformList(colnames(f.clean)[3:12], biexpTrans)
  #f.trans<-transform(f.clean,tf)
  trans<<-f.trans
  str<-f@description$GUID
  str<-gsub(".fcs","",str)
  str<-paste(mypath,str,"DensPlotTrans.png",sep="/")
  png(file = str)
  par( mfrow = c(4 ,3) ,mar = c(2, 2, 2, 1))
  for (i in 3:12){
    plotDens ( f.trans , c(i,2))
  }
  dev.off()
}

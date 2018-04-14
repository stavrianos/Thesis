my_normalize<-function(lb,pol,nv,graphics=TRUE)
{
  frames<-list("Lympho B"=lb,"Polarisation"=pol,"Naive Memory"=nv)
  fs<-as(frames,"flowSet")
  normalized<<-warpSet(fs, colnames(fs), grouping = NULL, monwrd = TRUE, subsample=NULL,peakNr=NULL, clipRange=0.01, nbreaks=11, bwFac=2,warpFuns=FALSE,target=NULL)
  cat("normalize...Done\n")
  if(graphics==TRUE)
  {
    cat("Let's have some pretty plots\n")
    cat("DensityPlot of all normalized data\n")
    d2 <- densityplot(~ ., normalized, main="normalized")
    png(file="normalized.png")
    plot(d2)
    dev.off()
  }
  
}




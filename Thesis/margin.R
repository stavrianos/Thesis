margin<-function(f,graphics=TRUE)
{
  mypath=getwd()
  ##################################
  ####Remove margin events and debris
  ##################################
  fs.margin.cells<-intersect ( which ( exprs (f)[, "FSC-A"] < 250) , which(exprs (f)[, "FSC-A"] > 10) )
  fs.margin.percent<-100*length(fs.margin.cells)/ nrow(f)
  100-fs.margin.percent
  ss.margin.cells<-intersect ( which ( exprs (f)[, "SSC-A"] >10) ,which ( exprs (f)[, "SSC-A"] < 250) )
  ss.margin.percent<-100*length(ss.margin.cells)/ nrow(f)
  100-ss.margin.percent
  non.debris.indices<-intersect(fs.margin.cells,ss.margin.cells)
  f.clean<-f[ non.debris.indices ]
  ##################################
  #######Ploting 
  #################################
  if(graphics==TRUE)
  {
    A<-exprs(f)[, c(1,2)]
    str<-f@description$GUID
    str<-gsub(".fcs","",str)
    str<-paste(mypath,str,"removing_margin_events.png",sep="/")
    png(file = str)
    plot (f , c("FSC-A", "SSC-A"), smooth= FALSE )
    points(A[ -fs.margin.cells , ], pch=".", col = "red", cex =2)
    points(A[ -ss.margin.cells , ], pch=".", col = "red", cex =2)
    par(xpd=TRUE)
    legend("topleft",inset=c(0,-0.22),legend=paste(" Margin Events for FS :",100-fs.margin.percent ,  "%"), col = "red", pch = 19)
    legend("topright",inset=c(0,-0.11),legend=paste(" Margin Events for SS" ,100-ss.margin.percent, "%"), col = "red", pch = 19)
    dev.off()
    ##################################
    ####Ploting flowDensity
    ##################################
    par(xpd=FALSE)
    str<-f@description$GUID
    str<-gsub(".fcs","",str)
    str<-paste(mypath,str,"DensPlotClean.png",sep="/")
    png(file = str)
    plotDens(f.clean,c("FSC-A","SSC-A"))
    dev.off()
  }
 
  return(f.clean)
}
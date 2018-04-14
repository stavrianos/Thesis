thesis <- function(graphics=TRUE) {
  ptm <- proc.time()[3]
  if(graphics==FALSE)
  {
    graphics.off()
  }
  #############################
  ############IMPORT DATA
  ############################
  cat("Import Data(0/3)\n")
  lb <<- read.FCS("Lympho B.fcs", transformation=FALSE);
  cat("Import Data(1/3)\n")
  nv<<- read.FCS("Naive Memory.fcs", transformation=FALSE);
  cat("Import Data(2/3)\n")
  pol<<- read.FCS("Polarisation.fcs", transformation=FALSE);
  cat("Import Data(3/3)\n")
  ####################################################
  ##############CHANGING NAMES TO COLUMNS
  ####################################################
  colnames(lb)<<-c("FSC-A","SSC-A","FL1-H","FL2-H","FL3-H","FL4-H","FL5-H","FL6-H","FL7-H","FL8-H","FL9-H","FL10-H","TIME","SSC-H","SSC-W","FSC-W")
  colnames(pol)<<-c("FSC-A","SSC-A","FL1-H","FL2-H","FL3-H","FL4-H","FL5-H","FL6-H","FL7-H","FL8-H","FL9-H","FL10-H","TIME","SSC-H","SSC-W","FSC-W")
  colnames(nv)<<-c("FSC-A","SSC-A","FL1-H","FL2-H","FL3-H","FL4-H","FL5-H","FL6-H","FL7-H","FL8-H","FL9-H","FL10-H","TIME","SSC-H","SSC-W","FSC-W")
  ##################################################
  ####  Remove Time
  ##################################################
  lb=lb[-13]
  pol=pol[-13]
  nv=nv[-13]
  ##############################################
  ######################QUALITY ASSURANCE
  ###############################################
  if (graphics==TRUE)
  {
    cat("Quality Assurance(0/1)\n")
    QA(lb,pol,nv)
    cat("Quality Assurance(1/1)\n")
  }

  ################################################
  #####DENSITY PLOT
  ################################################
  frames<-list("Lympho B"=lb,"Polarisation"=pol,"Naive Memory"=nv)
  fs<-as(frames,"flowSet")
  if (graphics==TRUE)
  {
    cat("DensityPlot of all raw data\n")
    d1 <- densityplot(~ ., fs, main="original")
    png( file = "DensityPlot_of_all_raw_data.png")
    plot(d1)
    dev.off()
  }
  #############################################
  #########MArgin 
  ############################################
  lb<-margin(lb,graphics)
  pol<-margin(pol,graphics)
  nv<-margin(nv,graphics)
  ###############################################
  ##### tranform
  ###############################################
  cat("tranforming(0/3)\n")
  transforming(lb.comp,graphics)
  lb.trans<<-trans
  cat("tranforming(1/3)\n")
  transforming(pol.comp,graphics)
  pol.trans<<-trans
  cat("tranforming(2/3)\n")
  transforming(nv.comp,graphics)
  nv.trans<<-trans
  cat("tranforming(3/3)\n")
  ################################################################
  ###QUALITY ASSURANCE FOR TRANSFORMED DATA 
  ################################################################
  cat("Quality assurance for tanformed Data(0/1)\n")
  if (graphics==TRUE)
  {
      QATrans(lb.trans,pol.trans,nv.trans)
  }
  cat("Quality assurance for tanformed Data(1/1)\n")
  #############################################################
  ##########DENSITY PLOT
  #############################################################
  cat("DensityPlot of all transformed data\n")
  frames<-list("Lympho B"=lb.trans,"Polarisation"=pol.trans,"Naive Memory"=nv.trans)
  fs.trans<<-as(frames,"flowSet")
  if (graphics==TRUE)
  {
    d1 <- densityplot(~ ., fs.trans, main="transformed")
    png(file="Tranformed.png")
    plot(d1)
    dev.off()
  }
  ################################
  #####NORMALIZE
  #################################
  cat("normalize...\n")
  my_normalize(lb.trans,pol.trans,nv.trans,graphics)
  lb.norm<<-normalized[[1]]
  pol.norm<<-normalized[[2]]
  nv.norm<<-normalized[[3]]
  fs.norm<<-normalized
  ##############################################################
  #####CLUSTERING 
  ##############################################################
  cat("Clustering...\n")
  subSpace(fs.norm)
  cat("Clustering...OK\n")
  cat("THE END\n")
  total<-proc.time()[3] - ptm
  cat("Finished at ",round(total,digits=4)," sec\n\n")
}

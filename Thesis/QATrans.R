QATrans<-function(lb,pol,nv)
{
  #Density check: In multi-tube/plate experiments, sometimes
  #the same donor's sample is divided up and analyzed multiple
  #times using different staining. In such a case we expect the
  #FSC and SSC densities to look almost identical across 
  #tubes/wells-if one looks very different, this could be due to a
  #technical issue with acquisition and that tube/well should be
  #flagged for inspection or removed from further analysis
  png(file="QATrans.png")
  plot(density(exprs(lb) [,"FSC-A"]) ,xlim =c (0 , 1100) , ylim = c(0 ,0.01 ), lwd =2, main = "FSC Density ", sub="", xlab ="FSC -A")
  lines(density(exprs(pol)[,"FSC-A"]) , col=2, lwd=2)
  lines(density(exprs(nv)[,"FSC-A"]) , col=3, lwd=2)
  dev.off()
}

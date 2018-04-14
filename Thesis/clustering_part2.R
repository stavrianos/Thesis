clustering<-function(x)
{
  lb.clust<<-flowClust(x[[1]], varNames=NULL, K=2, B=100, trans=0)
  pol.clust<<-flowClust(x[[2]], varNames=NULL, K=2, B=100, trans=0)
  nv.clust<<-flowClust(x[[3]], varNames=NULL, K=2, B=100, trans=0)
}

Peaks<-function(x)
{
  lb.peaks<<-flowPeaks(x[[1]]@exprs)
  pol.peaks<<-flowPeaks(x[[2]]@exprs)
  nv.peaks<<-flowPeaks(x[[3]]@exprs)
}

Means<-function(x)
{
  lb.means <<- flowMeans(x[[1]], varNames=colnames(x[[1]]), MaxN=8,NumC=NA);
  pol.means <<- flowMeans(x[[2]], varNames=colnames(x[[2]]),MaxN=8,NumC=NA);
  nv.means <<- flowMeans(x[[3]], varNames=colnames(x[[3]]), MaxN=8,NumC=NA);
}

subSpace<-function(x)
{
  lb.clust<<-orclus(x = x[[1]]@exprs, k = 2, l = 1, k0 = 8, a = 0.5)
  pol.clust<<-orclus(x = x[[2]]@exprs, k = 2, l = 1, k0 = 8, a = 0.5)
  nv.clust<<-orclus(x = x[[3]]@exprs, k = 2, l = 1, k0 = 8, a = 0.5)
}
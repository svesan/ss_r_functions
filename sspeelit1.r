#-- covex hull peeling
ss.peelit <- function(x, y, points=TRUE, target=c(0.98,0.95,0.9,0.75,0.5,0.25,0.1), print=FALSE, names=NULL, title=TRUE, cex=1, col="gray", labels=TRUE, prefix=TRUE) {

  # x, y...: Mandatory. The x and y vectors. Non-missing will be removed by the program
  # points.: Optional. TRUE or FALSE to plot the points outside the outer hull
  # target.: Optional. A vector with percentiles. Must be in the (0,1] interval.
  # names..: Optional. Two row character vector with labels for x and y to be used in the plot.
  # title..: Optional. TRUE of FALSE to print title
  # cex....: Optional. Size scaling factor for text
  # col....: Optional. Color for the plotted symbols
  # labels.: Optional. True or False for printing percentiles on contours
  # prefix.: Optional. True or False to include prefix and subtitle in titles

  tmp<-as.matrix(na.omit(data.frame(x,y)))

  if (is.null(names)==FALSE) colnames(tmp)<-names

  if (title==TRUE & prefix==TRUE) {
    t1<-paste(c("Convex hull peeling ",colnames(tmp)[1]," vs ",colnames(tmp)[2]),collapse="")
    t2<-paste(c("Regions containing the central",target*100,"% of the data"),collapse=" ")
  }
  else if (title==TRUE & prefix==FALSE) {
    t1<-paste(c(colnames(tmp)[1]," vs ",colnames(tmp)[2]),collapse="")
  }

  n<-NROW(tmp)
  plot(tmp,type="n", cex.lab=cex, cex.axis=cex)

  pct<-NA;i<-0;j<-1;found<-0;tmp.plt<-matrix(NA,ncol=2)
  while(NROW(tmp)>2 & j<=NROW(target)){
    a<-tmp[,1];b<-tmp[,2]
    i<-i+1
    i1<-chull(tmp)
    pct0<-NROW(tmp)/n
    pct<-c(pct,pct0)

    if (pct0==target[j]) {
      i1b<-c(i1,i1[1]);
      polygon(a[i1b],b[i1b],col="white")
      if (labels==T) text(a[i1[1]],b[i1[1]],target[j],pos=4)
      j<-j+1;found<-1
    }
    else if(pct0<target[j] & pct[i]>target[j]){
      i1b<-c(i1,i1[1]);
      polygon(a[i1b],b[i1b],col="white")
      if (labels==T) text(a[i1[1]],b[i1[1]],target[j],pos=4)
      j<-j+1;found<-1
    }
    else if(found==0) tmp.plt<-rbind(tmp.plt,matrix(tmp[row(tmp) %in% i1],ncol=2))
    tmp<-matrix(tmp[!row(tmp) %in% i1],ncol=2)
  }
  if (points==TRUE) points(tmp.plt,pch=20,col=col)

  if (title==TRUE & prefix==TRUE) title(c(t1,t2), cex.main=cex, cex.sub=cex)
  else if (title==TRUE & prefix==FALSE) title(t1, cex.main=cex)

  if (print==TRUE) print(pct)
  #--- End of function ----
}


#source(file="h:/mep/sasproj/WLH/WLH0301/other/read_nuts.r")
#nuts2<-study.nuts2
#rm(study.nuts2)

#png(filename="~/R/peelit%03d.png", width=1200,height=1200)
#png(filename="h:/R/nuts%03d.png", width=1200,height=1200)
#par(mfrow=c(3,2))

#attach(nuts2)
#ss.peelit(n2,n30,names=c("Energy","Saturated Fat"), cex=1.4)
#ss.peelit(n2,n29,names=c("Energy","Alcohol"), cex=1.4)
#ss.peelit(n2,n1,names=c("Energy","Acrylamide"), cex=1.4)
#ss.peelit(n2,n51,names=c("Energy","Fibre"), cex=1.4)
#ss.peelit(n1,n30,names=c("Acrylamide","Saturated Fat"), cex=1.4)
#ss.peelit(n1,n29,names=c("Acrylamide","Alcohol"), cex=1.4)
#ss.peelit(n1,n2,names=c("Acrylamide","Fibre"), cex=1.4)
#ss.peelit(n30,n29,names=c("Saturated Fat","Alcohol"), cex=1.4)
#ss.peelit(n30,n2,names=c("Saturated Fat","Fibre"), cex=1.4)
#ss.peelit(n29,n2,names=c("Alcohol","Fibre"), cex=1.4)

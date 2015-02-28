phest<-function(est, cov, pos, label, alpha=0.05, print.all = FALSE){
# est....vector with estimates
# cov....covariance matrix
# pos....vector with positions and sign for estimate. E.g c(-2,3) is 0 -1 1
# label..label attached to the estimates
# The pos matrix must take NA instead of zero as in SAS

if (is.null(est) | is.null(cov) | is.null(pos)){
 stop('Parameters phest(est,cov,pos) must all be entered')
}

if (class(cov) != "matrix") stop('Parameter cov must be of type matrix')
if (NROW(cov) != NCOL(cov)) stop('Covariance matrix must be square')
if (NROW(est) != NROW(cov)) stop('Parameter vector not the same dimension as covariance matrix')
if (alpha>0.99 | alpha<0.01) stop('Parameter alpha not between 0.01 and 0.99')

c1<-matrix(0,ncol=NROW(est),nrow=NROW(pos))
for (i in 1:NROW(pos)) c1[i,abs(pos[i,])] <- sign(pos[i,])
c2<-c1%*%est
c3<-c1%*%cov%*%t(c1)
c4<-sqrt(c3[col(c3)==row(c3)])
c5<-c2-c4*qnorm(1-alpha/2)
c6<-c2+c4*qnorm(1-alpha/2)
c8<-2*(1-pchisq((c2/c4)**2,1))
c7<-cbind(round(cbind(exp(c2),c4,exp(c5),exp(c6),c8),digits=4))
#writeLines(paste("Estimated Wald type risk ratios and associated two-sided",100*(1-alpha),"% confidence intervals"))
dimnames(c7)<-list(label,c("Risk Ratio","SD","Lower","Upper","P-value"))

if (print.all)
  list(coefficients=est, estimates=c2, x=c7,
       lower=c5, upper=c6, cov=c3,
       estimates.exp=exp(c2), lower.exp=exp(c5), upper.exp=exp(c6),
       alpha=alpha, contrasts=c1)
}

r1<-c(9,-10)
r2<-c(-10)
rm<-matrix(NA, ncol=2,nrow=2)
rm[1,1:2]<-c(9,-10) #Former vs never
rm[2,1]<--10 #current vs never
rm[3,1]<- 2
rm[4,1]<- 3
rm[5,1]<- 4

lbl<-c("OC Former vs never","Current vs never")
f4.cov <- vcov(f4)

ha<-phest(f4$coefficients,f4.cov,rm,lbl,print.all=T)

#-- Plot alt 1
id<-factor(1:2,levels=1:2,label=lbl)
id.num<-as.numeric(id)
plot(xx[,1], id, xlim=c(min(xx[,3]),max(xx[,4])),ylab="",xlab="", ylim=c(min(id.num)*0.9,max(id.num)*1.1), axes=F)
axis(2,at=id,labels=levels(id),las=1,pos=c(min(xx[,3],0)))
rect(xx[,3],as.numeric(id)-0.05,xx[,4],as.numeric(id)+0.05,border="black")
axis(1)

#-- Plot alt 2

phest.plot <- function(xx, labels=""){
  if (is.null(labels)==TRUE & is.null(dimnames(xx)[[1]])==FALSE) labels <- dimnames(xx)[[1]]
  else if (is.null(labels)==TRUE & is.null(dimnames(xx)[[1]])==FALSE) labels <- as.character(1:NROW(xx))
  id<-factor(1:NROW(xx), levels=1:NROW(xx), label=labels)
  id.num<-as.numeric(id)
  plot(xx[,1], id, xlim=c(min(xx[,3]),max(xx[,4])),ylab="",xlab="",
  ylim=c(min(id.num)*0.9,max(id.num)*1.1), axes=F)
  #axis(2,at=id,labels=levels(id))
  rect(xx[,3],as.numeric(id)-0.05,xx[,4],as.numeric(id)+0.05,border="black")
  axis(1)
  text(xx[,1], id.num+0.05, levels(id), pos=3)
}

phest.plot(xx,labels=lbl)

#  if (labels == "" & dimnames(xx)[[1]] != "") labels=dimnames(xx)[[1]]
#  elseif (labels == "" & dimnames(xx)[[1]] != "") labels=1:NROW(xx)

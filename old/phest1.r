phest<-function(est, cov, pos, label, alpha=0.05, print.all = FALSE, digits=3){
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
c7<-cbind(round(cbind(exp(c2),c4,exp(c5),exp(c6)),digits=digits))
c8<-(1-pnorm(c2/c4))*2
dimnames(c7)<-list(label,c("Risk Ratio","SD","Lower","Upper"))

if (print.all) {
  writeLines(paste("Estimated Wald type risk ratios and associated two-sided",100*(1-alpha),"% confidence intervals"))
  print(c7)
}
  list(coefficients=est, estimates=c2, x=c7,
       lower=c5, upper=c6, cov=c3,
       estimates.exp=exp(c2), lower.exp=exp(c5), upper.exp=exp(c6),
       alpha=alpha, contrasts=c1, pval=c8)
}

#

#r1<-c(18,-19)
#r2<-c(1)
#rm<-matrix(NA, ncol=2,nrow=5)
#rm[1,1:2]<-c(18,-19) #Former vs never
#rm[2,1]<--19 #current vs never
#rm[3,1]<- 2
#rm[4,1]<- 3
#rm[5,1]<- 4

#lbl<-c("OC Former vs never","Current vs never","","","")

#ha<-phest(v2$coefficients,v2.cov,rm,lbl,print.all=T)

a$conf.int[,c(1,3,4)]

cbind(c(a$coef,confint(f1))


b<-cbind(f1$coef, confint(f1))

b<-cbind(round(exp(cbind(a$coef[,1],confint(f1))), digits=2), format.pval(a$coef[,4], digits=4, eps=0.001))


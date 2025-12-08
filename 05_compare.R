load("_sch.Rdata")

## ##add prior year means for comparisons below
## load(paste("_ela.Rdata",sep=''))
## df<-df[df$year==2024,]
## df<-df[df$grade>3,]
## df$school_original<-df$school_code
## df$school_code<-paste(df$school_original,df$grade,sep='__')
## L<-split(df,df$school_code)
## m<-t(sapply(L,function(x) c(mean(x$scale_score,na.rm=TRUE),mean(x$lag_scale_score,na.rm=TRUE))))
## mm<-data.frame(gid=rownames(m),ss=m[,1],lag=m[,2])
## z$gid<-paste(z$id,"__",z$grade,sep='')
## z<-merge(z,mm,all.x=TRUE)

###########################################
##EA results
ea<-read.csv("School level 2324 Growth.csv")
ea<-ea[ea$subgroup_code=="ALL",]
ea<-ea[ea$measure_name=="Academic Growth - English Language Arts",]
ea<-ea[,c("school_code","measure_rate","avg_ss_pre","avg_ss_post","denominator")]
tab<-table(ea[,1])
ea0<-ea[ea[,1] %in% names(tab)[tab==1],]
##
ea<-read.csv("School level 2324 Growth.csv")
ea<-ea[ea$subgroup_code=="ALL",]
ea<-ea[ea$measure_name=="Academic Growth - Math",]
ea<-ea[,c("school_code","measure_rate")]
tab<-table(ea[,1])
ea<-ea[ea[,1] %in% names(tab)[tab==1],]
##
names(ea0)[2]<-'ea.ela'
names(ea)[2]<-'ea.math'
ea<-merge(ea0,ea)

###########################################
##merging and baseline comparisons
z<-merge(sch,ea,by.x='id',by.y='school_code')
ran<-max(z$n_ela)-min(z$n_ela)
cex<-.2+(z$n_ela-min(z$n_ela))/ran
par(mfrow=c(1,3),mgp=c(2,1,0))
plot(z$n_ela,z$denominator,xlab='mine',ylab='ea',main='N')
plot(z$per_ela,z$ea.ela,pch=19,cex=cex,xlab='mine',ylab='ea',main='ela')
legend("topleft",bty='n',title=expression(rho),legend=round(cor(z$per_ela,z$ea.ela,use='p'),4))
plot(z$per_math,z$ea.math,pch=19,cex=cex,xlab='mine',ylab='ea',main='math')
legend("topleft",bty='n',title=expression(rho),legend=round(cor(z$per_math,z$ea.math,use='p'),4))

###########################################
###########################################
###########################################
###########################################
###########################################
##sleuthing below
###########################################
##sample sizes
##bunch of schools with very different sample sizes [but not the driver of difference]
plot(z$n,z$denominator); abline(0,1)
delta<-z$n-z$denominator

flag<-delta< -100
plot(z$per.ela,z$ea.ela,col=ifelse(flag,'red','black'),pch=19)

##what is going on here?
load("_ela.Rdata")
tmp<-df[df$school_code %in% as.numeric(z$id[flag]),]
unique(tmp$school_name) #almost all high schools
par(mfrow=c(1,2))
plot(z$mean.lag[flag],z$avg_ss_pre[flag]); abline(0,1)
plot(z$mean.scale[flag],z$avg_ss_post[flag]); abline(0,1)

load("_ela.Rdata")
i<-which(df$school_code=="101337")

###########################################
##same schools in english and math? yeah mostly
f1<-z$per.ela*100-z$ea.ela
f2<-z$per.math*100-z$ea.math
plot(f1,f2)

hist(f1)

###########################################
##comparison of pre/post tests: they look great!
par(mfrow=c(1,2))
plot(z$mean.scale,z$avg_ss_post); abline(0,1)
plot(z$mean.lag,z$avg_ss_pre); abline(0,1)


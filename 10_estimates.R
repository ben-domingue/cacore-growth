


################################################################
## School-grade growth estimates with related uncertainty information. 
load("_estimates.Rdata")
##
f<-function(x) {
    z<-lapply(x,function(x) x$coef)
    z<-do.call("rbind",z)
    z<-data.frame(z)
    names(z)<-paste("ela_",names(z),sep='')
    txt<-strsplit(rownames(z),"schoolid_",fixed=TRUE)
    txt<-sapply(txt,function(x) x[2])
    z$id<-sapply(strsplit(txt,"__",fixed=TRUE),function(x) x[1])
    z$grade<-sapply(strsplit(txt,"__",fixed=TRUE),function(x) x[2])
    z
}
L<-lapply(estimates,f)
names(L[[2]])<-gsub("ela_","math_",names(L[[2]]),fixed=TRUE)
x<-merge(L[[1]],L[[2]],all=TRUE)

write.csv(x,file="/tmp/est.csv",quote=FALSE,row.names=FALSE)

################################################################
## School growth estimates with related uncertainty information. 
load("_sch.Rdata")
write.csv(sch,file="/tmp/school.csv",quote=FALSE,row.names=FALSE)

################################################################
## Student residuals that were used in computation of (I). 
load("_estimates.Rdata")
f<-function(x) x$m3$m3resid[,c("id","m3resid")]
r1<-lapply(estimates[[1]],f)
r1<-data.frame(do.call("rbind",r1))
names(r1)[2]<-"_ela"
r2<-lapply(estimates[[2]],f)
r2<-data.frame(do.call("rbind",r2))
names(r2)[2]<-"_math"
r<-merge(r1,r2)
write.csv(r,file="/tmp/residual.csv",quote=FALSE,row.names=FALSE)

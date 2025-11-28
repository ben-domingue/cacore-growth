


################################################################
## School-grade growth estimates with related uncertainty information. 
load("_estimates.Rdata")
library(weights)
grest<-list() ##get growth estimates `grest`
for (i in 1:length(estimates)) {
    z1<-estimates[[i]]$co$coef
    z2<-estimates[[i]]$co.grade$coef
    grest[[names(estimates)[i] ]]<-list(co=z1,co.grade=z2)
}

L<-lapply(grest,function(x) x$co.grade)
for (i in 1:length(L)) L[[i]]$subject<-names(L)[i]
x<-data.frame(do.call("rbind",L))

nms<-names(x)
x$id<-rownames(x)
rownames(x)<-NULL
x<-x[,c("id",nms)]

z<-strsplit(x$id,"schoolid_",fixed=TRUE)
z<-sapply(z,function(x) x[2])
z<-strsplit(z,"__",fixed=TRUE)
id<-sapply(z,function(x) x[1])
gr<-sapply(z,function(x) x[2])
gr<-ifelse(is.na(gr),11,gr)
x$school<-id
x$grade<-gr

write.csv(x,file="/tmp/est.csv",quote=FALSE,row.names=FALSE)

################################################################
## School growth estimates with related uncertainty information. 
load("_estimates.Rdata")
library(weights)
grest<-list() ##get growth estimates `grest`
for (i in 1:length(estimates)) {
    z1<-estimates[[i]]$co$coef
    z2<-estimates[[i]]$co.grade$coef
    grest[[names(estimates)[i] ]]<-list(co=z1,co.grade=z2)
}
##aggregate to school-level
z1<-grest[[1]]$co.grade
names(z1)<-paste(names(z1),names(grest)[1],sep='')
z2<-grest[[2]]$co.grade
names(z2)<-paste(names(z2),names(grest)[2],sep='')
z<-merge(z1,z2,by=0,all=TRUE)
txt<-strsplit(z[,1],"_")
z$id<-sapply(txt,function(x) x[2])
L<-split(z,z$id)
f<-function(x) {
    m1<-Hmisc::wtd.mean(x$fe_ela,x$n_ela)
    m2<-Hmisc::wtd.mean(x$eb_ela,x$n_ela)
    m3<-Hmisc::wtd.mean(x$fe_math,x$n_math)
    m4<-Hmisc::wtd.mean(x$eb_math,x$n_math)
    se1<-sqrt(sum(x$eb.se_ela^2))
    se2<-sqrt(sum(x$eb.se_math^2))
    c(fe.ela=m1,eb.ela=m2,eb.ela.se=se1,fe.math=m3,eb.math=m4,eb.math.se=se2)
}
x<-t(sapply(L,f))
x<-data.frame(id=rownames(x),x)
write.csv(x,file="/tmp/school.csv",quote=FALSE,row.names=FALSE)

################################################################
## Student residuals that were used in computation of (I). 
load("_estimates.Rdata")
r1<-estimates[[1]]$co.grade$m3$m3resid
r2<-estimates[[2]]$co.grade$m3$m3resid
names(r1)[2]<-"_ela"
names(r2)[2]<-"_math"
r<-merge(r1,r2)
write.csv(r,file="/tmp/residual.csv",quote=FALSE,row.names=FALSE)

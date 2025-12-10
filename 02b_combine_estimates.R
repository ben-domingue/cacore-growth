   
bigfun<-function(est) {
    f<-function(xx) {
        ##get school part ready
        co<-xx$m3coef
        fe<-grep("^schoolid_",rownames(co))
        ids<-gsub("schoolid_","",rownames(co)[fe],fixed=TRUE)
        co<-data.frame(id=ids,fe=co[fe,1],se=co[fe,2])
        nms<-rownames(co)
        out<-list()
        df0<-xx$m3resid
        n<-table(df0$school_code)
        n<-data.frame(id=gsub("__11","",names(n),fixed=TRUE),n=as.numeric(n))
        co<-merge(co,n)
        co
    }
    L<-list()
    for (i in 1:length(est)) L[[i]]<-f(est[[i]]$m3)
    df<-data.frame(do.call("rbind",L))
    ## ##averaging
    txt<-strsplit(df$id,"__",fixed=TRUE)
    df$id<-sapply(txt,function(x) x[1])
    agg(df)
}

load("_estimates.Rdata")
sch<-lapply(estimates,bigfun)
for (i in 1:2) names(sch[[i]])[-1]<-paste(names(sch[[i]])[-1],names(sch)[i],sep='')
sch<-merge(sch[[1]],sch[[2]],by='id',all=TRUE)
save(sch,file="_sch.Rdata")

load("_estimates2024.Rdata")
sch<-lapply(estimates,bigfun)
for (i in 1:2) names(sch[[i]])[-1]<-paste(names(sch[[i]])[-1],names(sch)[i],sep='')
sch<-merge(sch[[1]],sch[[2]],by='id',all=TRUE)
save(sch,file="_sch2024.Rdata")



##school-level
load("_estimates.Rdata")
z<-estimates[[1]]
z<-lapply(z,function(x) x$coef)
z<-do.call("rbind",z)
y<-strsplit(rownames(z),"schoolid_",fixed=TRUE)
y<-sapply(y,function(x) x[2])
y2<-strsplit(y,"__",fixed=TRUE)
id<-sapply(y2,function(x) x[1])
gr<-sapply(y2,function(x) x[2])
df<-data.frame(id=id,gr=gr)
df$gr<-ifelse(is.na(df$gr),11,df$gr)
L<-split(df,df$id)
f<-function(x) paste(as.numeric(x$gr),collapse='.')
levs<-sapply(L,f)
tab<-table(levs)
z<-data.frame(as.numeric(tab),names(tab))
write.csv(z,'',quote=F,row.names=F)

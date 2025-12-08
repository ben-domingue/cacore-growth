
## bigfun<-function(est) {
##     f<-function(xx) {
##         ##get school part ready
##         co<-xx$m3coef
##         fe<-grep("^schoolid_",rownames(co))
##         ids<-gsub("schoolid_","",rownames(co)[fe],fixed=TRUE)
##         co<-data.frame(id=ids,fe=co[fe,1],se=co[fe,2])
##         nms<-rownames(co)
##         out<-list()
##         df0<-xx$m3resid
##         n<-table(df0$school_code)
##         n<-data.frame(id=names(n),n=as.numeric(n))
##         co<-merge(co,n)
##         ## get student part rady
##         names(df0)<-c("stud","id","resid")
##         xx<-df0[,c("id","resid")]
##         names(xx)<-c("id","res")
##         ##
##         xx<-merge(co,xx)
##         return(xx)
##     }
##     L<-list()
##     for (i in 1:length(est)) L[[i]]<-f(est[[i]]$m3)
##     df<-data.frame(do.call("rbind",L))
##     ##
##     eb<-function(x) {
##         aggregate(x$res,list(x$id),sd,na.rm=TRUE)->out
##         names(out)<-c("id","sd")
##         data.frame(table(x$id))->tab
##         names(tab)<-c("id","n")
##         merge(out,tab,by="id")->out
##         out$se<-out$sd^2/out$n
##         fe<-x[c("id","fe")]
##         fe<-fe[!duplicated(fe$id),]
##         merge(fe,out,by=1)->out
##         var(out$fe)-mean(out$se,na.rm=TRUE)->v2
##         out$shrink<-v2/(v2+out$se)
##         out$fe*out$shrink->out$eb
##         out
##     }
##     txt<-strsplit(df$id,"__")
##     df$id<-sapply(txt,function(x) x[1])
##     co<-eb(df)
##     f<-function(xx) {
##         ##get school part ready
##         co<-xx$m3coef
##         fe<-grep("^schoolid_",rownames(co))
##         ids<-gsub("schoolid_","",rownames(co)[fe],fixed=TRUE)
##         co<-data.frame(id=ids,fe=co[fe,1],se=co[fe,2])
##         nms<-rownames(co)
##         out<-list()
##         df0<-xx$m3resid
##         n<-table(df0$school_code)
##         n<-data.frame(id=names(n),n=as.numeric(n))
##         co<-merge(co,n)
##         co
##     }
##     L<-list()
##     for (i in 1:length(est)) L[[i]]<-f(est[[i]]$m3)
##     df<-data.frame(do.call("rbind",L))
##     ## ##averaging
##     txt<-strsplit(df$id,"__",fixed=TRUE)
##     df$id<-sapply(txt,function(x) x[1])
##     L<-split(df,df$id)
##     f<-function(x) {
##         #m<-Hmisc::wtd.mean(x$fe,x$n,na.rm=TRUE)
##         m<-mean(x$fe,na.rm=TRUE)
##         s<-sqrt(sum(x$se^2))
##         data.frame(id=unique(x$id),fe=m,se=s,n=sum(x$n))
##     }
##     L<-lapply(L,f)
##     co<-data.frame(do.call("rbind",L))
##     ##shrinking
##     return(co)
## }

## load("_estimates2024.Rdata")
## sch<-lapply(estimates,bigfun)
## for (i in 1:2) names(sch[[i]])[-1]<-paste(names(sch[[i]])[-1],names(sch)[i],sep='')
## sch<-merge(sch[[1]],sch[[2]],by='id',all=TRUE)
## save(sch,file="_sch.Rdata")





    
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
        n<-data.frame(id=names(n),n=as.numeric(n))
        co<-merge(co,n)
        co
    }
    L<-list()
    for (i in 1:length(est)) L[[i]]<-f(est[[i]]$m3)
    df<-data.frame(do.call("rbind",L))
    ## ##averaging
    txt<-strsplit(df$id,"__",fixed=TRUE)
    df$id<-sapply(txt,function(x) x[1])
    L<-split(df,df$id)
    f<-function(x) {
        ##m<-Hmisc::wtd.mean(x$fe,x$n,na.rm=TRUE)
        m<-mean(x$fe,na.rm=TRUE)
        s<-mean(x$se)##sqrt(sum(x$se^2))
        data.frame(id=unique(x$id),fe=m,se=s,n=sum(x$n))
    }
    L<-lapply(L,f)
    co<-data.frame(do.call("rbind",L))
    ##shrinking
    shrink.univariate<-function(co) {
        library(Hmisc)
        var.alpha<-wtd.var(co$fe,co$n)
        mean.se<-wtd.mean(co$se^2,co$n)
        omega<-var.alpha-mean.se
        print(omega)
        co$eb<-co$fe*(omega/(omega+co$se^2))
        co$eb.se<-co$se*(omega/(omega+co$se^2))
        return(co)
    }
    co<-shrink.univariate(co)
########################################
    ##conversion to percentiles
    m<-mean(co$eb,na.rm=TRUE)
    s<-sd(co$eb,na.rm=TRUE)
    co$per<-pnorm(co$eb,m,s)
    return(co)
}

load("_estimates2024.Rdata")
sch<-lapply(estimates,bigfun)
for (i in 1:2) names(sch[[i]])[-1]<-paste(names(sch[[i]])[-1],names(sch)[i],sep='')
sch<-merge(sch[[1]],sch[[2]],by='id',all=TRUE)
save(sch,file="_sch.Rdata")

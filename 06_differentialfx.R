## Growth measures for student subgroups within schools are produced in addition to the overall
## growth measures described above. Subgroup measures are produced by
## The analysis that produces subgroup growth measures is conducted separately
## for each class of subgroups. In other words, the subgroup analysis conducted for the set of
## race subgroups is separate from the analysis conducted for the set of disability subgroups.

source("/home/bdomingu/Dropbox/projects/ca_growth/src/0_aggregate.R")
##
load("_estimates.Rdata")
load("_boot2.Rdata")
##
load("_g11.Rdata")
getwt<-function(g11.tmp) {
    ids<-c(g11.tmp[,1],g11.tmp[,2],g11.tmp[,3])
    ids<-unique(ids)
    ids<-ids[!is.na(ids)]
    ss<-matrix(0,nrow=nrow(g11.tmp),ncol=length(ids))
    colnames(ss)<-paste("schoolid_",ids,sep='')
    for (i in 1:nrow(g11.tmp)) {
        index<-match(paste("schoolid_",g11.tmp[i,1:3],sep=""),colnames(ss))
        index<-index[!is.na(index)]
        for (j in index) ss[i,j]<-ss[i,j]+1
    }
    for (i in 1:ncol(ss)) ss[,i]<-ss[,i]/3
    rownames(ss)<-g11.tmp$id
    ss
}
wtmat<-getwt(g11)

getavg<-function(x.all,wtmat,ss=FALSE) {
    ##grade<11
    xx<-x.all[x.all$grade<11,]
    mm<-by(xx$errors2,xx$school_code,mean,na.rm=TRUE)
    nn<-by(xx$errors2,xx$school_code,length)
    z<-data.frame(school_code=names(mm),fe=as.numeric(mm))
    tmp<-data.frame(school_code=names(nn),n=as.numeric(nn))
    z<-merge(z,tmp)
    ##need to do something different with g11 here
    xx<-x.all[x.all$grade==11,]
    zz<-merge(xx,wtmat,by.x='id',by.y=0)
    sch<-as.matrix(zz[,grep("^schoolid_",names(zz))])
    e2<-as.matrix(zz$errors2,ncol=1)
    num<-t(sch) %*% e2
    n<-rowSums(t(sch))
    fe<-as.numeric(num/n)
    fe<-ifelse(is.nan(fe),NA,fe)
    z11<-data.frame(school_code=colnames(sch),fe=fe,n=n)
    z11$school_code<-gsub("schoolid_","",z11$school_code)
    z11<-z11[!is.na(z11$fe),]
    ##
    z<-data.frame(rbind(z,z11))
    if (ss) {
        mm<-by(x.all$scale_score,x.all$school_code,mean,na.rm=TRUE)
        pp<-by(x.all$lag_scale_score,x.all$school_code,mean,na.rm=TRUE)
        tmp<-data.frame(school_code=names(mm),scale_mean=as.numeric(mm))
        tmp$school_code<-gsub("__11","",tmp$school_code)
        z<-merge(z,tmp)
        tmp<-data.frame(school_code=names(pp),lag_mean=as.numeric(pp))
        tmp$school_code<-gsub("__11","",tmp$school_code)
        z<-merge(z,tmp)
    }
    z
}

    
agg.local<-function(x,res,shoe) {
    x$school<-x$school_code
    x$school_code<-paste(x$school_code,x$grade,sep='__')
    ##get average fe
    sch<-x[,c("id","school_code","grade","scale_score","lag_scale_score")]
    xx<-merge(sch,res)
    z<-getavg(xx,wtmat,ss=TRUE)
    ##get variability in estimate via boot (see 06b_)
    estL<-list()
    for (i in 1:length(shoe)) {
        sh<-shoe[[i]]
        tmp<-merge(sch,sh) ##need to merge carefully
        tmp<-getavg(tmp,wtmat)
        tmp<-tmp[,c("school_code","fe")]
        names(tmp)[2]<-paste("est",i,sep='')
        estL[[i]]<-tmp
    }
    tmp<-estL[[1]]
    for (i in 2:length(estL)) tmp<-merge(tmp,estL[[i]],all=TRUE)
    id<-tmp$school_code
    tmp$school_code<-NULL
    om<-data.frame(school_code=id,
                   se=apply(tmp,1,sd,na.rm=TRUE),
                   boot.m=apply(tmp,1,mean,na.rm=TRUE))
    z<-merge(z,om)
    z<-z[z$n>1,]
    ##
    z$id<-z$school_code
    co<-agg(z)
    merge(co,z[,c("id","scale_mean","lag_mean","boot.m")])
}

parfun<-function(x,res,shoe) agg.local(x,res=res,shoe=shoe)

out<-list()
for (nm in c("_ela","_math")) {
    ##
    est<-estimates[[nm]]
    res<-list()
    for (i in 1:length(est)) {
        tmp<-est[[i]]$m2$m2q
        tmp$grade<-as.numeric(names(est)[i])
        res[[i]]<-tmp
    }
    res<-data.frame(do.call("rbind",res))
    ##
    shoe<-boot[[nm]]
    ##
    load(paste(nm,".Rdata",sep=''))
    df<-df[df$year==2025,]
    df<-df[df$grade>3,]
    ##analysis by subgroup
    L<-list()
    L$all<-list(all=df)
    ## race (Asian, African American, Filipino, Hispanic/Latinx, Pacific Islander, Multiracial, Native American, White, and race missing);
    df$race_eth_rollup<-ifelse(df$race_eth_rollup=="",NA,df$race_eth_rollup)
    L$race<-split(df,df$race_eth_rollup)
    ## English language learner (ELL, not ELL);
    df$ell<-ifelse(is.na(df$ell_level),0,1)
    L$ell<-split(df,df$ell)
    ## disability (with disability, without disability);
    df$disability<-ifelse(df$disability_type=="",0,1)
    L$disability<-split(df,df$disability)
    ## economic disadvantage (economically disadvantaged, not economically disadvantaged);
    L$disadvantage<-split(df,df$socioecon_disadvantaged)
    ## and pretest performance (low pretest performance, high pretest performance).
    tmp<-df[!is.na(df$lag_achievement_level),]
    tmp$lowpretest<-ifelse(tmp$lag_achievement_level %in% 1:2,1,0)
    L$pretest<-split(tmp,tmp$lowpretest)
    ##
    L<-do.call("c",L)
    out.inner<-parallel::mclapply(L,parfun,mc.cores=6,
                                  res=res,shoe=shoe)
    for (i in 1:length(out.inner)) { ##just adding some final columns
        est<-out.inner[[i]]
        est$group<-names(L)[i]
        est$subject<-nm
        out.inner[[i]]<-est
    }
    out[[nm]]<-out.inner
}
hold<-out


for (i in 1:length(out)) out[[i]]<-data.frame(do.call("rbind",out[[i]]))
dfx<-data.frame(do.call("rbind",out))
save(dfx,file="_difffx.Rdata")

write.csv(dfx,'/tmp/diff.csv',quote=FALSE,row.names=FALSE)

##########################################################
##compare to "direct" school effects (that come from 02b_)
load("_difffx.Rdata")
z<-dfx[dfx$subject=="_ela" & dfx$group=="all.all",]
txt<-strsplit(z$id,"__")
z$id<-sapply(txt,function(x) x[1])
co<-agg(z)
z<-co[,c("id","fe","eb","se","n")]
##
load("_sch.Rdata")
x<-sch[,c("id","fe_ela","se_ela","eb_ela","n_ela")]
##merge
y<-merge(z,x)

par(mfrow=c(2,2))
y<-y[order(y$n),]
cex<-.3+2*(y$n)/1200
plot(y$n,y$n_ela)
plot(y$fe,y$fe_ela,pch=19,cex=cex); abline(0,1)
plot(y$eb,y$eb_ela,pch=19,cex=cex); abline(0,1)
m<-loess(se~n,y)
plot(y$n,fitted(m),type='l')
m<-loess(se_ela~n,y)
lines(y$n,fitted(m),col='red')


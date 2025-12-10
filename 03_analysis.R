load("_estimates.Rdata")
library(weights)

################################################################
##coefficients
f<-function(estimates) {
    tab<-list()
    for (i in 1:length(estimates)) {
        m1<-estimates[[i]]$m1
        co<-m1$m1coef[!grepl("^schoolid_",names(m1$m1coef))]
        m1<-c(co,c(N=m1$m1N,m1$m1coef.noisy[grep("^lag_scale_score",names(m1$m1coef.noisy))]))
        m2<-estimates[[i]]$m2$m2coef
        m3<-estimates[[i]]$m3
        #co<-m3$m3coef[!grepl("^schoolid_",rownames(m3$m3coef))]
        m3<-c(N=m3$m3N)
        tab[[i]]<-c(m1,m2,m3)
    }
    tab<-do.call("cbind",tab)
    colnames(tab)<-names(estimates)
    tab
}
tabL<-lapply(estimates,f)

write.csv(rbind(tabL[["_ela"]],tabL[["_math"]]),'')

################################################################
##Distribution
load("_sch.Rdata")

par(mfcol=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
z<-sch
##
plot(density(z$eb_ela),col='blue',lwd=2,main='',xlim=c(-.3,.3),xlab="EB estimates")
mtext(side=3,line=0,"ELA")
hist(z$per_ela,col='blue',main='',xlab='Percentiles')
##
plot(density(z$eb_math),col='blue',lwd=2,main='',xlim=c(-.3,.3),xlab="EB estimates")
mtext(side=3,line=0,"MATH")
hist(z$per_math,col='blue',main='',xlab='Percentiles')


################################################################
##Grade-level correlations within school
library(lme4)
load("_estimates.Rdata")

##
f<-function(x) {
    x<-x[-length(x)] #no grade 11
    z<-lapply(x,function(x) x$coef)
    z<-do.call("rbind",z)
    txt<-strsplit(rownames(z),"schoolid_",fixed=TRUE)
    txt<-sapply(txt,function(x) x[2])
    z$id<-sapply(strsplit(txt,"__",fixed=TRUE),function(x) x[1])
    lme4::lmer(eb~(1|id),z)
}
lapply(estimates,f)

.0292^2/(.0292^2+.1167^2)
.0209^2/(.0209^2+.1348^2)

## ####adding delta
load("_ela.Rdata")
df<-df[df$year==2025,]
df<-df[df$grade>3,]
df$school_code<-paste(df$school_code,df$grade,sep='__')
df$delta<-df$scale_score-df$lag_scale_score
xx<-df[,"delta"]
xx<-split(xx,df$school_code)
xx<-lapply(xx,mean,na.rm=TRUE)
x<-data.frame(do.call("rbind",xx))
names(x)[1]<-"delta.ela"
x$id<-names(xx)
del<-x
x<-estimates[[1]]
x<-x[-length(x)] #no grade 11
z<-lapply(x,function(x) x$coef)
z<-do.call("rbind",z)
z$id<-rownames(z)
txt<-strsplit(z$id,".",fixed=TRUE)
z$id<-sapply(txt,function(x) x[2])
txt<-strsplit(rownames(z),"schoolid_",fixed=TRUE)
z$id<-sapply(txt,function(x) x[2])
z<-merge(z,del)
z$id<-sapply(strsplit(z$id,"__",fixed=TRUE),function(x) x[1])
lme4::lmer(delta.ela~(1|id),z)
##9.9^2/(9.9^2+21.7^2)
lme4::lmer(eb~(1|id),z)


################################################################
##Correlation across subjects
library(weights)
load("_sch.Rdata")
z<-sch
coors<-list()
coors$fe<-cor(z$fe_ela,z$fe_math)
coors$wtfe<-wtd.cor(z$fe_ela,z$fe_math,z$n_ela)[1]
cor(z$n_ela,z$n_math)
coors$eb<-cor(z$eb_ela,z$eb_math)
coors$wteb<-wtd.cor(z$eb_ela,z$eb_math,z$n_ela)[1]
coors$per<-cor(z$per_ela,z$per_math)
coors$wtper<-wtd.cor(z$per_ela,z$per_math,z$n_ela)[1]
write.csv(unlist(coors))

################################################################
##Correlation with key categories
load("_math.Rdata")
df<-df[df$year==2025,]
df<-df[df$grade>3,]
df$delta.math<-df$scale_score-df$lag_scale_score
del<-df[,c("id","delta.math")]
del$scale_score_math<-df$scale_score
del$lag_scale_score_math<-df$lag_scale_score
##
load("_ela.Rdata")
df<-df[df$year==2025,]
df<-df[df$grade>3,]
##df$school_code<-paste(df$school_code,df$grade,sep='__')
df$delta.ela<-df$scale_score-df$lag_scale_score
df$scale_score_ela<-df$scale_score
df$lag_scale_score_ela<-df$lag_scale_score
df<-merge(df,del,all.x=TRUE)
vars<-c( #"free_or_reduced", 
    "socioecon_disadvantaged", 
    "race_ai", "race_as", "race_aa", "race_fi", "race_hi", "race_mr", 
    "race_pi", "race_na", "race_wh", #"disability_type", 
    "scale_score_ela", "scale_score_math",
    "lag_scale_score_ela","lag_scale_score_math",
"delta.math","delta.ela")
xx<-df[,vars]
xx<-split(xx,df$school_code)
xx<-lapply(xx,colMeans,na.rm=TRUE)
x<-data.frame(do.call("rbind",xx))
x$id<-names(xx)
load("_sch.Rdata")
x<-merge(sch,x,by='id')

coors<-list()
for (v in vars) {
    c1<-weights::wtd.cor(x[,v],x$fe_ela,x$n_ela)[1]
    c2<-weights::wtd.cor(x[,v],x$fe_math,x$n_math)[1]
    coors[[v]]<-c(c1,c2)
}
coors<-do.call("rbind",coors)
write.csv(coors,'')




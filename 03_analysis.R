load("_estimates.Rdata")
library(weights)

################################################################
##coefficients
tab<-list()
for (i in 1:length(estimates)) {
    z1<-estimates[[i]]$co.grade$m1
    co<-z1$m1coef
    co<-co[!grepl("^schoolid_",names(co))]
    m1<-c(co,c(z1$m1N,z1$m1coef.noisy[grep("^lag_scale_score",names(z1$m1coef.noisy))]))
    m2<-estimates[[i]]$co.grade$m2
    co<-estimates[[i]]$co.grade$m3$m3coef
    co<-co[!grepl("^schoolid_",names(co))]
    m3<-c(co,estimates[[i]]$co.grade$m3$m3N)
    tab[[i]]<-c(m1,m2,m3)
}
tab<-do.call("cbind",tab)
write.csv(tab,'')

################################################################
grest<-list() ##get growth estimates `grest`
for (i in 1:length(estimates)) {
    z1<-estimates[[i]]$co$coef
    z2<-estimates[[i]]$co.grade$coef
    grest[[names(estimates)[i] ]]<-list(co=z1,co.grade=z2)
}

################################################################
##shrinkage analysis
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
for (i in 1:2) {
    x<-grest[[1]][[2]]
    s<-x$eb/x$fe
    plot(log10(x$n),s,xlab="N",ylab="Shrinkage factor",xaxt='n',pch=19,col='blue',ylim=c(0,1))
    vals<-c(1,10,50,100,250)
    axis(side=1,at=log10(vals),vals)
    mtext(side=3,line=0,names(grest)[i])
}

x<-grest[[1]][[2]]
i<-grepl("__",rownames(x))
s<-x$eb/x$fe
by(s,i,summary)
by(x$n,i,summary)
by(x$se,i,summary)

################################################################
##Distribution
par(mfcol=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
for (i in 1:length(grest)) {
    z<-grest[[i]]
    plot(density(z[[2]]$eb),col='blue',lwd=2,main='',xlim=c(-.7,.7),xlab="EB estimates")
    mtext(side=3,line=0,names(grest)[i])
    hist(z[[2]]$per,col='blue',main='',xlab='Percentiles')
    print(summary(z[[2]]$per))
}

################################################################
##Correlation across subjects
z1<-grest[[1]]$co.grade
names(z1)<-paste(names(z1),names(grest)[1],sep='')
z2<-grest[[2]]$co.grade
names(z2)<-paste(names(z2),names(grest)[2],sep='')
z<-merge(z1,z2,by=0,all=TRUE)
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
##Grade-level correlations within school
txt<-strsplit(z[,1],"_")
z$id<-sapply(txt,function(x) x[2])
library(lme4)
##
lmer(eb_ela~(1|id),z)
 ## Groups   Name        Std.Dev.
 ## id       (Intercept) 0.0448  
 ## Residual             0.1065  
.0448^2/(.1065^2+.0448^2)
##
lmer(eb_math~(1|id),z)
 ## Groups   Name        Std.Dev.
 ## id       (Intercept) 0.0382  
 ## Residual             0.1175  
.0382^2/(.0382^2+.1175^2)

##aggregate to school-level
L<-split(z,z$id)
f<-function(x) {
    m1<-Hmisc::wtd.mean(x$fe_ela,x$n_ela)
    m2<-Hmisc::wtd.mean(x$eb_ela,x$n_ela)
    m3<-Hmisc::wtd.mean(x$fe_math,x$n_math)
    m4<-Hmisc::wtd.mean(x$eb_math,x$n_math)
    c(fe.ela=m1,eb.ela=m2,fe.math=m3,eb.math=m4)
}
x<-t(sapply(L,f))
x<-data.frame(id=paste("schoolid_",rownames(x),sep=''),x)

z1<-estimates[[1]]$co$coef
tmp<-merge(z1,x,by.x=0,by.y='id')
cor(tmp$fe,tmp$fe.ela)
cor(tmp$eb,tmp$eb.ela)
z1<-estimates[[2]]$co$coef
tmp<-merge(z1,x,by.x=0,by.y='id')
cor(tmp$fe,tmp$fe.math)
cor(tmp$eb,tmp$eb.math)

################################################################
##Correlation with key categories
load("_ela.Rdata")
df<-df[df$year==2025,]
df<-df[df$grade>3,]
df$school_code<-paste(df$school_code,df$grade,sep='__')
df$delta<-df$scale_score-df$lag_scale_score
vars<-c( #"free_or_reduced", 
                                        #"homeless", "foster",
                                        #"sped_flag", "elpac_level",
    "socioecon_disadvantaged", 
    "race_ai", "race_as", "race_aa", "race_fi", "race_hi", "race_mr", 
    "race_pi", "race_na", "race_wh", #"disability_type", 
                                        #"ell_level",
    "scale_score", 
    "lag_scale_score","delta")
xx<-df[,vars]
xx<-split(xx,df$school_code)
xx<-lapply(xx,colMeans,na.rm=TRUE)
x<-data.frame(do.call("rbind",xx))
x$id<-paste("schoolid_",names(xx),sep='')
x<-merge(z,x,by.x=1,by.y='id')

coors<-list()
for (v in vars) {
    c1<-weights::wtd.cor(x[,v],x$fe_ela,x$n_ela)[1]
    c2<-weights::wtd.cor(x[,v],x$fe_math,x$n_math)[1]
    coors[[v]]<-c(c1,c2)
}
coors<-do.call("rbind",coors)
write.csv(coors,'')




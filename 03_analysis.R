load("_estimates.Rdata")

##Distribution
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
for (i in 1:length(estimates)) {
    z<-estimates[[i]]
    plot(density(z[[2]]$eb),col='blue',lwd=2,main='',xlim=c(-.7,.7))
    mtext(side=3,line=0,names(estimates)[i])
}

##Correlation across subjects
z1<-estimates[[1]]$co.grade
names(z1)<-paste(names(z1),names(estimates)[1],sep='')
z2<-estimates[[2]]$co.grade
names(z2)<-paste(names(z2),names(estimates)[2],sep='')
z<-merge(z1,z2,by=0,all=TRUE)
cor(z$fe_ela,z$fe_math)
cor(z$n_ela,z$n_math)
cor(z$eb_ela,z$eb_math)
cor(z$per_ela,z$per_math)

##Grade-level correlations within school
txt<-strsplit(z[,1],"_")
z$id<-sapply(txt,function(x) x[2])
library(lme4)
lmer(eb_ela~(1|id),z)
lmer(eb_math~(1|id),z)

##aggregate to school-level

##Correlation with key categories
load("_ela.Rdata")
df<-df[df$year==2025,]
df<-df[df$grade>3,]
df$school_code<-paste(df$school_code,df$grade,sep='__')
vars<-c( #"free_or_reduced", 
    #"homeless", "foster",
                                        #"sped_flag", "elpac_level",
    "socioecon_disadvantaged", 
  "race_ai", "race_as", "race_aa", "race_fi", "race_hi", "race_mr", 
  "race_pi", "race_na", "race_wh", #"disability_type", 
                                        #"ell_level",
  "scale_score", 
  "lag_scale_score")
xx<-df[,vars]
xx<-split(xx,df$school_code)
xx<-lapply(xx,colMeans,na.rm=TRUE)
x<-data.frame(do.call("rbind",xx))
x$id<-paste("schoolid_",names(xx),sep='')
x<-merge(z,x,by.x=1,by.y='id')

coors<-list()
for (v in vars) {
    c1<-cor(x[,v],x$fe_ela,use='p')
    c2<-cor(x[,v],x$fe_math,use='p')
    coors[[v]]<-c(c1,c2)
}

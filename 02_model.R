##need to get reliabilities

load("_ela.Rdata")

vx<-var(df$scale_score,na.rm=TRUE)
##reliability<-vx/(vx+var(df$standard_error_measurement,na.rm=TRUE))
reliability<-0.9
library(mecor)

##sampling
ids<-sample(unique(df$school_code),200)
df<-df[df$school_code %in% ids,]

z<-df[,c("school_code","scale_score","lag_scale_score","alt_lag_scale_score")]
df0<-df[rowSums(is.na(z))==0,] #just for analysis
rm("df") ##just to ensure i don't use it
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df0$scale_score<-std(df0$scale_score)

##school matrix
ids<-unique(df0$school_code)
for (id in ids) df0[[paste("schoolid_",id,sep='')]]<-as.numeric(df0$school_code==id)
    
##Additionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 
fm.base<-c("disability_type",
           "ell_level",
           "socioecon_disadvantaged",
           "foster",
           "homeless"
)
   
fm.list<-character()
for (nm in fm.base) {
    xx<-as.character(df0[[nm]])
    xx<-ifelse(is.na(xx),'missing',xx)
    xx<-factor(xx,exclude=NULL)
    levs<-levels(xx)
    zz<-list()
    for (lev in levs[-1]) {
        test<- xx==lev
        new.nm<-paste(nm,lev,sep="__")
        df0[[new.nm]]<-as.numeric(test)
        fm.list[new.nm]<-new.nm
    }
}
##special check
if ("homeless__missing" %in% names(df0) & all(df0$homeless__missing==df0$foster__missing))
    fm.list<-fm.list[-grep("core_fst__missing",fm.list)]
    
        
ii<-grep("^schoolid",names(df0))
ii<-ii[-1] #to ensure no collinearity problem with schools
fm<-paste(
    "scale_score~MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))+alt_lag_scale_score+lag_mean+",
    paste(fm.list,collapse="+"),
    "+",
    paste(names(df0)[ii],collapse='+'),sep=''
)

m1<-mecor(as.formula(fm),df0)
#coef(m)[2]
#m1$corfit$coef[2]

##fm0<-gsub("MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))","lag_scale_score",fm,fixed=TRUE)
##lm(as.formula(fm0),df0)


##standardize//shrink [see page 10 of core report re shrinkage]
co<-m1$uncorfit$coefficients
fe<-co[grep("^schoolid_",names(co))]
fe<-fe-mean(fe,na.rm=TRUE) #mean-center
out<-list()
nms<-names(fe)
for (nm in nms) out[[nm]]<-c(fe[nm],sum(df0[[nm]]))
fe<-do.call("rbind",out)
fe<-data.frame(fe)
names(fe)<-c("fe","n")
fe$school<-nms
d<-diag(m1$corfit$zerovar_vcov)
fe<-merge(fe,data.frame(school=names(d),se=d))
## To implement this shrinkage, we estimate the sampling-error-adjusted variance of the
## unshrunk school growth measures 𝛼̂ 𝑗 . This is equal to the variance across schools of the
## estimates 𝛼̂ 𝑗 , minus the mean across schools of the squared standard errors 𝜎̂ 𝑗2 of those
## estimates. Both the mean and variance are estimated using the number of students associated with the school, 𝑛 𝑗 , as a weight. The resulting variance estimate, 𝜔
## the schools' "true" effects 𝛼 𝑗 . It also estimates what the variance across schools of the
## unshrunk school growth estimates 𝛼̂ 𝑗 would be if each school had an extremely large number
## of students to the point of sampling error being ignorable.
library(Hmisc)
var.alpha<-wtd.var(fe$fe,fe$n)
mean.se<-wtd.mean(fe$se^2,fe$n)
omega<-var.alpha-mean.se
fe$eb<-fe$fe*(omega/(omega+fe$se^2))

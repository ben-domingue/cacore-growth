##idea was to compare the mecor/lm() based approach with one that used feols and didn't have school effects inthe first model. but this was going to be messy in part due to the high school weighting

growth<-function(df,reliability=0.9,sampling=NULL) {
    library(mecor)
    ##sampling
    if (!is.null(sampling)) {
        ids<-sample(unique(df$school_code),sampling)
        df<-df[df$school_code %in% ids,]
    }
    z<-df[,c("school_code","scale_score","lag_scale_score","alt_lag_scale_score")]
    df0<-df[rowSums(is.na(z))==0,] #just for analysis
    rm("df") ##just to ensure i don't use it
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    df0$scale_score<-std(df0$scale_score)
    ##school matrix
    ids<-unique(df0$school_code)
    for (id in ids) df0[[paste("schoolid_",id,sep='')]]<-as.numeric(df0$school_code==id)
    ##Aditionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 
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
        fm.list<-fm.list[-grep("foster__missing",fm.list)]
    ##build formula
    ii<-grep("^schoolid",names(df0))
    ii<-ii[-1] #to ensure no collinearity problem with schools
    fm<-paste(
        "scale_score~MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))+alt_lag_scale_score+",
        paste(fm.list,collapse="+"),
        "+",
        paste(names(df0)[ii],collapse='+'),sep=''
    )
    ##step 1
    m1<-mecor(as.formula(fm),df0)
    ##fm0<-gsub("MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))","lag_scale_score",fm,fixed=TRUE)
    ##lm(as.formula(fm0),df0)
    ##step 1.5: residualize
    co<-m1$corfit$coef
    fe<-grep("^schoolid_",names(co))
    co0<-co[-fe]
    cols<-sub("cor_lag","lag",names(co0),fixed=TRUE)  ##sub is to get rid of `cor_` put in by mecor
    mm<-df0[,cols[-1]] #no intercept
    mm<-cbind(1,mm)
    co0m<-matrix(co0,ncol=1)
    fit<-as.matrix(mm) %*% co0m
    df0$errors<-df0$scale_score-fit
    ##step 2: regress pretest mean
    m2<-lm(errors~lag_mean,df0)
    df0$errors2<-m2$resid
    ##step3: almost there!
    ii<-grep("^schoolid",names(df0))
    ii<-ii[-1] #to ensure no collinearity problem with schools
    fm<-paste(
        "errors2~",
        paste(names(df0)[ii],collapse='+'),sep=''
    )
    m3<-lm(as.formula(fm),df0)
    ##standardize//shrink [see page 10 of core report re shrinkage]
    S<-summary(m3)$coef
    fe<-grep("^schoolid_",rownames(S))
    co<-S[fe,1:2]
    co<-data.frame(co)
    names(co)<-c("fe","se")
    co$fe<-co$fe-mean(co$fe,na.rm=TRUE) #mean-center
    nms<-rownames(co)
    out<-list()
    for (nm in nms) out[[nm]]<-sum(df0[[nm]])
    co$n<-unlist(out)
    ## To implement this shrinkage, we estimate the sampling-error-adjusted variance of the
    ## unshrunk school growth measures 𝛼̂ 𝑗 . This is equal to the variance across schools of the
    ## estimates 𝛼̂ 𝑗 , minus the mean across schools of the squared standard errors 𝜎̂ 𝑗2 of those
    ## estimates. Both the mean and variance are estimated using the number of students associated with the school, 𝑛 𝑗 , as a weight. The resulting variance estimate, 𝜔
    ## the schools' "true" effects 𝛼 𝑗 . It also estimates what the variance across schools of the
    ## unshrunk school growth estimates 𝛼̂ 𝑗 would be if each school had an extremely large number
    ## of students to the point of sampling error being ignorable.
    library(Hmisc)
    var.alpha<-wtd.var(co$fe,co$n)
    mean.se<-wtd.mean(co$se^2,co$n)
    omega<-var.alpha-mean.se
    co$eb<-co$fe*(omega/(omega+co$se^2))
    ##conversion to percentiles
    m<-mean(co$eb)
    s<-sd(co$eb)
    co$per<-pnorm(co$eb,m,s)
    return(co)
}

##reliabilities, see table 8.3. https://www.cde.ca.gov/ta/tg/ca/documents/sbcaaspptechrpt24.docx
reliabilities<-c(`_ela`=0.88,`_math`=0.9)
estimates<-list()
nm<-"_ela"
load(paste(nm,".Rdata",sep=''))
##
df<-df[df$year==2025,]
df<-df[df$grade>3,]
##analysis by school
co.v1<-growth(df,reliability=reliabilities[nm],sampling=100)

##new approach
nm<-"_ela"
load(paste(nm,".Rdata",sep=''))
library(mecor)
reliability<-0.88
##sampling
ids<-gsub("schoolid_","",rownames(co.v1),fixed=TRUE)

df<-df[df$school_code %in% ids,]
z<-df[,c("school_code","scale_score","lag_scale_score","alt_lag_scale_score")]
df0<-df[rowSums(is.na(z))==0,] #just for analysis
rm("df") ##just to ensure i don't use it
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
df0$scale_score<-std(df0$scale_score)

##school matrix
ids<-unique(df0$school_code)
for (id in ids) df0[[paste("schoolid_",id,sep='')]]<-as.numeric(df0$school_code==id)

##Aditionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 
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
    fm.list<-fm.list[-grep("foster__missing",fm.list)]

##build formula
fm<-paste(
    "scale_score~MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))+alt_lag_scale_score+",
    paste(fm.list,collapse="+")
    )
##step 1
m1<-mecor(as.formula(fm),df0)

##step 1.5: residualize
co0<-m1$corfit$coef
cols<-sub("cor_lag","lag",names(co0),fixed=TRUE)  ##sub is to get rid of `cor_` put in by mecor
mm<-df0[,cols[-1]] #no intercept
mm<-cbind(1,mm)
co0m<-matrix(co0,ncol=1)
fit<-as.matrix(mm) %*% co0m
df0$errors<-df0$scale_score-fit
##step 2: regress pretest mean
m2<-lm(errors~lag_mean,df0)
df0$errors2<-m2$resid

##step3: almost there!
ii<-grep("^schoolid",names(df0))
ii<-ii[-1] #to ensure no collinearity problem with schools
fm<-paste(
    "errors2~",
    paste(names(df0)[ii],collapse='+'),sep=''
)
m3<-lm(formula(fm),df0)

##standardize//shrink [see page 10 of core report re shrinkage]
S<-summary(m3)$coef
    fe<-grep("^schoolid_",rownames(S))
    co<-S[fe,1:2]
    co<-data.frame(co)
    names(co)<-c("fe","se")
    co$fe<-co$fe-mean(co$fe,na.rm=TRUE) #mean-center
    nms<-rownames(co)
    out<-list()
    for (nm in nms) out[[nm]]<-sum(df0[[nm]])
    co$n<-unlist(out)
    ## To implement this shrinkage, we estimate the sampling-error-adjusted variance of the
    ## unshrunk school growth measures 𝛼̂ 𝑗 . This is equal to the variance across schools of the
    ## estimates 𝛼̂ 𝑗 , minus the mean across schools of the squared standard errors 𝜎̂ 𝑗2 of those
    ## estimates. Both the mean and variance are estimated using the number of students associated with the school, 𝑛 𝑗 , as a weight. The resulting variance estimate, 𝜔
    ## the schools' "true" effects 𝛼 𝑗 . It also estimates what the variance across schools of the
    ## unshrunk school growth estimates 𝛼̂ 𝑗 would be if each school had an extremely large number
    ## of students to the point of sampling error being ignorable.
    library(Hmisc)
    var.alpha<-wtd.var(co$fe,co$n)
    mean.se<-wtd.mean(co$se^2,co$n)
    omega<-var.alpha-mean.se
    co$eb<-co$fe*(omega/(omega+co$se^2))
    ##conversion to percentiles
    m<-mean(co$eb)
    s<-sd(co$eb)
    co$per<-pnorm(co$eb,m,s)

dim(co)
dim(co.v1)
names(co)<-paste(names(co),'.new',sep='')
z<-merge(co.v1,co,by=0)

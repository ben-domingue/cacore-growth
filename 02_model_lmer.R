##my alt

growth<-function(df,reliability=0.9,g11,sampling=NULL) {
    library(mecor)
    library(shrink)
    ##sampling
    if (!is.null(sampling)) {
        ids<-sample(unique(df$school_code),sampling)
        df<-df[df$school_code %in% ids,]
    }
    output<-list() #object to return
    z<-df[,c("school_code","scale_score","lag_scale_score","alt_lag_scale_score")]
    df0<-df[rowSums(is.na(z))==0,] #just for analysis
    rm("df") ##just to ensure i don't use it
########################################
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    df0$scale_score<-std(df0$scale_score)
    df0$lag_scale_score<-std(df0$lag_scale_score)
    df0$alt_lag_scale_score<-std(df0$alt_lag_scale_score)
########################################
    ##school matrix [the below is somewhat hacky as was originally designed to split into two groups (<11 and 11). now that things are entirely separate there is no harmonization that needs to be done.
    jrs<-df0[df0$grade==11,]
    oth<-df0[df0$grade<11,]
    if (nrow(oth)>0) {
        f<-function(df0) { #for oth
            ids<-unique(df0$school_code)
            ss<-data.frame(matrix(0,nrow=nrow(df0),ncol=length(ids)))
            names(ss)<-paste("schoolid_",ids,sep='')
            for (id in ids) ss[[paste("schoolid_",id,sep='')]]<-as.numeric(df0$school_code==id)
            ss
        }
        ss.oth<-f(oth)
        ss.oth$id<-oth$id
        df0<-merge(oth,ss.oth)
    } else {
        ##now juniors
        f<-function(jrs,g11) {
            g11.tmp<-g11[g11$id %in% jrs$id,]
            ids<-c(g11.tmp[,1],g11.tmp[,2],g11.tmp[,3])
            ids<-unique(ids)
            ids<-ids[!is.na(ids)]
            ss<-data.frame(matrix(0,nrow=nrow(g11.tmp),ncol=length(ids)))
            names(ss)<-paste("schoolid_",ids,sep='')
            for (i in 1:nrow(g11.tmp)) {
                index<-match(paste("schoolid_",g11.tmp[i,1:3],sep=""),names(ss))
                index<-index[!is.na(index)]
                for (j in index) ss[i,j]<-ss[i,j]+1
            }
            for (i in 1:ncol(ss)) ss[,i]<-ss[,i]/3
            ss<-ss[,colSums(ss)>1] ##those with super small samples are just going to cause problems
            ss$id<-g11.tmp$id
            ss
        }
        ss.jrs<-f(jrs,g11)
        df0<-merge(jrs,ss.jrs)
    }
########################################
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
########################################
    ##build formula
    ii<-grep("^schoolid",names(df0))
    ii<-ii[-1] #to ensure no collinearity problem with schools
    fm<-paste(
        "scale_score~MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))+alt_lag_scale_score+",
        paste(fm.list,collapse="+"),
        "+",
        paste(names(df0)[ii],collapse='+'),sep=''
    )
########################################
    ##step 1
    m1<-mecor(as.formula(fm),df0)
    ##fm0<-gsub("MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))","lag_scale_score",fm,fixed=TRUE)
    ##lm(as.formula(fm0),df0)
    m1coef<-m1$corfit$coef
    m1coef.noisy<-m1$uncorfit$coef
    m1N<-length(m1$uncorfit$residuals)
    output$m1<-list(m1coef=m1coef,m1coef.noisy=m1coef.noisy,m1N=m1N)
########################################
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
########################################
    ##step 2: regress pretest mean
    m2<-lm(errors~lag_mean,df0)
    df0$errors2<-m2$resid
    m2coef<-coef(m2)
    output$m2<-m2coef
########################################
    ##step3: almost there!
    ii<-grep("^schoolid",names(df0))
    ii<-ii[-1] #to ensure no collinearity problem with schools
    fm<-paste(
        "errors2~0+",
        paste(names(df0)[ii],collapse='+'),sep=''
    )
    m3<-lm(as.formula(fm),df0,x=TRUE,y=TRUE)
    m3coef<-summary(m3)$coef
    m3N<-length(m3$resid)
    m3resid<-data.frame(id=df0$id,school_code=df0$school_code,m3resid=m3$resid)
    output$m3<-list(m3coef=m3coef,m3N=m3N,m3resid=m3resid)
########################################
    #shr<-shrink::shrink(m3,method='dfbeta')
    #shr.est<-shr$ShrunkenRegCoef
    ##standardize//shrink [see page 10 of core report re shrinkage]
    S<-summary(m3)$coef
    fe<-grep("^schoolid_",rownames(S))
    co<-S[fe,1:2]
    co<-data.frame(co)
    names(co)<-c("fe","se")
    #co$fe<-co$fe-Hmisc::wtd.mean(co$fe,co$n,na.rm=TRUE) #mean-center
    nms<-rownames(co)
    out<-list()
    for (nm in nms) out[[nm]]<-sum(df0[[nm]])
    co$n<-unlist(out)
    co$fe<-co$fe-mean(co$fe,na.rm=TRUE)
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
    shrink.matrix<-function(co,m3=m3) {
        library(Hmisc)
        alpha<-matrix(co$fe,ncol=1)
        ##compute omega
        var.alpha<-wtd.var(co$fe,co$n)
        mean.se<-wtd.mean(co$se^2,co$n)
        omega<-var.alpha-mean.se
        print(omega)
        ##
        mat<-vcov(m3)
        ii<-match(rownames(co),rownames(mat))
        sig<-mat[ii,ii]
        I<-diag(nrow(sig))
                 co$eb<- (omega*I) %*% solve(omega*I+sig) %*% alpha
                 co$eb.se<- sqrt(diag((omega*I) %*% solve(omega*I+sig) %*% sig))
                 return(co)
    }
    co<-shrink.univariate(co)
    #co<-shrink.matrix(co,m3)
########################################
    ##conversion to percentiles
    m<-mean(co$eb,na.rm=TRUE)
    s<-sd(co$eb,na.rm=TRUE)
    co$per<-pnorm(co$eb,m,s)
    output$coef<-co
########################################
    print(summary(co$eb/co$fe))
    return(output)
}


load("_g11.Rdata")
##reliabilities, see table 8.3. https://www.cde.ca.gov/ta/tg/ca/documents/sbcaaspptechrpt24.docx
reliabilities<-c(`_ela`=0.88,`_math`=0.9)
estimates<-list()

for (nm in c("_ela","_math")) {
    load(paste(nm,".Rdata",sep=''))
    ##
    df<-df[df$year==2025,]
    df<-df[df$grade>3,]
    ##analysis by school
    ##print(1)
    ##co<-growth(df,reliability=reliabilities[nm],g11=g11,sampling=50)
    ##analysis by schoolXgrade
    df$school_original<-df$school_code
    df$school_code<-paste(df$school_original,df$grade,sep='__')
    L<-split(df,df$grade)
    out<-list()
    for (i in 1:length(L)) {
        x<-L[[i]]
        gr<-unique(x$grade)
        print(gr)
        if (gr==11) g11.pass<-g11 else g11.pass<-NULL
        out[[as.character(gr)]]<-growth(x,reliability=reliabilities[nm],g11=g11.pass)
    }
    ##
    estimates[[nm]]<-out
}

save(estimates,file="_estimates.Rdata")

#######################################################
##load("_g11_2024.Rdata")
reliabilities<-c(`_ela`=0.88,`_math`=0.9)
estimates<-list()
for (nm in c("_ela","_math")) {
    load(paste(nm,".Rdata",sep=''))
    ##
    df<-df[df$year==2024,]
    df<-df[df$grade %in% 4:8,]
    ##analysis by schoolXgrade
    df$school_original<-df$school_code
    df$school_code<-paste(df$school_original,df$grade,sep='__')
    L<-split(df,df$grade)
    out<-list()
    for (i in 1:length(L)) {
        x<-L[[i]]
        gr<-unique(x$grade)
        print(gr)
        if (gr==11) g11.pass<-g11 else g11.pass<-NULL
        out[[as.character(gr)]]<-growth(x,reliability=reliabilities[nm],g11=g11.pass)
    }
    ##
    estimates[[nm]]<-out
}

save(estimates,file="_estimates2024.Rdata")


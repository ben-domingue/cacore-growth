
f<-function(df) {
    tab<-list()
    df<-df[df$year==2025,]
    ##
    tab$totalN<-table(df$year,df$grade)
    ##
    L<-split(df,df$grade)
    ##
    f<-function(x) {
        ll<-split(x,x$school_code)
        length(ll)
    }
    tab$Nschool<-sapply(L,f)
    ##
    f<-function(x) {
        ll<-split(x,x$school_code)
        n<-sapply(ll,nrow)
        mean(n)
    }
    tab$school.meanN<-sapply(L,f)
    ##
    f<-function(x) {
        sum(is.na(x$lag_scale_score))/nrow(x)
    }
    tab$nolag<-sapply(L,f)
    ##
    tab$scale_score<-unlist(by(df$scale_score,df$grade,mean,na.rm=TRUE))
    tab$sd_score<-unlist(by(df$scale_score,df$grade,sd,na.rm=TRUE))
    delta<-df$scale_score-df$lag_scale_score
    tab$delta<-unlist(by(delta,df$grade,mean,na.rm=TRUE))
    ##
    do.call("rbind",tab)
}

load("_ela.Rdata")
r<-f(df)
nrow(df)
load("_math.Rdata")
m<-f(df)
nrow(df)
length(unique(df$school_code))

tab<-rbind(r,m)
write.csv(tab,'')


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
    do.call("rbind",tab)
}

load("_ela.Rdata")
r<-f(df)
load("_math.Rdata")
m<-f(df)

tab<-rbind(r,m)
write.csv(tab,'')

agg<-function(x) { ##x should be a dataframe of estimates you want to aggregate based on a `id` column
    ##needs to have columns:
    ##id: the identifer you are going to split on to get estimates
    ##fe: the original fixed effect estimates
    ##se: that estimate's standar error
    ##n: the sample size
    L<-split(x,x$id)
    f<-function(x) {
        m<-Hmisc::wtd.mean(x$fe,x$n,na.rm=TRUE)
        N<-sum(x$n)
        nn<-(x$n/N)^2
        s<-sqrt(sum(nn*x$se^2))
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


##example usage
x <-
structure(list(id = c("100180", "100792", "100792", "102277", 
"102277"), fe = c(0.079447632689757491, -0.10151685646264125, 
-0.07732656324934066, -0.021838261288718099, 0.019924534120988366
), se = c(0.35067973083825299, 0.086371711201888482, 0.090202839992377049, 
0.066289435647257103, 0.058940933013649235), n = c(4, 43, 38, 
73, 89)), class = "data.frame", row.names = c(NA, -5L))
agg(x)


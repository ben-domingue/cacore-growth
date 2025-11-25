b1<-1
b2<-.5

sd.err<-sort(runif(50,0,1))
##
out<-list()
list<-for (sig in sd.err) {
    x0<-rnorm(1000)
    e<-rnorm(1000,sd=sig)
    x<-x0+e
    reliability<-var(x0)/(var(x0)+var(e))
    ##
    y<-rnorm(1000)
    z<-b1*x0+b2*y+rnorm(length(x))
    est.naive<-coef(lm(z~x+y))[2]
    est.true<-coef(lm(z~x0+y))[2]
    ##
    df<-data.frame(x=x,y=y,z=z)
    library(mecor)
    est<-mecor(z ~ MeasErrorRandom(x, variance = var(x)*(1-reliability)) +y,df)$corfit$coef[2]
    out[[as.character(sig)]]<-c(sig,est.naive,est.true,est)
}
x<-do.call("rbind",out)

plot(x[,1],x[,2],type='l')
lines(x[,1],x[,3],col='gray')
lines(x[,1],x[,4],col='red')

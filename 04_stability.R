load("_estimates.Rdata")
e25<-estimates
load("_estimates2024.Rdata")
e24<-estimates

f<-function(estimates) {
    grest<-list() ##get growth estimates `grest`
    for (i in 1:length(estimates)) {
        z1<-estimates[[i]]$co$coef
        z2<-estimates[[i]]$co.grade$coef
        grest[[names(estimates)[i] ]]<-list(co=z1,co.grade=z2)
    }
    grest
}
g25<-f(e25)
g24<-f(e24)

tab<-list()
for (i in 1:2) {
    x<-g24[[i]]$co.grade
    y<-g25[[i]]$co.grade
    names(x)<-paste(names(x),'_24',sep='')
    names(y)<-paste(names(y),'_25',sep='')
    z<-merge(x,y,by=0)
    c1<-weights::wtd.cor(z$fe_24,z$fe_25,z$n_25)[1]
    c2<-weights::wtd.cor(z$eb_24,z$eb_25,z$n_25)[1]
    c3<-weights::wtd.cor(z$per_24,z$per_25,z$n_25)[1]
    tab[[i]]<-c(c1,c2,c3)
}
tab<-do.call("cbind",tab)

write.csv(tab,'')


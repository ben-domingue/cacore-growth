load("_sch.Rdata")
e25<-sch[,c("id","eb_ela","per_ela","eb_math","per_math")]
load("_sch2024.Rdata")
e24<-sch[,c("id","eb_ela","per_ela","eb_math","per_math","n_ela")]
x<-merge(e24,e25,by='id')


tab<-list()
c2<-weights::wtd.cor(x$eb_ela.x,x$eb_ela.y,x$n_ela)[1]
c3<-weights::wtd.cor(x$per_ela.x,x$per_math.y,x$n_ela)[1]
tab[[1]]<-c(c2,c3)
##
c2<-weights::wtd.cor(x$eb_math.x,x$eb_math.y,x$n_ela)[1]
c3<-weights::wtd.cor(x$per_math.x,x$per_math.y,x$n_ela)[1]
tab[[2]]<-c(c2,c3)
tab<-do.call("cbind",tab)

write.csv(tab,'')


###################################
##grade-specific correlations
load("_estimates.Rdata")
e25<-estimates
load("_estimates2024.Rdata")
e24<-estimates

coors<-list()
for (j in 1:2) {
    coor<-numeric()
    for (i in 1:length(estimates[[j]])) {
        x25<-e25[[j]][[i]]$coef
        x24<-e24[[j]][[i]]$coef
        z<-merge(x25,x24,by=0)
        #coor[i]<-weights::wtd.cor(z$eb.x,z$eb.y,z$n.x)
        coor[i]<-cor(z$eb.x,z$eb.y,use='p')
    }
    coors[[j]]<-coor
}
tab<-do.call("cbind",coors)
rownames(tab)<-names(estimates[[1]])
write.csv(tab,'')

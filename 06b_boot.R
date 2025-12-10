##to get variation in subgroup estimates; needed in 06_

source("0_growth.R")

load("_g11.Rdata")
##reliabilities, see table 8.3. https://www.cde.ca.gov/ta/tg/ca/documents/sbcaaspptechrpt24.docx
reliabilities<-c(`_ela`=0.88,`_math`=0.9)
estimates<-list()

nboot<-5
nc<-5

parfun<-function(arg,df,reliability,g11.pass) {
    print(arg)
    df<-df[sample(1:nrow(df),nrow(df),replace=TRUE),]
    ##making ids ok
    id.post<-1:nrow(df)
    new.id<-paste(df$id,id.post,sep="_")
    df$id<-ifelse(df$grade!=11,new.id,df$id)
    ##
    L<-split(df,df$grade)
    out<-list()
    for (i in 1:length(L)) {
        x<-L[[i]]
        gr<-unique(x$grade)
        if (gr==11) g11.pass<-g11 else g11.pass<-NULL
        z<-growth(x,reliability=reliabilities[nm],g11=g11.pass)
        out[[i]]<-z$m2$m2q
    }
    data.frame(do.call("rbind",out))
}

for (nm in c("_ela","_math")) {
    load(paste(nm,".Rdata",sep=''))
    ##
    df<-df[df$year==2025,]
    df<-df[df$grade>3,]
    ##analysis by schoolXgrade
    df$school_original<-df$school_code
    df$school_code<-paste(df$school_original,df$grade,sep='__')
    library(parallel)
    arg<-1:nboot
    boot.out<-mclapply(arg,parfun,mc.cores=nc,
                       df=df,reliability=reliabilities[nm],g11.pass=g11.pass)
    estimates[[nm]]<-boot.out
}

boot<-estimates
save(boot,file="_boot.Rdata")



##with 10 cores/500g
## > proc.time()
##      user    system   elapsed 
## 49070.524 49136.821  9066.295 

##with20cores/1000g
## > proc.time()
##      user    system   elapsed 
##  76801.65 170494.84  11954.17 

load("_boot.Rdata")
for (i in 1:length(boot)) {
    for (j in 1:length(boot[[i]])) {
        sh<-boot[[i]][[j]]
        txt<-strsplit(sh$id,"_")
        sh$id<-sapply(txt,function(x) x[1])
        boot[[i]][[j]]<-sh
    }
}
save(boot,file="_boot2.Rdata")

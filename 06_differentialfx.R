## Growth measures for student subgroups within schools are produced in addition to the overall
## growth measures described above. Subgroup measures are produced by
## The analysis that produces subgroup growth measures is conducted separately
## for each class of subgroups. In other words, the subgroup analysis conducted for the set of
## race subgroups is separate from the analysis conducted for the set of disability subgroups.


source("/home/bdomingu/Dropbox/projects/ca_growth/src/0_growth.R")


load("_g11.Rdata")
##reliabilities, see table 8.3. https://www.cde.ca.gov/ta/tg/ca/documents/sbcaaspptechrpt24.docx
reliabilities<-c(`_ela`=0.88,`_math`=0.9)
estimates<-list()

for (nm in c("_ela","_math")) {
    load(paste(nm,".Rdata",sep=''))
    ##
    df<-df[df$year==2025,]
    df<-df[df$grade>3,]
######################################################
    ##analysis by subgroup
    L<-list()
    ## race (Asian, African American, Filipino, Hispanic/Latinx, Pacific Islander, Multiracial, Native American, White, and race missing);
    df$race_eth_rollup<-ifelse(df$race_eth_rollup=="",NA,df$race_eth_rollup)
    L$race<-split(df,df$race_eth_rollup)
    ## English language learner (ELL, not ELL);
    df$ell<-ifelse(is.na(df$ell_level),0,1)
    L$ell<-split(df,df$ell)
    ## disability (with disability, without disability);
    df$disability<-ifelse(df$disability_type=="",0,1)
    L$disability<-split(df,df$disability)
    ## economic disadvantage (economically disadvantaged, not economically disadvantaged);
    L$disadvantage<-split(df,df$socioecon_disadvantaged)
    ## and pretest performance (low pretest performance, high pretest performance).
    tmp<-df[!is.na(df$lag_achievement_level),]
    tmp$lowpretest<-ifelse(tmp$lag_achievement_level %in% 1:2,1,0)
    L$pretest<-split(tmp,tmp$lowpretest)
######################################################
    ##formula mods
    fm0<-list(race=NULL,
                  ell="ell_level",
                  disability="disability_type",
                  disadvantage=c("socioecon_disadvantaged","foster","homeless"),
                  pretest=NULL
                  )
    ntimes<-sapply(L,length)
    fm.list<-list()
    for (i in 1:length(ntimes)) for (j in 1:ntimes[i]) fm.list[[paste(i,j)]]<-fm0[i]
    L<-do.call("c",L)
    ##pruning for small samples
    props<-sapply(L,nrow)/nrow(df)
    test<- props>.01
    L<-L[test]
    fm.list<-fm.list[test]
######################################################
    out<-list()
    for (i in 1:length(L)) {
        print(i)
        x<-L[[i]]
        print(nrow(x))
        out[[names(L)[i] ]]<-growth(x,
                                        reliability=reliabilities[nm],g11=g11,
                                        fm.edit=fm.list[[i]][[1]]
                                        )
    }
    ##
    estimates[[nm]]<-out
}

save(estimates,file="_difffx.Rdata")

######################################################
load("_difffx.Rdata")

##analysis
f<-function(x) {
    n<-x$m3$m3N
    lag<-as.numeric(x$m1$m1coef[2])
    ns<-nrow(x$coef)
    sig<-sd(x$coef$eb)
    shr=min(x$coef$eb/x$coef$fe,na.rm=TRUE)
    c(n=n,ns=ns,lag=lag,sig=sig,shrink=shr)
}
#do.call("rbind",lapply(out,f))

tab<-lapply(estimates[["_ela"]],f)
tab1<-do.call("rbind",tab)
tab<-lapply(estimates[["_math"]],f)
tab2<-do.call("rbind",tab)
write.csv(rbind(tab1,tab2)[,-5])

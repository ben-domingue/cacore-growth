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
source("/home/bdomingu/Dropbox/projects/ca_growth/src/0_growth.R")

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


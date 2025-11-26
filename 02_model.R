##need to get reliabilities
##Additionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 

load("_ela.Rdata")

ids<-sample(unique(df$school_code),100)
y<-df[df$school_code %in% ids,]

z<-y[,c("school_code","scale_score","lag_scale_score")]
y<-y[rowSums(is.na(z))==0,]

m<-lm(scale_score~lag_scale_score+factor(school_code),y)
library(mecor)
reliability<-0.9
m1<-mecor(scale_score~MeasErrorRandom(lag_scale_score,var(lag_scale_score,na.rm=TRUE)*(1-reliability))+factor(school_code),y)
coef(m)[2]
m1$corfit$coef[2]

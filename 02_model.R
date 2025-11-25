##what years? covid complicates things
##they were doing something wonky for high school. 

##school-level variable: pretest score
##Additionally, all models control for disability, English language learner, economic disadvantage, foster care, and homelessness statuses at the student level. 

x<-read.csv("agg_caaspp_math_ela_by_student_grade_year.csv")
x$school<-sample(1:1000,nrow(x),replace=TRUE)
x<-x[x$spring_year>2020,]

x0<-x[,c("state_student_id","spring_year","grade_assessed","math_scale_score")]
x0$spring_year<-x0$spring_year+1
names(x0)[3:4]<-paste("lag_",names(x0)[3:4],sep='')
x<-merge(x,x0) #this will break grade 11

ids<-sample(unique(x$school),100)
y<-x[x$school %in% ids,]

m<-lm(math_scale_score~lag_math_scale_score+factor(school),y)
library(mecor)

reliability<-0.9
y<-y[!is.na(y$lag_math_scale_score),]
y<-y[!is.na(y$math_scale_score),]
m1<-mecor(math_scale_score~MeasErrorRandom(lag_math_scale_score,var(lag_math_scale_score,na.rm=TRUE)*(1-reliability))+factor(school),y)
coef(m)[2]
m1$corfit$coef[2]

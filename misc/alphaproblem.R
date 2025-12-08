##https://domingue-lab.slack.com/archives/C0A00JZ7P1P/p1764521042707479

N<-10000
n<-25
sigma<- 0.02

x<-rnorm(N)
gr<-sample(1:n,N,replace=TRUE)
fe<-rnorm(n,sd=sigma)
y<-.3*x+fe[gr]+rnorm(N)

m<-lm(y~x+factor(gr))
ii<-grep("^factor",names(coef(m)))
S<-summary(m)$coef[ii,1:2]
omega2<-var(S[,1])-mean(S[,2]^2)
omega2


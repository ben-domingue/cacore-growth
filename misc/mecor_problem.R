mecor(ir_ln ~ MeasErrorRandom(wc, variance = 0.25) + sex + age + tbf,data = vat)

vat$cov<-sample(c("foo","bar"),nrow(vat),replace=TRUE)
mecor(ir_ln ~ MeasErrorRandom(wc, variance = 0.25) + sex + age + tbf+vat,data = vat) ##problem


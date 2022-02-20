tab<-read.table(file="C:\\Users\\pc\\Downloads\\HortonGeneralHospital.csv", header = TRUE, sep=",")
tab
myhist=hist(tab$Adm, col = 'red')
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(tab$Adm,p=p) # percentiles of the sample distribution
plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="blue", lty=2)
shapiro.test(tab$Adm)
library(nortest) 
ad.test(tab$Adm)
cvm.test(tab$Adm)
lillie.test(tab$Adm)
sf.test(tab$Adm)
m=mean(tab$Adm)
m
t.test(tab$Adm, mu = 500)
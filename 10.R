tab<-read.table(file="C:\\Users\\pc\\Downloads\\Absenteeism_at_work.csv", header = TRUE, sep=";")
head(tab)
resp_dis = tab[tab$Reason.for.absence == 10, ]
head(resp_dis)
myhist=hist(resp_dis$Age, col = 'red')
library(nortest)
shapiro.test(resp_dis$Age)
ad.test(resp_dis$Age)
cvm.test(resp_dis$Age)
lillie.test(resp_dis$Age)
sf.test(resp_dis$Age)

cir_dis = tab[tab$Reason.for.absence == 26, ]
head(cir_dis)
myhist=hist(cir_dis$Age, col = 'red')
shapiro.test(cir_dis$Age)
sf.test(cir_dis$Age)
ad.test(cir_dis$Age)
cvm.test(cir_dis$Age)
lillie.test(cir_dis$Age)

common = tab[tab$Reason.for.absence %in% c(10,26),  ]
d = abs( mean(cir_dis$Age) - mean(resp_dis$Age) ) / sd(common$Reason.for.absence)

pwr.t.test(d = 0.8, power = 0.9, sig.level = 0.05)
pwr.t.test(d = 0.9, power = 0.95, sig.level = 0.02)
pwr.t.test(n=20, d=.5, sig.level=.01)
pwr.t2n.test(n1=15, n2=25, d=.5, sig.level=.01)
t.test(cir_dis$Age, mu = mean(resp_dis$Age) )

n  = 10
mu = 100
sigma = 10  
sample=rnorm(n,mu,sigma)
sample
m=mean(sample)
m
s=sd(sample)
s
myhist=hist(sample,col="red")
n_k=myhist$counts #частоты
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(sample) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP

p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(sample,p=p) # percentiles of the sample distribution
plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="blue", lty=2)

n  = 10000 
mu = 100
sigma = 10  
sample=rnorm(n,mu,sigma)
sample
myhist=hist(sample,col="red")
n_k=myhist$counts #частоты
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(sample) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
myChiSq
P
N
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(sample,p=p) # percentiles of the sample distribution
plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="blue", lty=2)

n  = 10 
sample=rexp(n,rate=2)
sample
m=mean(sample)
m
s=sd(sample)
s
myhist=hist(sample,col="red")
n_k=myhist$counts #частоты
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(sample) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
myChiSq
P
N
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP

p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(sample,p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="blue", lty=2)

n  = 10000 
sample=rexp(n,rate=2)
head(sample)
myhist=hist(sample,col="red")
n_k=myhist$counts #частоты
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(sample) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
myChiSq
P
N
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(sample,p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="blue", lty=2)


tab<-read.table(file="C:\\Users\\pc\\Downloads\\HortonGeneralHospital.csv", header = TRUE, sep=",")
tab
myhist=hist(tab$Adm, col = 'red')
m=mean(tab$Adm)
m
s=sd(tab$Adm)
s
n_k=myhist$counts #chastoty
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(tab$Adm) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
myChiSq
P
N
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(tab$Adm,p=p) # percentiles of the sample distribution
plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="blue", lty=2)


tab<-read.table(file="C:\\Users\\pc\\Downloads\\seeds_dataset.txt", header = TRUE)
tab
myhist=hist(tab$X5.22, col = 'red')
m=mean(tab$X5.22)
m
s=sd(tab$X5.22)
s
n_k=myhist$counts #chastoty
K=length(myhist$counts) #число интервалов
a_k=myhist$breaks #границы интервалов
a_k
n=length(tab$X5.22) #объем выборки
myChiSq=0 #наблюдаемое значение Хи-квадрат
P=0 #сумма вероятностей (д.б. =1)
N=0 #сумма частот (д.б. =n)
for (k in 1:K)
{
  p_k=pnorm(a_k[k+1], m, s) - pnorm(a_k[k],m,s) #вероятность попадания в интервал
  print(p_k)
  P=P+p_k
  N=N+n_k[k]
  myChiSq=myChiSq+(n*p_k-n_k[k])^2/(n*p_k);
}
myChiSq
P
N
fd=K-1-2
myP=1-pchisq(myChiSq,fd)
myP
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(tab$X5.22,p=p) # percentiles of the sample distribution
plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="blue", lty=2)

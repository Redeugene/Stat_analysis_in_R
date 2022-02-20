library(mice)
library(VIM)
tab<-read.table(file="C:\\Users\\pc\\Downloads\\StoneFlakes.dat", header = TRUE, sep=",")
data(tab,"VIM")
head(tab)
tab[complete.cases(tab),]
tab[!complete.cases(tab),]
sum(is.na(tab))
md.pattern(tab)
matrixplot(tab)
x <- as.data.frame (abs (is.na (tab))) 
y <- x[, which(colSums(x) > 0)] 
print(cor(y),2)
cor(tab, y, use="pairwise.complete.obs")
imp <- mice(tab, seed=1234)
fit <- with(imp, lm(FLA ~ ZDF1 + RTI))
pooled <- pool(fit)
summary(pooled)
tab_imp <- complete(imp, action=4)
tab_imp
tab[!complete.cases(tab_imp),]
save(tab_imp, file="tab_imp.Rdata")

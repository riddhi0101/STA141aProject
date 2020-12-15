## matrix completion code
library(filling)

data.mat = data.matrix(review_data)
data.mat = data.mat[,2:25]
data.mat = t(data.mat)
dim(data.mat)

incomp10 = aux.rndmissing(data.mat, x = .1)
incomp75 = aux.rndmissing(data.mat, x = 0.75)

## 10% incomplete
aa = seq(1,101, by=10)
lambdasL = rev(aa)
comp10 = fill.SoftImpute(incomp10,lambdas =lambdasL)
comp10 = comp10$X

## calculating total residual for each lambda value
d = dim(comp10)
normC = c()
for (i in 1:d[3]){
    r = data.mat - comp10[,,i]
    a = norm(r,type = "F")
    normC = append(normC, a)}
minE = which(normC == min(normC))
bestcomp10 = comp10[,,minE]
plot(aa, rev(normC), xlab= "lambda values", ylab = "total residual", main= "Residuals of Lambda values (10% incomplete)")

## residual calculations

obsvRdata = incomp10 - bestcomp10
obsvRdata = as.vector(obsvRdata)
obsvR = sum(obsvRdata*obsvRdata, na.rm = T)
totR = min(normC)
unobR = totR-obsvR

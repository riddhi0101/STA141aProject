## matrix completion code
library(filling)

data.mat = data.matrix(review_data)
data.mat = data.mat[,2:25]
data.mat = t(data.mat)
dim(data.mat)

incomp10 = aux.rndmissing(data.mat, x = .1)
incomp75 = aux.rndmissing(data.mat, x = 0.75)

aa = seq(.1,1, by=.1)
lambdasL = rev(aa)
lambdasL = 11
comp10 = fill.SoftImpute(incomp10,lambdas =lambdasL)
comp10 = comp10$X

comp10l10 = comp10[,,1]
r = data.mat - comp10l10
norm(r,type = "F")

d = dim(comp10)
normC.1to1 = c()
for (i in 1:d[3]){
    print(i)
    r = data.mat - comp10[,,i]
    a = norm(r,type = "F")
    normC.1to1 = append(normC, a)}

#plot(a,normC)
normC


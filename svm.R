set.seed(1)
x=matrix(rnorm(2*200), ncol=2)
x[1:100,] = x[1:100, ] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1,150), rep(2,50))
dat= data.frame(x=x, y=as.factor(y))

#train
train=sample(200,100)
svmfit= svm(y~., data=dat[train,], kernel = "radial", gamma= 1, cost=1)
summary(svmfit)

#Perform crossvalidation to find best gamma and cost for SVM.
tune.out = tune(svm, y~., data=dat[train,], kernel='radial', ranges = list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

pred=predict(tune.out$best.model, newdata = dat[-train,])
table(true=dat[-train, "y"],pred)

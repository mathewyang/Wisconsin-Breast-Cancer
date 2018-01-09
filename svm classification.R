#Ch9 lab

#When the separation between classes is distinct, SVM is better. When the classes are closer then
#using Logistic Regression is better. (see loss func)

set.seed(1)
x=matrix(rnorm(2*20),ncol = 2)
y=c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,]+1
plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit = svm(y~. , 
             data = dat,
             kernel="linear",
             cost=.1,
             scale=FALSE)

plot(svmfit, dat)

set.seed(1)
tune.out = tune(svm, y~., data=dat, kernel = "linear", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

best.model = tune.out$best.model


xtest = matrix ( rnorm (20*2) , ncol =2)
ytest = sample ( c ( -1 ,1) , 20 , rep = TRUE )
xtest [ ytest ==1 ,]= xtest [ ytest ==1 ,] + 1
testdat = data.frame ( x = xtest , y = as.factor ( ytest ) )

ypred=predict(best.model, testdat)
table(predict=ypred, truth=testdat$y)

x [ y ==1 ,]= x [ y ==1 ,]+0.5
plot (x , col =( y +5) /2 , pch =19)

dat = data.frame( x =x , y = as.factor ( y ) )
svmfit = svm(y~., data = dat , kernel ="linear" , cost=1)
summary(svmfit)


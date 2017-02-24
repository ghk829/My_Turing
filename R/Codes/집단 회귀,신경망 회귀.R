library('forecast')
z=data.frame(1:150,(1:150)^2)
z
i=1:100
a=z[i,]
b=z[-i,]
i=1:400

tr=creditlog[i,]
tes=creditlog[-i,]
z=nn.train(as.matrix(tr[,-1]),tr[,1],hidden=c(10,10,10))
predict(z,as.matrix(tes[,-1]))
library(h2o)
tr <- as.h2o(
  tr,
  destination_frame = "ar")
tes <- as.h2o(tes,destination_frame = "ar")
colnames(tr)
xnames <- colnames(tr)[-1]
xnames
xnames=xnames[-c(5,6)]
colnames(tr)
xnames
ex1 <- h2o.deeplearning(
  x = xnames,
  y = "score",
  training_frame= tr,
  validation_frame = tes,
  hidden = c(100),
  epochs = 25,
  average_activation = 25,
  hidden_dropout_ratios = c(.1)
)
summary(ex1)
plot(ex1)
im(tes)
summary(ex1)
plot()
pr=predict(ex1,tes[,2:5])
pr=as.vector(pr)
par(mfrow=c(1,1))
plot(pr,type='o',ylim=c(20,80),col='green')
d=data.frame(pr,prr)
k=cbind(d,rowMeans(d))
prr=predict(fit,tes[,-c(1,6,7)])
lines(predict(fit,tes[,-c(1,6,7)]),type='o',ylim=c(20,80))
lines(k[,4],type='o',col='blue')
lines(as.vector(tes[,1]),type='o',col='red')
d=nnetar(a[,1])
plot(forecast(d))
mean((as.vector(tes[,1])-predict(fit))^2)
ex1
RMSE(prr,as.vector(tes[,1]))^2
RMSE(pr,as.vector(tes[,1]))^2
RMSE(k[,3],as.vector(tes[,1]))^2
k=cbind(as.vector(tes[,1]),k)
cor(k)
a=predict(fit)-as.vector(tes[,1])
b=predict(ex1)
mean((as.vector(tes[,1])-as.vector(predict(ex1,tes[,-1])))^2)
# 오히려 간단한 모델을 쓴 후 Averaging을 하는 것이 나
require(quantmod) 
library(deepnet)
require(nnet)
require(caret)
T = seq(0,20,length=200)

y = arima.sim(list(1,1,1),200)
dat <- data.frame( y, x1=Lag(y,1), x2=Lag(y,2))
names(dat) <- c('y','x1','x2')
#Fit model
model <- train(y ~ x1+x2 , 
               dat, 
               method='nnet', 
               linout=TRUE, 
               trace = FALSE)
ps <- predict(model, dat)

#Examine results
par(mfrow=c(2,1))
plot(T,y,type="l",col = 2)
lines(T,ps, col=3)
legend(5, 70, c("y", "pred"), cex=1.5, fill=2:3)


library(caret)
install.packages('fpp')
require(fpp)
data(credit)
creditlog  <- data.frame(score=credit$score,
                         log.savings=log(credit$savings+1),
                         log.income=log(credit$income+1),
                         log.address=log(credit$time.address+1),
                         log.employed=log(credit$time.employed+1),
                         fte=credit$fte, single=credit$single)
fit  <- avNNet(score ~ log.savings + log.income + log.address +
                 log.employed, data=tr, repeats=100, size=10, decay=0.1,
               linout=TRUE)
x
i=1:450
fit$model
tr=creditlog[1:450,]
tes=creditlog[-i,]
fit  <- avNNet(score ~ log.savings + log.income + log.address +
                 log.employed, data=tr, repeats=25, size=3, decay=0.1,
               linout=TRUE)
summary(fit)
names(tes)
par(mfrow=c(1,1))

plot.ts(creditlog)
data("sunspotarea")
sunspotarea
?avNNet
forecast(fit,tes[,-1])
predict(fit,tes[,-1])
############
library(hts)
install.packages('hts')
data(bts)
x=vn[1:50,]
y <- hts(x, nodes=list(2, c(3,2)))
library(hts)
y <- hts(x, nodes=list(3,c(1,4,3)))
length(y)
allf <- forecast(y,fmethod='arima' ,h=8)
plot(allf)
xx=vn[-i,]
yy<-hts(xx,nodes=list(3,c(1,4,3)))
yy<-gts(xx,nodes=list(3,c(1,4,3)))
gts
plot(yy)
y$bts
allf$bts

#########

abc <- ts(5 + matrix(sort(rnorm(1600)), ncol = 16, nrow = 100))
sex <- rep(c("female", "male"), each = 8)
state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 2)
gc <- rbind(sex, state) # a matrix consists of strings.
gn <- rbind(rep(1:2, each = 8), rep(1:8, 2)) # a numerical matrix
rownames(gc) <- rownames(gn) <- c("Sex", "State")
gn
x <- gts(abc, groups = gc)
y <- gts(abc, groups = gn)
plot(x)
plot(y)
z=forecast(x,fmethod = 'arima',h=8)
xxx=allts(x, forecasts = TRUE)
plot(z)
plot(xxx[,2:4])
dev.new()
allf[[1]]
plot(allf)
names(allf$labels)<-c('Total',c('location','fuck'))
names(allf$nodes)<-
dev.new()

auto.arima(yy)
plot(yy)
#############
Model Assessment
caret::RMSE()
?RMSE()
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the LVQ model
set.seed(7)
modelLvq <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", trControl=control)
# train the GBM model
set.seed(7)
modelGbm <- train(diabetes~., data=PimaIndiansDiabetes, method="gbm", trControl=control, verbose=FALSE)
# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=control)
# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

f=nnetar(1:100)
plot(forecast(f,h=8))
ff=auto.arima(1:100)
plot(forecast(ff,h=8))
#######


#http://www.svm-tutorial.com/2014/10/support-vector-regression-r/

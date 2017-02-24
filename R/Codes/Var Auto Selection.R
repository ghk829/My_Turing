library('vars')
library(astsa)
library(forecast)
x = cbind(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")
result=VARselect(x,lag.max=20)
result$selection
model=VAR(x,p=9)
model
res.model = restrict(model,method='ser',thresh=2)
p=predict(res.model,ahead=20,ci=0.95)
plot(p)
p$fcst$cmort
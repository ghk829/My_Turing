install.packages('rvest')
install.packages('forecast')
###########################################################################################
Scrapping <- function(original){
  library('rvest')
  text <- matrix(0,100,10)
  l <- c()
  for (x in 1:100){
    html <- read_html(paste(original,x,sep=""))
    nodes <- html_nodes(html,".num:nth-child(2) .p11")
    html.text <- html_text(nodes)
      for (i in 1:10){
if(length(strsplit(html.text[i],",")[[1]])==2){e<-as.numeric(strsplit(html.text[i],",")[[1]]);
	l[i] <- e[1]*1000 + e[2]} else
if(length(strsplit(html.text[i],",")[[1]])==1){e<-as.numeric(strsplit(html.text[i],",")[[1]]);
	l[i] <- e[1]} else
if(length(strsplit(html.text[i],",")[[1]])==3){e<-as.numeric(strsplit(html.text[i],",")[[1]]);
	l[i] <- e[1]*1000000+e[2]*1000+e[3]}}
    for (k in 1:10){
      text[101-x,11-k] <- l[k]}
  }
	data.v<-c()
for (i in 1:100){
	for(j in 1:10){
  data.v[10*(i-1)+j]<-text[i,j]}}
	return(data.v)}
code=function(){
	Input = scan(what="",n=1)
	original = paste("http://finance.naver.com/item/sise_day.nhn?code=",Input,"&page=",sep="")
	data=Scrapping(original)
	return(data)}
#########################################
stock=code()
############################################################################################################
if(is.na(sum(stock))){
    stock<-stock[(which(is.na(stock))[length(which(is.na(stock)))]
                                    +1):1000]}
is.na(stock)
length(stock)
####################################################################################################
  fit<-arima(stock,order=c(1,0,1))
  predict.stock1<-predict(fit,n.ahead=100)
  fit<-arima(stock[700:1000],order=c(1,0,1))
  predict.stock2<-predict(fit,n.ahead=30)
  fit<-arima(stock[800:1000],order=c(1,0,1))
  predict.stock3<-predict(fit,n.ahead=20)
  fit<-arima(stock[900:1000],order=c(1,0,1))
  predict.stock4<-predict(fit,n.ahead=10)
####################################################################################
#1000days
mean(predict.stock1$pred) # 팔아야 할 값
predict.stock1$pred
predict.stock1$pred + 0.5*predict.stock1$se # 상위 위험선
predict.stock1$pred - 0.5*predict.stock1$se # 하위 위험선
#################################################################################
#300days
mean(predict.stock2$pred) # 팔아야 할 값
predict.stock2$pred
predict.stock2$pred + 1*predict.stock2$se # 상위 위험선
predict.stock2$pred - 1*predict.stock2$se # 하위 위험선
####################################################################################################
#200days
mean(predict.stock3$pred) # 팔아야 할 값
predict.stock3$pred
predict.stock3$pred + 1.5*predict.stock3$se # 상위 위험선
predict.stock3$pred - 1.5*predict.stock3$se # 하위 위험선
####################################################################################################
#100days
mean(predict.stock4$pred) # 팔아야 할 값
predict.stock4$pred
predict.stock4$pred + 2*predict.stock4$se # 상위 위험선
predict.stock4$pred - 2*predict.stock4$se # 하위 위험선
#################################################################################
par(mfrow=c(2,2))
plot(stock,xlim=c(0,1100),xlab='days',ylab='Price',main='1000days')
lines(predict.stock1$pred,col="blue")
lines(predict.stock1$pred+1.2*predict.stock1$se,col="red",lty=3)
lines(predict.stock1$pred-1.2*predict.stock1$se,col="red",lty=3)
plot(stock[700:1000],xlim=c(0,400),main='300days',xlab='days',ylab='Price')
lines(predict.stock2$pred,col="blue")
lines(predict.stock2$pred+1*predict.stock2$se,col="red",lty=3)
lines(predict.stock2$pred-1*predict.stock2$se,col="red",lty=3)
plot(stock[800:1000],xlim=c(0,300),main='200days',xlab='days',ylab='Price')
lines(predict.stock3$pred,col="blue")
lines(predict.stock3$pred+0.8*predict.stock3$se,col="red",lty=3)
lines(predict.stock3$pred-0.8*predict.stock3$se,col="red",lty=3)
plot(stock[900:1000],xlim=c(0,200),main='100days',xlab='days',ylab='Price')
lines(predict.stock4$pred,col="blue")
lines(predict.stock4$pred+0.5*predict.stock4$se,col="red",lty=3)
lines(predict.stock4$pred-0.5*predict.stock4$se,col="red",lty=3)
###################################################################
#원하는 위치의 가격
locator(4)
#################################################################
#weibull 근사
breaks<-function(){
  class.n <- ceiling(log(length(stock),2)+1);
  hist.breaks <- pretty(stock, class.n);
  md<-hist.breaks;
  while(hist.breaks[1]>10){hist.breaks<-hist.breaks/10};
  data<-hist.breaks-hist.breaks[1];
  hist.interval <- cut(stock,breaks=md,include.lowest = T);
  r<-hist.interval; ct<-c();
  for (i in 1:length(table(r)))
  ct[i]<-table(r)[[i]]
  return(ct)}
class.n <- ceiling(log(length(stock),2)+1);
hist.breaks <- pretty(stock, class.n);
md<-hist.breaks;
while(hist.breaks[1]>10){hist.breaks<-hist.breaks/10};
data<-hist.breaks-hist.breaks[1];
hist.interval <- cut(stock,breaks=md,include.lowest = T)
#####################################
wei<-breaks()
wei
length(wei)
[2]-seq(0,7,length=length(wei)+1)[1]
###########################################################################
# 카이 통계량으로 근사화
c<-matrix(0,101,101)
vec<-c()
for (i in seq(1,6,by=0.05)){
  for(j in seq(1,6,by=0.05)){
    for (k in 1:length(wei)){
      vec[k] <- pweibull(seq(0,seq(0,7,length=length(wei)+1)[length(wei)],by=seq(0,7,length=length(wei)+1)[2])[k]+seq(0,7,length=length(wei)+1)[2],shape=i,scale=j) - pweibull(seq(0,seq(0,7,length=length(wei)+1)[length(wei)],by=seq(0,7,length=length(wei)+1)[2])[k],shape=i,scale=j);
      vec[1]<-1-sum(vec)+vec[1];
      c[(i*100-95)/5,(j*100-95)/5]<-sum((wei-vec*1000)^2/(vec*1000))
    }}}
k<-c[which(c!=0,arr.ind=T)]
min(k)
which(c==min(k),arr.ind=T)[1,][1]
which(c==min(k),arr.ind=T)[1,][2]
seq(1,6,by=0.05)[which(c==min(k),arr.ind=T)[1,][1]]
seq(1,6,by=0.05)[which(c==min(k),arr.ind=T)[1,][2]]
barplot(table(hist.interval),xaxt='n',yaxt='n',xlab='',ylab='',col='skyblue')
par(new=T)
curve(dweibull(x,seq(1,6,by=0.05)[which(c==min(k),arr.ind=T)[1,][1]],seq(1,6,by=0.05)[which(c==min(k),arr.ind=T)[1,][2]]),xlim=c(0,7),col='red',ylab='weibull')
####################################################################################
# 적합도 검정 p.value로 근사화
vec<- array(0,dim=c(101,101,length(wei)))
for (i in seq(1,6,by=0.05)){
  for(j in seq(1,6,by=0.05)){
    for(k in 1:length(wei)){
      vec[(i*100-95)/5,(j*100-95)/5,k] <- pweibull(seq(0,seq(0,7,length=length(wei)+1)[length(wei)],by=seq(0,7,length=length(wei)+1)[2])[k]+seq(0,7,length=length(wei)+1)[2],shape=i,scale=j) - pweibull(seq(0,seq(0,7,length=length(wei)+1)[length(wei)],by=seq(0,7,length=length(wei)+1)[2])[k],shape=i,scale=j);
      vec[i,j,length(wei)]<-1-sum(vec[i,j,])+vec[i,j,length(wei)]
    }}}
for (i in 1:101){
  for(j in 1:101){
    if(sum(vec[i,j,])==1){
      c[i,j]<-chisq.test(wei,p=vec[i,j,])$p.value}}}

##################################################
k<-c[which(c!=0,arr.ind=T)]
max(k)
which(c==max(k),arr.ind=T)[1,][1]
barplot(table(hist.interval),xaxt='n',yaxt='n',xlab='',ylab='',col='skyblue')
par(new=T)
curve(dweibull(x,seq(1,6,by=0.05)[which(c==max(k),arr.ind=T)[1,][1]],seq(1,6,by=0.05)[which(c==max(k),arr.ind=T)[1,][2]]),xlim=c(0,7),col='red',ylab='weibull')
plot(table(stock),col='skyblue',xaxt='n',yaxt='n',xlab='',ylab='')
curve(dweibull(x,3.7,1.9),xlim=c(0,7))
which(seq(1,6,by=0.05)==2)
seq(1,6,by=0.05)[which(c==max(k),arr.ind=T)[1,][1]]
seq(1,6,by=0.05)[which(c==max(k),arr.ind=T)[1,][2]]

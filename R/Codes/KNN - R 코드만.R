wbcd<-read.csv("hm.csv",stringsAsFactors=F)
str(wbcd)
wbcd<-wbcd[-1] # id 삭제
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
summary(wbcd) # 단위가 굉장히 다름 -> 정규화가 필요
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize)) # 리스트를 데이터프레임형식으로 변환
wbcd_train<-wbcd_n[1:469,]
wbcd_test <-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

prop.table(table(wbcd_train_labels))
prop.table(table(wbcd_test_labels)) # 데이터 분할이 골고루 잘 되었는지 확인
library(class)
wbcd_test_pred<-knn(train=wbcd_train,
                    test=wbcd_test,
                    cl=wbcd_train_labels, # class : train 데이터의 각 행에 대한 범주인 팩터 벡터
                    k=21)
wbcd_test_pred
table(wbcd_test_pred)
install.packages('gmodels')
library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE,prop.c=FALSE)

library(dplyr)
library(hflights)
dim(hflights)
str(hflights)
summarise(hflights$TailNum)
hflights_df <- tbl_df(hflights)

install.packages('data.table',dependencies = T)
library(data.table)
setwd('C:/Users/evalue')
df=fread('training.csv')
dff=df
df=data.frame(df)
install.packages('dplyr')
library(dplyr)

require(data.table) ## 1.9.2+
setDT(df)
head(df)
df2=df %>% mutate(V42=ifelse(V42=='normal.',1,0))
df2 %>% group_by(V42) %>% summarise(total=n())
library(h2o)
cl <- h2o.init(
  max_mem_size = "3G",
  nthreads = 2)
str(df2)
df2=select(df2,-c(V2,V3,V4))
h2o.train=as.h2o(df2)
xnames=colnames(df2)
ynames=colnames(df2)[length(df2)]
results=h2o.glm(x=xnames,y=ynames,training_frame = h2o.train,family='binomial')
h2o.varimp(results)
results2=h2o.randomForest(x=xnames,y=ynames,training_frame = h2o.train)

###########
test=fread('corrected.csv')
test=test %>% mutate(V42 =ifelse(V42=='normal.',1,0))
test = select(test,-c(V2,V3,V4))
testh2o=as.h2o(test)
predicted=h2o.predict(results2,newdata=testh2o)
what=as.data.frame(predicted) %>% mutate(predict=ifelse(predict>0.5,1,0))
sum(select(test,V42)==what)/length(what[,1])
what %>% group_by(predict) %>% summarise(total=n())

####
h2o.varimp_plot(results2)
library(rpart) #classification and regression trees
library(partykit) #treeplots
tree.pros = rpart(V42~., data=test)
plot(as.party(tree.pros))

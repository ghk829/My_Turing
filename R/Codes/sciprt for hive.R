library(dplyr)
library(data.table)
fdf=fread('C:/Users/evalue/training.csv')
df=fdf %>% mutate(V42 = factor(ifelse(V42=='normal.',1,0)))
library(bnlearn)
ind=base::sample(c(0,1),size=length(df[,1]),replace=T,prob=c(0.7,0.3))
length(ind)==length(df[,1])
res=sapply(df,class)
for (i in 1:length(res)){
  if (res[i]=='integer'){
    df[,i]=as.numeric(df[,i])
  }
}
df=df[,-c(1,2,3,4)]
test=df[ind==1,]
train=df[ind==0,]
res=bnlearn::hc(test)
z=str(df)

df %>% group_by(V42) %>% summarise(means = mean(V5,na.rm=T),
                                   sds= sd(V5,na.rm=T),
                                   total = n()) 
options(scipen = 100, dplyr.width = Inf, dplyr.print_max = Inf)
fdf %>% group_by(V42) %>% summarise(mean=mean(V6))

library(h2o)
cl <- h2o.init(
  max_mem_size = "3G",
  nthreads = 2)
# we should set variables
xnames=colnames(df)
ynames=xnames[42]
xnames=xnames[-42]
xnames=xnames[-(1:3)]
h2o.train=as.h2o(df)
rm(df)
rm(fdf)
?h2o.glm
result=h2o.glm(xnames,ynames,training_frame=h2o.train,validation_frame=h2o.valid,family='binomial')
h2o.confusionMatrix(result)
####################################################
options(java.parameters = "-Xmx4g")
library(DBI)
library(rJava)
library(RJDBC)
hive.class.path = list.files(path=c("F:/lib"), pattern="jar", full.names=T);
hadoop.lib.path = list.files(path=c("F:/hadoop/share/hadoop/common"), pattern="jar", full.names=T);
hadoop.class.path = list.files(path=c("F:/hadoop/share/hadoop/tools/lib"), pattern="jar", full.names=T);
class.path = c(hive.class.path, hadoop.lib.path, hadoop.class.path); 
.jinit(classpath = class.path)
drv <- JDBC("org.apache.hive.jdbc.HiveDriver")
conn <- dbConnect(drv, "jdbc:hive2://100.100.100.21:10000/", "", "")

# setting driver 2
cp = c("C:/hive/lib/hive-jdbc-1.2.1.jar", "C:/hadoop/share/hadoop/common/hadoop-common-3.0.0-alpha1.jar", "C:/hive/lib/libthrift-0.9.2.jar", "C:/hive/lib/hive-service-1.2.1.jar", "C:/hive/lib/httpclient-4.4.jar", "C:/hive/lib/httpcore-4.4.jar", "C:/hive/lib/hive-jdbc-1.2.1-standalone.jar")
.jinit(classpath=cp)

#initialisation de la connexion
drv <- JDBC("org.apache.hive.jdbc.HiveDriver", "/usr/lib/hive/lib/hive-jdbc.jar", identifier.quote="`")





db_qry <- dbGetQuery(conn, 
                     "select * from testhivedrivertable limit 100")

db_qry # query result
######################################
library(ggplot2)
fdf=fread('C:/Users/evalue/landdata-states.csv')
ggplot(fdf,aes(x=Home_Value))+geom_histogram()
str(fdf)
fdf %>% filter(STATE %in% c("MA","AL"))%>%
  ggplot(aes(x=Date,y=Home_Value,color=STATE))+geom_point()
fdf$pred.SC <- predict(lm(Structure_Cost ~ log(Land_Value), data = fdf))

p1=fdf %>% filter(log(Land_Value)<12) %>% ggplot(aes(x=log(Land_Value),y=Structure_Cost))
p1 + geom_point(aes(color=Home_Value))+ geom_line(aes(y=pred.SC),color='red')
p1 +geom_point(aes(color=Home_Value,shape=STATE))+geom_smooth()

fdf %>% arrange(Date)%>% filter(STATE %in% c('AL','CO','MA'))%>%  ggplot(aes(x=Date,y=Home_Value,color=STATE)) + geom_point() +labs(title="diff",x='Date',y='pricing')+facet_grid(~STATE)

fdf2=fdf %>% mutate(good = ifelse(Home_Value<20000,1,ifelse(Home_Value<25000,2,ifelse(Home_Value<30000,3,4))))
fdf2 = fdf %>% mutate(group = cut(Home_Value,breaks=c(0,20000,25000,30000,max(Home_Value)),labels=c('low','middle1','middle2','high')))
fdf2 %>% ggplot(aes(x=Home_Value))+ geom_histogram(aes(fill=group))+labs(x='Price',y='Counts',title='show the counts of price')
fdf2$group=as.factor(as.numeric(cut(fdf$Home_Value,3)))
str(fdf)
fdf2=fdf %>% mutate(scaled_x  = scale2(Home_Value),scaled_y = scale2(Structure_Cost))
scale2 = function(x){
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)
}
fdf2 %>% filter(STATE %in% c('AL','MA','CO'))%>% ggplot(aes(x=scaled_x,y=scaled_y))+geom_point(aes(color=STATE))
#########

library(plyr)
library(ggplot2)
library(dplyr)
library(broom)
Orange %>% group_by(Tree) %>% do(tidy(lm(age~circumference,data=.)))
results=Orange %>% group_by(Tree)%>% do(model = lm(age~circumference,data=.))
results$model
iris %>% ggplot(aes(x=factor(1),fill=factor(Species))) +geom_bar(width=1)+coord_polar(theta="y")
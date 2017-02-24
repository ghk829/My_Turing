library(ggplot2)
library(dplyr)
library(broom)
library(datat.table)
data(Orange)
Orange %>% group_by(Tree) %>% do(tidy(lm(age~circumference,data=.)))
results=Orange %>% group_by(Tree)%>% do(model = lm(age~circumference,data=.))
head(iris)
iris %>% ggplot(aes(x=Petal.Length,y=Sepal.Width,color=Species))+ geom_point()+stat_smooth(method='lm',se=F)
x=iris %>% group_by(Species) %>% do(tidy(lm(Sepal.Width~Petal.Length,data=.)))

# piechart
Orange %>% arrange(Tree) %>% ggplot(aes(x=factor(1),fill=Tree)) +geom_bar(width=0.5)+coord_polar(theta="y")


# pie chart
data <- data%>% group_by(T)%>% mutate(pos = cumsum(T)- T/2)
at <-nrow(data) -cumsum(table(data))
result %>% ggplot(aes(x=factor(1),fill=total)) +
  geom_bar(width=0.5)+geom_text(y=result$pos,label=result$total)
data %>% ggplot(aes(x=factor(1),fill=as.factor(T)))+geom_bar(width=0.5)+coord_polar(theta='y')
+geom_text(y=data$pos,label=data$T)
data %>% ggplot(aes(x=factor(1),fill=as.factor(a))) + geom_bar(width=0.5) + coord_polar(theta='y')+labs(aes(x='count',y='??€ì¶œê¸ˆ?•¡'))
data %>% ggplot(aes(x=factor(1),fill=as.factor(T))) + geom_bar(width=0.5) +geom_text(y=data$T,label=data$T)
+coord_polar(theta='y')

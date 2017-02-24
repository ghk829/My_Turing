data=read.csv('secu_com_finance_2007.csv')
data[,7:11]<-data[,2]
data[,6]
for (i in 2:6){data[,i+5] <- scale(data[,i])}
data[,7:11]
data2 <- data[,c(1,7:11)]
data2[,5]=-data2[,5]
pairs(data2[-1])
# 상관관계 시각화
cor(data2[-1])
# pca 분석
pca <- prcomp(data2[-1])
# 상관관계 사라짐
pairs(pca$x)

# pca$sd^2 분산 / PCA
# 기여도
pca$sd^2/sum(pca$sd^2)
# 기여도 시각화
# Scree Plot
plot(pca,type='l')
biplot(pca)
text(pca$x[,1],pca$x[,2],labels=data2[,1],col='blue')
l=cov(data2[-1])
# 특이값분해
eigen(l)
plot(eigen(l)$values,type='o')
# 특이값분해의 고유값은 분산, 
# PCA의 일차결합 가장 잘 설명하는 방향 벡터
# 내적은 항상 1되는 제약조건
t(eigen(l)$vectors)%*%eigen(l)$vectors
# PCA로 만든 Reduction data
eigen(l)$vectors[,1]%*%t(data2[-1])
# 완전히 orthogonal한 데이터로 만들어버림
as.matrix(data2[-1])%*%as.matrix(pca$rotation)
cor(pca$x)
pairs(pca$x)
# scores matrix
cbind(as.character(data2[,1]),round(pca$x,2))
# 새로운 변수 명명 - 이 작업이 아마 제일 중요할거임 (데이터에 대한 이해필요)
pca$rotation
biplot(pca)

require(rgl)
plot3d(pca$x[,1],pca$x[,2],pca$x[,3])
points3d(pca$x[,1], pca$x[,2], pca$x[,3], color = 'red', size = 10)
shift <- matrix(c(-0.1, 0.1, 0), 18, 3, byrow = TRUE)
text3d(pca$x[,1:3]+shift,texts=1:18)
grid3d(c("x", "y", "z"))
biplot(pca)
grid()
abline(h=0)
abline(v=0)
# 각 변수를 나타내는 화살표의 길이는 각 변수의변동(분산) 의 
# 상대적인 크기를 나타내고 벡터들의 각은 상관관계를 나타낸다. 
# 두 벡터의 사이각이 작을수록 양의 상관관계가 크다.
# 주성분분석 실수 후
(1) 보통 다수의 변수를 차원축소하기 위해 요인분석할 때 초기 m값 잡는 방법으로 주성분분석(PCA)을 선택하여 요인분석을 실시하고, 
(2) 관측값들을 유사특성 그룹으로 묶기 위해 요인분석을 통해 나온 요인점수를 가지고, 군집분석(보통 K-means clustering)을 한 다음에, 
(3) 설명변수들을 가지고 profiling 작업을 해서 각 군집에 대한 특성을 파악하고, 각 군집에 naming을 하는 순서로 작업을 진행합니다. 


# Robust PCA - 아마 쓸 일 없을거임...
install.packages('amap')
library('amap')
rpca=acprob(data2[-1])
biplot(rpca)
biplot(pca)
rpca
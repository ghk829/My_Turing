library(network)
library(igraph)
library(intergraph)
data(flo)
g1=make_graph(edges=c(1,2,2,3,4,5),n=20)
plot(g1)
data=read.table('facebook_combined.txt')
head(data)
make_graph(edges=as.matrix(data))
as.matrix(data)
g=graph_from_data_frame(data)
plot(g)
setwd('C:/Users/kikun/Google 드라이브/공모전/R codes')
g=read.graph('dolphins.gml',format=c('gml'))
tkplot(g,vertex.size = degree(g)*1.5)
plot(g,vertex.size = betweenness(g)/15,main='Network for 소상공인')
gplot(g)
g$bet <- betweenness(g)
plot(g,vertex.size=k)
Net <- asNetwork(g)
Net
library(ergm)
Net%v%'bet' <- betweenness(g)
mo <- ergm(Net ~edges+nodematch('bet'))
summary(mo)
result=betweenness(g)
which(betweenness(g)==max(result))
str(g)
V(g)[37]$label
ec <- eigen_centrality(g)
s=which(ec$vector==max(ec$vector))
V(g)[s]$label
sum(g[15])
library(network)
network(g)
V(g)[28]$col
V(g)$col
network(E(g))
dolphinNet <-network(E(g), matrix.type='edgelist',
ignore.eval=FALSE, directed=FALSE)
plot(g,vertex.size=V(g)$bet/20)
g
V(g)$bet <-betweenness(g)
##################

data(flo)
floma=network(flo,directed=F)
floma%v%'wealth'<-c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)
plot(floma, vertex.cex=floma %v% "wealth" / 20, main="Marriage Ties")
floma
data(package="ergm")
data(florentine)
# Fit a model where the propensity to form ties between
# families depends on the absolute difference in wealth
#
library(ergm)
gest <- ergm(floma ~ edges+absdiff('wealth'))
summary(gest)
gest <- ergm(floma ~ kstar(1:2) + absdiff("wealth") + triangle)
summary(gest)
names(floma)
floma
gest$coef
pFit <- exp(gest$coef)/(1+exp(gest$coef))
pFit
l=simulate(gest)
plot(floma)
dev.new()
tkplot(l)
dd=gof(gest)
plot(dd)
str(floma)
as.matrix(floma,matrix.type="edgelist")
####################
install.packages('sna')
library(sna)
library(igraph)
x=rgraph(80,3)
make_graph(x)
igraph.plot(x)
g <- graph.tree(80, 10)
plot(g,main='Network for 소상공인')
gplot(x,main='Network for 소상공인')
karate <- read.graph("http://cneurocvs.rmki.kfki.hu/igraph/karate.net",
                     format="pajek")
karate
plot(karate)
y<-x[1,,]+4*x[2,,]+x[3,,]
nl<-netlm(y,x)
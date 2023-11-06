#A. added both age and f2 -> choose which ones to keep/describe, got a few options in A1.
setwd("~/Desktop/social networks")

library(igraphdata)
library(igraph)

drivers <- read.csv('f1_weights.csv')
#error in dataset -> delete row with albon -- vries

drivers <- drivers[-13,]

drivers_graph <- graph.data.frame(drivers, directed = FALSE)
drivers_graph_nw <- graph.data.frame(drivers, directed = FALSE)
drivers_graph <- set_edge_attr(drivers_graph, "weight", value= E(drivers_graph)$WEIGHTS)

l <- layout_with_graphopt(drivers_graph)
plot(drivers_graph, edge.width=E(drivers_graph)$weight, layout = l)

l1 <- layout_with_graphopt(drivers_graph, charge=0.023)
plot(drivers_graph, edge.width=E(drivers_graph)$weight, layout = l1, main="F1 drivers driving in the same team, 2018-2022")
#fixed

f1_wins <- read.csv('f1more.csv')
f1_wins2 <- f1_wins
f1_wins2$EVER <- ifelse(f1_wins$WINS>0, 'YES', 'NO')

names2 <- V(drivers_graph)$name
#correct order, no need to change

#this might need to be specified separately
wins <- set_vertex_attr(drivers_graph, "EVER", value = f1_wins2$EVER)
wins <- set_vertex_attr(wins, "AGE", value = f1_wins2$AGE)
wins <- set_vertex_attr(wins, "F2", value = f1_wins2$F2)

wins2 <- set_vertex_attr(drivers_graph, "EVER", value = f1_wins2$EVER)

wins3 <- set_vertex_attr(drivers_graph, "F2", value = f1_wins2$F2)

V(wins)$names <- f1_wins2$ID
V(wins)$size <- f1_wins2$WINS

V(wins2)$names <- f1_wins2$ID
V(wins2)$size <- f1_wins2$WINS

V(wins3)$names <- f1_wins2$ID
V(wins3)$size <- f1_wins2$WINS

f1_wins2$size <- ifelse(f1_wins2$WINS>30, f1_wins2$WINS/2, f1_wins2$WINS+2)

set.seed(3)
plot(wins, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins)$weight,
     edge.curved=.2, asp = 0, layout=l1, vertex.label.dist=c(0.7, 0)[as.factor(V(wins)$EVER)], vertex.label.font=2,
     vertex.color = c("#FD5E53", "#006400")[as.factor(V(wins)$EVER)], main = "Network of F1 drivers, 2018-2022",
     vertex.size = f1_wins2$size)

legend("topleft",bty = "n",legend=unique(as.factor(V(wins)$EVER)), cex = 0.7, 
       fill = c("#FD5E53", "#006400")[as.factor(V(wins)$EVER)], title='Have they won a race in this time?')


#1. 
modularity(wins2, as.factor(V(wins2)$EVER))
#0.08941486

modularity(wins3, as.factor(V(wins3)$F2))
#-0.1462853

#2. 
assortativity(wins2, f1_wins2$AGE)
#-0.09203449
#older individuals tend to associate with younger ones, yet the relationship
#is very weak

#3. Louvain?
fg <- cluster_fast_greedy(wins2)
table(membership(fg))

fl <- cluster_louvain(wins)
table(membership(fl))

#fl2 <- cutat(fl, 2)

table(membership(fl), membership(fg))

#largely overlap, choose fast greedy as I don't think I can restrict louvain to 2
#restrict to 2, as in  won a race/haven't
fg2 <- cutat(fg, 2)

#compared to have won or haven't

table(fg2, V(wins2)$EVER)

par(mfrow = c(1,2))
plot(wins2, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins2)$weight,
     edge.curved=.2, asp = 0, layout = l, vertex.label.dist=0.7, vertex.label.font=2,
     vertex.color = c("#FD5E53","#006400")[as.factor(V(wins2)$EVER)], main = "Network of F1 drivers: won/haven't won race division",
     vertex.size = 4)
plot(wins2, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins2)$weight,
     edge.curved=.2, asp = 0, layout = l, vertex.label.dist=0.7, vertex.label.font=2,
     vertex.color = c("#FD5E53", "#006400")[fg2], main = "Network of F1 drivers: fast greedy algorithm division",
     vertex.size = 4)
par(mfrow = c(1,1))

#compared to have driven in F2/haven't - yes green, no red

table(fg2, V(wins3)$F2)

par(mfrow = c(1,2))
plot(wins3, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins3)$weight,
     edge.curved=.2, asp = 0, layout = l, vertex.label.dist=0.7, vertex.label.font=2,
     vertex.color = c("#FD5E53","#006400")[as.factor(V(wins3)$F2)], main = "Network of F1 drivers: driven/haven't driven in F2",
     vertex.size = 4)
plot(wins2, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins2)$weight,
     edge.curved=.2, asp = 0, layout = l, vertex.label.dist=0.7, vertex.label.font=2,
     vertex.color = c("#FD5E53", "#006400")[fg2], main = "Network of F1 drivers: fast greedy algorithm division",
     vertex.size = 4)
par(mfrow = c(1,1))

#it might be useful to add -> have only driven in one team this whole period and do that instead of won/not won
#or just talk about how won/not won is a division on a completely different plane

#the subgroups imposed in a1 are a in a way a completely separate to the structure
#of the network category - having ever won a race. As most of these nodes are quite well
#connected drivers, rather central in the network, they don't stand out that much
#so the community detection algorithm doesnt distinguish them. Instead it only investigates
#structure of the network thus as there is a subgroup of individuals not connected
#to the main core - the Haas subgroup, it only distinguishes them

#B.
degree(wins)
diameter(wins, unconnected=FALSE)
diameter(wins, unconnected=TRUE)

degree_distribution(wins)

plot(c(0:4), degree_distribution(wins, cumulative = TRUE), 
     type = "b", main = "Cumulative degree distribution", 
     xlab="Degree", ylab ="p(degree)>x" )

par(mfrow=c(1,2))
plot(c(0:4), degree_distribution(wins, cumulative = FALSE), 
     type = "b", main ="Degree distribution", 
     xlab="Degree", ylab ="p(degree)" )
plot(c(0:4), degree_distribution(wins, cumulative = TRUE), 
     type = "b", main = "Cumulative degree distribution", 
     xlab="Degree", ylab ="p(degree)>x" )


degree_dist <- degree_distribution(wins)
# Filter out zero entries from the degree distribution
degree_dist_nonzero <- degree_dist[degree_dist > 0]

degree_dist_cumulative <- degree_distribution(wins, cumulative=TRUE)
# Filter out zero entries from the degree distribution
degree_dist_nonzero_cumulative <- degree_dist_cumulative[2:5]

par(mfrow=c(1,2))
plot(c(1:4), degree_dist_nonzero, 
     type = "b", main ="Degree distribution", 
     xlab="Degree", ylab ="p(degree)" )
plot(c(1:4), degree_dist_nonzero_cumulative, 
     type = "b", main = "Cumulative degree distribution", 
     xlab="Degree", ylab ="p(degree)>x" )


#small-world
d <- degree(wins) #extract the degrees first
a <- fit_power_law(d)     #then fit the power law
print(a)   

#small-world, some values of distribution

mean(degree(wins))
#2.294118
md <- mean(degree(wins))
average.path.length(wins)
mean_distance(wins)
#5.651442
transitivity(wins, type="global") 
#0.04225352
gorder(wins)
#34
nr <- gorder(wins)
p <- md/(nr-1) 
#probability that a given edge is included in 
#the random network
#compare to random network
random <- sample_gnp(nr, p)
mean(degree(random))
#2.705882
average.path.length(random)
mean_distance(random)
# 3.176344
transitivity(random, type="global") 
#0.1008403

#Degree pretty similar, slightly lower in my network; 
#distance much lower in the random network;
#global clustering lower in my network
#therefore, my network is less clustered than the random one -> completely doesn't
#follow small world characteristics

#C.
library(ergm)
library(intergraph)

network_f1 <- asNetwork(wins)
network_f1_extra <- asNetwork(wins3)

model1 <- ergm(network_f1 ~ edges + nodefactor('EVER') + nodematch('EVER') + absdiff('AGE') + nodefactor('F2') + nodematch('F2'))
summary(model1)
#those who have won more social
#no homophily
#does homophily vary across those who won and those who haven't - nodefactor('EVER'):nodematch('EVER')
#not working, maybe cuz has to be nodeofactor and that's only for directed
#no stat signiff differences between those who have been in f2 and those who haven't
#no homophily
#no age influence


model_gw <- ergm(network_f1~edges+  nodefactor('EVER') + nodematch('EVER') +
                   absdiff('AGE'))
#fails to converge with gwdegree, and decay parameters of 0.25 or 0.5
summary(model_gw)

#simulation
set.seed(12345)
model_gw.gof <- gof(model_gw) 

par(mfrow=c(3,2)) #plotting the results, because the gof plots will be 5, we will 
#plot all in one window with mfrow.
plot(model_gw.gof)
par(mfrow=c(1,1))

#edgewise shared partners not very well fitted so accounting for gwdsp
model_gwdsp <- ergm(network_f1~edges+  nodefactor('EVER') + nodematch('EVER') +
                   absdiff('AGE')+gwdsp(0.5,T))
#decay parameter 0.25 doesn't converge
summary(model_gwdsp)

set.seed(12345)
model_gwdsp.gof <- gof(model_gwdsp) 

par(mfrow=c(3,2)) #plotting the results, because the gof plots will be 5, we will 
#plot all in one window with mfrow.
plot(model_gwdsp.gof)
par(mfrow=c(1,1))

model_next <- ergm(network_f1~edges+  nodefactor('EVER') + nodematch('EVER') +
                      absdiff('AGE')+gwdsp(0.5,T) + gwdegree(0.5, T))
#decay parameter 0.25 doesn't converge
summary(model_next)


set.seed(12345)
model_next.gof <- gof(model_next) 

par(mfrow=c(3,2)) #plotting the results, because the gof plots will be 5, we will 
#plot all in one window with mfrow.
plot(model_next.gof)
par(mfrow=c(1,1))

setwd("~/Desktop/social networks")

library(igraphdata)
library(igraph)

drivers <- read.csv('f1_weights.csv')
#error in dataset -> delete row with albon -- vries

drivers <- drivers[-13,]

drivers_graph <- graph.data.frame(drivers, directed = FALSE)
drivers_graph <- set_edge_attr(drivers_graph, "weight", value= E(drivers_graph)$WEIGHTS)

l <- layout_with_graphopt(drivers_graph)
plot(drivers_graph, edge.width=E(drivers_graph)$weight, layout = l)

l1 <- layout_with_graphopt(drivers_graph, charge=0.023)
plot(drivers_graph, edge.width=E(drivers_graph)$weight, layout = l1, main="F1 drivers driving in the same team, 2018-2022")
#fixed

plot(drivers_graph, edge.width=E(drivers_graph)$weight, layout = l1)

#2018 - hamilton
#2019 - hamilton
#separate them into those that were getting better and those that were getting worse (getting to better teams etc)
#another separation into who was changing teams a lot and who wasn't

#node attributes in week 2
#35 drivers
#need edges in fixed distance - fixed
#add weight if appear more often - fixed

f1_wins <- read.csv('f1_wins.csv')
f1_wins2 <- f1_wins
f1_wins2$EVER <- ifelse(f1_wins$WINS>0, 'YES', 'NO')

names2 <- V(drivers_graph)$name
#correct order, no need to change

wins <- set_vertex_attr(drivers_graph, "EVER", value = f1_wins2$EVER)

V(wins)$names <- f1_wins2$ID
V(wins)$size <- f1_wins2$WINS

#specify the size differently cuz it looks weird

plot(wins)

f1_wins2$size <- ifelse(f1_wins2$WINS>30, f1_wins2$WINS/2, f1_wins2$WINS+2)

set.seed(1)
plot(wins, edge.arrow.size = 0.3,  vertex.label.cex = 0.7, edge.width=E(wins)$weight,
     edge.curved=.2, asp = 0, layout = l1, vertex.label.dist=c(0.7, 0)[as.factor(V(wins)$EVER)], vertex.label.font=2,
     vertex.color = c("#006400","#FD5E53")[as.factor(V(wins)$EVER)], main = "Network of F1 drivers, 2018-2022",
     vertex.size = f1_wins2$size)

legend("topleft",bty = "n",legend=unique(as.factor(V(wins)$EVER)), cex = 0.7, 
       fill = c("#006400","#FD5E53")[as.factor(V(wins)$EVER)], title='Have they won a race in this time?')

#density
edge_density(wins, loops = FALSE)
#0.06951872

#diameter
diameter(wins, unconnected=FALSE)
diameter(wins, unconnected=TRUE)
#12

var(degree(wins))

#analysis, weight only in alpha centrality and page rank
degree_15 <- head(sort(degree(wins), decreasing = TRUE), n=15)
degree_15

#this one might have been only for weighted ones so possibly omit
strength_15 <- head(sort(strength(wins), decreasing = TRUE), n=15)
strength_15

eigen_15 <- head(sort(eigen_centrality(wins, weights=V(wins)$WINS)$vector, decreasing = TRUE), n=15)
eigen_15

prank_15 <- head(sort(page.rank(wins, weights=V(wins)$WINS)$vector, decreasing = TRUE), n=15)
prank_15

close_15 <- head(sort(closeness(wins, weights=NA), decreasing = TRUE), n=15)
close_15

between_15 <- head(sort(betweenness(wins, weights=NA), decreasing = TRUE), n=15)
between_15

#creating tables
df_for_tables <- data.frame(names(V(wins)), degree(wins), strength(wins),
                eigen_centrality(wins, weights = V(wins)$WINS)$vector, 
                page.rank(wins, weights=V(wins)$WINS)$vector, closeness(wins, weights = NA),
                betweenness(wins, weights = NA))

names(df_for_tables) <- c("cname", "degree","w-degree", "eigen", "prank", "close", 
              "between")

standings <- c("Verstappen", "Leclerc", "Perez", "Russell", "Sainz", "Hamilton", "Norris", 
              "Ocon", "Alonso", "Bottas", "Ricciardo", "Vettel", "Magnussen", "Gasly", "Stroll")
              

topwins <- c("Hamilton", "Verstappen", "Bottas", "Vettel", "Leclerc", "Perez", "Ricciardo",
             "Gasly", "Raikkonen", "Russell", "Ocon", "Sainz")

#standings - 2022 top 15 selected in their standings order:

library(tidyverse)

df_for_tables <- as_tibble(df_for_tables)
df_for_tables_sub <- df_for_tables %>% 
  filter(cname %in% standings) %>% #select only main drivers 
  arrange(match(cname, standings)) %>% #ordering by name
  select("cname", "degree", "w-degree", "eigen", "prank", "close", "between") %>% #selecting subset of variables 
  pivot_longer(-cname, names_to = "centrality", values_to = "value") %>%  #making the data long format
  group_by(centrality) %>% 
  arrange(desc(value)) %>% 
  mutate(order = row_number()) %>% #creating order within centrality to be used in graph
  ungroup %>% 
  mutate(cname = factor(cname, levels = standings)) %>%  #arranging cname/centrality as factor for order in plot
  mutate(centrality = factor(centrality, levels = c("degree", "w-degree", "eigen", "prank", "close", "between")))

#plot the graph

library(ggplot2)

df_for_tables_sub %>% 
  ggplot(aes(cname, value)) + geom_bar(stat ="identity") +  
  facet_grid(~ centrality, scales = "free", shrink = TRUE) +
  coord_flip() + geom_label(aes(label = order), size = 3) +
  scale_x_discrete(limits = rev(levels(df_for_tables_sub$cname))) +theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), panel.spacing.x = unit(4, "mm")) 
  
#mention that interesting thing happens with closeness as Magnussen is in completely different 'tree'

#top wins - drivers with the biggest number of won races in these years (only those 
#who won smt taken into consideration)

df_for_tables <- as_tibble(df_for_tables)
df_for_tables_sub <- df_for_tables %>% 
  filter(cname %in% topwins) %>% #select only main drivers 
  arrange(match(cname, topwins)) %>% #ordering by name
  select("cname", "degree", "w-degree", "eigen", "prank", "close", "between") %>% #selecting subset of variables 
  pivot_longer(-cname, names_to = "centrality", values_to = "value") %>%  #making the data long format
  group_by(centrality) %>% 
  arrange(desc(value)) %>% 
  mutate(order = row_number()) %>% #creating order within centrality to be used in graph
  ungroup %>% 
  mutate(cname = factor(cname, levels = topwins)) %>%  #arranging cname/centrality as factor for order in plot
  mutate(centrality = factor(centrality, levels = c("degree","w-degree",  "eigen", "prank", "close", "between")))

df_for_tables_sub %>% 
  ggplot(aes(cname, value)) + geom_bar(stat ="identity") +  
  facet_grid(~ centrality, scales = "free", shrink = TRUE) +
  coord_flip() + geom_label(aes(label = order), size = 3) +
  scale_x_discrete(limits = rev(levels(df_for_tables_sub$cname))) +theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), panel.spacing.x = unit(4, "mm"))

#coreness
coreness(wins)
table(coreness(wins))
set.seed(1)

library(wesanderson)
plot(wins, vertex.size=15, vertex.color=wes_palette("Royal2")[as.factor(coreness(wins))], main='Coreness in F1 network', 
     edge.width=E(wins)$weight, edge.curved=.2,  asp=0, edge.arrow.size = 0.3)

library(blockmodeling)
f1_matrix <- as.matrix(get.adjacency(wins))

F1_class2 <- optRandomParC(M=f1_matrix, k=2, rep=50, approach='ss', blocks='com')
F1_class3 <- optRandomParC(M=f1_matrix, k=3, rep=50, approach="ss", blocks = "com") 
F1_class4 <- optRandomParC(M=f1_matrix, k=4, rep=50, approach="ss", blocks = "com") 
F1_class5 <- optRandomParC(M=f1_matrix, k=5, rep=50, approach="ss", blocks = "com") 
F1_class6 <- optRandomParC(M=f1_matrix, k=6, rep=50, approach="ss", blocks = "com") 
F1_class7 <- optRandomParC(M=f1_matrix, k=7, rep=50, approach="ss", blocks = "com") 

par(mfrow=c(1,4)) 
set.seed(12)
#two blocks:
#set.seed(1)
#plot(wins, vertex.size=15, vertex.color=F1_class2$best$best1$clu, asp=0)
#title("Two Block Partition")
#three blocks:
#set.seed(1)
#plot(wins, vertex.size=15, vertex.color=F1_class3$best$best1$clu, asp=0)
#title("Three Block Partition")
#four blocks:
set.seed(1)
plot(wins, vertex.size=15, vertex.color=F1_class4$best$best1$clu, asp=0)
title("Four Block Partition")
#five blocks:
set.seed(1)
plot(wins, vertex.size=15, vertex.color=F1_class5$best$best1$clu, asp=0)
title("Five Block Partition")
#six blocks:
plot(wins, vertex.size=15, vertex.color=F1_class6$best$best1$clu, asp=0)
title("Six Block Partition")
#seven blocks:
plot(wins, vertex.size=15, vertex.color=F1_class7$best$best1$clu, asp=0)
title("Seven Block Partition")


par(mfrow=c(1,1))

plot(wins, vertex.size= 15, edge.arrow.size = 0.3, edge.curved=.2, asp = 0, 
     edge.width=E(wins)$weight, layout=l1, vertex.color = F1_class6$best$best1$clu, 
     main = "6 blocks of F1", margin=c(0,0,0,0))

#testing the relationship between amount of won races and structural equivalence formally

table(F1_class6$best$best1$clu, f1_wins2$WINS)
chisq.test(table(F1_class6$best$best1$clu, f1_wins2$WINS))

#transitivity: 1. global
transitivity(wins, type="global") 
# 0.04225352

#transitivity: 2. local
transitivity(wins, type="local") 
V(wins)$names

#plotting correlation with betweenness for 'Mercedes clique'
#extracting only rows in which Bottas, Hamilton, Russell appear

T <- transitivity(wins, type="local")               #new vector storing local clustering coeffs
B <- betweenness(wins)   

                           #new vector storing betweenness
plot(T, B, xlab="Local clustering", ylab="Betweenness")          #plotting them, the rest below are cosmetic
#abline(lm(B ~ T))   #adding a regression (best-fitting) line on plot 
text(0.8, 100, paste("Correlation:", round(cor(T,B),2))) #adding correlation between T and B on plot
text(T, B, labels=V(wins)$name,cex=0.4, font=2, pos=1)





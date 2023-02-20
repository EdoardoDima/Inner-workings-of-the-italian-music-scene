#install.packages("netseg")
library(igraph)
library(netseg)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ITNr)
library(backbone)
library(WGCNA)
#library(dplyr)
#library(networkD3)
#library(network)

##### IDEA 07/02 ---> Compara assortativity generale con quella del network
####################  risultante dopo aver selezionato solo artisti con popularity > 50
####################  per vedere se assortativity bassa è causa del network troppo grande
####################  e con molti artisti poco popolari


##### 13/02 ---> PENSA A QUESTO: dato che l'assortativity per popolarità è maggiore
################# di quella per degree, significa che i nodi popolari non sono per forza hubs?

#####
# Loading and preprocessing

setwd('C:/Users/CreCre/Downloads/Assortativity-in-the-italian-music-market')
nodes = read.csv("nodes_2.csv")
edges = read.csv("edges_2_clean.csv")

artist_df_2 = read.csv("artist_df_2.csv")
artist_hiphop = read.csv("artist_hiphop.csv")



artist_df = rbind(artist_df_2, artist_hiphop)

missclass = c(955, 959, 964, 977, 992, 1011, 2795, 2796, 2798, 2802, 2822, 2826, 
              2841, 2865, 2883, 2954, 2999, 3118, 3272, 3297, 3409)

artist_df[missclass, 6] = 'rap'
#art2 = artist_df[!duplicated(artist_df$X1), ]
italians = (which(artist_df$X4 == 1))
dati_artisti = artist_df[italians,]
#which(dati_artisti$X2 == 'Gary Numan')
#which(dati_artisti$X2 == 'Simple Minds')
#dati_artisti = dati_artisti[-c(857, 1123, 1213)]
dati_artisti[,8] = 'Genre_agg'


for (row in 1:nrow(dati_artisti)){
  if (dati_artisti[row,]$X6 == 'rap'){
    dati_artisti[row,8] = 'Rap/Hip-Hop'
  }
  if (dati_artisti[row,]$X6 == 'hip-hop'){
    dati_artisti[row,8] = 'Rap/Hip-Hop'
  }
  if (dati_artisti[row,]$X6 == 'indie'){
    dati_artisti[row,8] = 'Indie'
  }
  if (dati_artisti[row,]$X6 == 'indie-pop'){
    dati_artisti[row,8] = 'Indie'
  }
  if (dati_artisti[row,]$X6 == 'pop'){
    dati_artisti[row,8] = 'Pop'
  }
  if (dati_artisti[row,]$X6 == 'emo'){
    dati_artisti[row,8] = 'Emo/Punk'
  }
  if (dati_artisti[row,]$X6 == 'punk'){
    dati_artisti[row,8] = 'Emo/Punk'
  }
  if (dati_artisti[row,]$X6 == 'punk-rock'){
    dati_artisti[row,8] = 'Emo/Punk'
  }
}

#duplicati = dati_artisti[which(duplicated(dati_artisti$X1)),]
dati_artisti_final = dati_artisti[!duplicated(dati_artisti$X1), -c(4,6)]
colnames(dati_artisti_final) = c('id', 'name', 'followers', 'popularity', 'genre')

#edges_unique = edges[!duplicated(edges), ]
#which(edges$X1 == 'Gary Numan')
#which(edges$X1 == 'Simple Minds')
#edges = edges[-c(2515, 2516),]

edges_unique2 <- edges[!duplicated(apply(edges[,1:2], 1, function(row) paste(sort(row), collapse=""))),]

artisti_genre = as.vector(dati_artisti_final[,6])
artisti_popol = as.vector(dati_artisti_final[,4])
artisti_foll = as.vector(dati_artisti_final[,3])

#####
# Building network
g <- graph_from_data_frame(edges_unique2, directed=FALSE, vertices=nodes
                           )


g = set_vertex_attr(g, 'Genre', index = V(g), artisti_genre)
g = set_vertex_attr(g, 'Popularity', index = V(g), artisti_popol)
g = set_vertex_attr(g, 'Followers', index = V(g), artisti_foll)

V(g)[vertex_attr(g,'Genre') == 'Rap/Hip-Hop']$color = 'violetred'#'deeppink4'
V(g)[vertex_attr(g,'Genre') == 'Indie']$color = 'darkturquoise'#'turquoise4'
V(g)[vertex_attr(g,'Genre') == 'Pop']$color = 'darkorange1'#'azure4'
V(g)[vertex_attr(g,'Genre') == 'Emo/Punk']$color = 'slategray4'#'olivedrab'

#V(g)$degree <- degree(g)
#zero_degree = which(degree(g)==0)

#g = g - zero_degree


which(V(g)$name == 'Simple Minds') # 563
which(V(g)$name == 'Gary Numan') # 766
which(V(g)$name == 'Killa Fonic') #114
which(V(g)$name == 'NANE') #113
neighbors(g, 97)
g = delete_vertices(g, c(113, 114, 563, 766))

gsize(g)
V(g)$degree <- degree(g)
zero_degree = which(degree(g)==0)
g = g - zero_degree
V(g)$degree[1]

# Plotting:

plot(g, vertex.size=5, edge.curverd=.1, arrow.size=.1, vertex.color = "orchid", 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, 
     vertex.label = NA)#ifelse(degree(g) >= 34, V(g)$name, NA))



degree_g = sort(igraph::degree(g, v = V(g), mode = "all"), decreasing = TRUE)
length(which(degree_g== 0)) 
mean(degree_g)

hist(degree_g[degree_g > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

plot_degree_distribution(g, a = 'all')

x = degree_g

plot(log(degree_g[degree_g > 0]))

occur = as.vector(table(x))
occur = occur/sum(occur)
p = occur/sum(occur)
y = rev(cumsum(rev(p)))
x = as.numeric(names(table(x)))
plot(x, y, log="xy", type="l")

scaleFreePlot(degree_g,main = 'Scale-free fit ')


backbone.suggest(g)
?sparsify.with.lspar
g_backbone = sparsify.with.localdegree(g, s = 0.1)
plot(g_backbone, vertex.size=ifelse(degree(g) >= 5, V(g)$degree/60*10, V(g)$degree+2), 
     edge.curverd=.1, arrow.size=.1, 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, 
     vertex.label = NA)#ifelse(degree(g) >= 60, V(g)$name, NA))


length(which(V(g)$Genre == 'Emo/Punk'))

genres_name = c('Rap', 'Pop', 'Indie', 'Alt')
genres_count = c(291, 170, 165, 64)
sum(genres_count)

barplot(genres_count, names.arg = genres_name, main = "Distribution of genres",
        xlab = "Genre",
        ylab = "# of artists")


# Hubs:

quant = quantile(degree_g[degree_g>0],0.95)
print(quant)
hubs <- degree_g[degree_g>=quant]
hubs2 = V(g)[which(V(g)$degree >=quant)]
length(hubs)

mean(V(g)$Popularity[hubs2])
mean(V(g)$Popularity)

g_hubs = subgraph(g, hubs)
mean(V(g_hubs)$Popularity)

# ARE POPULAR ARTISTS HUBS?

names(degree_g[1:200])
df2 <- dati_artisti_final[order(dati_artisti_final$popularity,decreasing=TRUE),]

df2[1:36,]$name

intersect(names(degree_g[1:70]), df2[1:70,]$name)

V(g)[which(V(g)$Name == "Måneskin")]

neighbors(g, v = , mode = c("out", "in", "all", "total"))


popularity_g = sort(V(g)$Popularity, decreasing = TRUE)
quant_popularity = quantile(popularity_g[popularity_g>0],0.95)
print(quant_popularity)

followers_g = sort(V(g)$Followers, decreasing = TRUE)
quant_foll = quantile(followers_g[followers_g>0],0.95)
print(quant_foll)

length(which(hubs2$Followers >= quant_foll))
hubs2
which(hubs2$Genre != 'Rap/Hip-Hop')
hubs2[19]

which(hubs2$Popularity >= 66)
hubs_popularity <- V(g)[which(V(g)$Popularity >=quant_popularity)]
mean(hubs_popularity$Popularity)
hubs_popularity$name
names(hubs)

intersect(hubs2$name, hubs_popularity$name)

# Clustering coefficient

igraph::transitivity(
  g,
  type = 'undirected',
  vids = NULL,
  weights = NULL,
  isolates = c("NaN", "zero")
)

# Closeness centrality

clos_g = igraph::closeness(
  g,
  vids = igraph::V(g),
  mode = 'total',
  weights = NULL,
  normalized = TRUE,
  cutoff = -1
)

quant_clos = quantile(clos_g[clos_g>0],0.95)
print(quant_clos)
hubs_clos <- clos_g[clos_g>=quant_clos]
hubs_clos

intersect(names(hubs),names(hubs_clos))

# Betweenness centrality

bet_g = igraph::betweenness(
  g,
  v = igraph::V(g),
  directed = FALSE,
  weights = NULL,
  #nobigint = TRUE,
  normalized = FALSE,
  cutoff = -1
)

quant_bet = quantile(bet_g[bet_g>0],0.95)
print(quant_bet)
hubs_bet <- bet_g[bet_g>=quant_bet]
hubs_bet

intersect(names(hubs),names(hubs_bet))

# Segregation:

m = mixingm(g, "Genre", full = FALSE)
m

# Which pop artist and emo artist are collaborating?

long_cat = igraph::as_long_data_frame(g)
long_cat_inter = long_cat[long_cat[,4] != long_cat[,10],]
long_cat_edges = long_cat_inter[,c(3,9)]

long_cat_inter_pop = long_cat_inter %>%
  filter(from_Genre=='Pop', to_Genre=='Emo/Punk')
long_cat_inter_pop_edges = long_cat_inter_pop[,c(3,10)]
g_inter_pop = igraph::graph_from_data_frame(long_cat_inter_pop_edges, directed=FALSE)

V(g_inter_pop)

V(g_inter_pop)[c(1:7)]$color = 'orange'
V(g_inter_pop)[c(8:13)]$color = 'grey'

plot(g_inter_pop, vertex.label = V(g_inter_pop)$name, layout = layout.fruchterman.reingold )
gsize(g_inter_pop)


gsize(g)
1797/gsize(g)*100

(1797+363+327+58)/gsize(g)*100


# E-I and SSI

ei(g, "Genre")
orwg(g, "Genre")
assort(g, 'Genre')
freeman(g, 'Genre')

#install.packages("cli")
#library(devtools)
#install_github("mbojan/isnar")
#library(isnar)
#isnar::ei(g, "Genre")
                            # Cross checked with another package
                            # To see if results were the same

v_10 = V(g)[which(V(g)$degree >= 10)]
count_components(g)
V(g)[242]


long_ssi = igraph::as_long_data_frame(g)
long_ssi = long_ssi[,c(8,9)]
long_ssi = long_ssi[order(long_ssi[,1]),]
plot(log(long_ssi[,1]), log(long_ssi[,2]))


uela = sort(specsi, decreasing = TRUE)
uela[1]
specsi = (v <- ssi(g, "Genre"))
mean(specsi)
max(specsi)
hist(specsi, breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

g = set_vertex_attr(g, 'ssi', index = V(g), specsi)


specsi_rap = V(g)[which(V(g)$Genre == 'Rap/Hip-Hop')]$ssi
popularity_rap = V(g)[which(V(g)$Genre == 'Rap/Hip-Hop')]$Popularity
mean(specsi_rap)
mean(popularity_rap)

specsi_pop = V(g)[which(V(g)$Genre == 'Pop')]$ssi
popularity_pop = V(g)[which(V(g)$Genre == 'Pop')]$Popularity
mean(specsi_pop)
mean(popularity_pop)

specsi_indie = V(g)[which(V(g)$Genre == 'Indie')]$ssi
popularity_indie = V(g)[which(V(g)$Genre == 'Indie')]$Popularity
mean(specsi_indie)
mean(popularity_indie)

specsi_emo = V(g)[which(V(g)$Genre == 'Emo/Punk')]$ssi
popularity_emo = V(g)[which(V(g)$Genre == 'Emo/Punk')]$Popularity
mean(specsi_emo)
mean(popularity_emo)


rap_nodes = V(g)[which(V(g)$Genre == 'Rap/Hip-Hop')]
pop_nodes = V(g)[which(V(g)$Genre == 'Pop')]
indie_nodes = V(g)[which(V(g)$Genre == 'Indie')]
emo_nodes = V(g)[which(V(g)$Genre == 'Emo/Punk')]

rap_indie = induced_subgraph(g, vids = c(rap_nodes, indie_nodes))
specsi_rap_indie = (v <- ssi(rap_indie, "Genre"))
mean(specsi_rap_indie)

genres = c('Rap/Hip-Hop', 'Pop', 'Indie', 'Emo/Punk')

specsi_genres = c()

for (i in genres){
  #print(i)
  for (j in genres){
    if (i != j){
    nodes1 = V(g)[which(V(g)$Genre == i)]
    nodes2 = V(g)[which(V(g)$Genre == j)]
    net = induced_subgraph(g, vids = c(nodes1, nodes2))
    spsi = (v <- ssi(net, "Genre"))
    spsi = mean(spsi)
    specsi_genres[length(specsi_genres)+1]=spsi
    }
  }
}

specsi_genres

## Doesn't seem to work properly (NOOO IT ACTUALLY WORKS!!!!)
ei_genres = c()

for (i in genres){
  #print(i)
  for (j in genres){
    if (i != j){
      nodes1 = V(g)[which(V(g)$Genre == i)]
      nodes2 = V(g)[which(V(g)$Genre == j)]
      net = induced_subgraph(g, vids = c(nodes1, nodes2))
      ei_gen = ei(net, "Genre")
      #ei_gen = mean(ei_gen)
      print(c(i, j, ei_gen))
      ei_genres[length(ei_genres)+1]=ei_gen
    }
  }
}

ei_genres


freeman_genres = c()

for (i in genres){
  #print(i)
  for (j in genres){
    if (i != j){
      nodes1 = V(g)[which(V(g)$Genre == i)]
      nodes2 = V(g)[which(V(g)$Genre == j)]
      net = induced_subgraph(g, vids = c(nodes1, nodes2))
      freeman_gen = freeman(net, "Genre")
      #ei_gen = mean(ei_gen)
      print(c(i, j, freeman_gen))
      freeman_genres[length(freeman_genres)+1]=freeman_gen
    }
  }
}

freeman_genres
genres

specsi_sort = sort(specsi, decreasing = TRUE)
specsi_sort
 
index_specsi = as.numeric(names(specsi_sort[1:75]))
cane1 = cbind(V(g)[index_specsi]$name, specsi_sort)
print(cane1)
fifty_bet = sort(bet_g, decreasing = TRUE)[1:75]
cane2 = names(fifty_bet)
cane1
cane2
intersect(cane1,cane2)

which(V(g)$name == 'Tre Allegri Ragazzi Morti')
E(g) [ .from(192) ]
#V(g)$Popularity
#igraph::any_multiple(g)
#length(which(which_multiple(g, eids = E(g)))==TRUE)

popular_ssi = which(V(g)[index_specsi]$Popularity > 70)
V(g)[popular_ssi]

rap_nodes = V(g)[which(V(g)$Genre == 'Rap/Hip-Hop')]


uis = make_ego_graph(
  g,
  order = 1,
  nodes = V(g)[5],
  mode = "all",
  mindist = 0
)

V(uis)



# Assortativity:
        
igraph::assortativity_degree(g, directed = FALSE)

igraph::assortativity(g, igraph::V(g)$Popularity, types2 = NULL, directed = FALSE)
#get.vertex.attribute(g,'Popularity')
igraph::assortativity(g, igraph::V(g)$Followers, types2 = NULL, directed = FALSE)
knn_g = knn(
  g,
  vids = V(g),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


knn_g$knn

degree_unsorted = as.data.frame(degree(g, v = V(g)))
degree_unsorted_un = sort(unique(degree_unsorted[,1]))
knnk = knn_g$knnk
knnk = knnk[!is.na(knnk)]
avg_degree_squared = degree_unsorted^2
avg_degree_squared = mean(avg_degree_squared[,1])
neutral = avg_degree_squared / mean(degree_unsorted[,1])
knnk = append(knnk, neutral)
#knnk = (knnk-min(knnk))/(max(knnk)-min(knnk))
#neutral_norm = knnk[57] #〈k2〉 / 〈k〉
knnk = knnk[1:56]
plot1 = as.data.frame(cbind(unique(degree_unsorted_un), knnk))
colnames(plot1) = c('k', 'knnk')
plot(log(plot1$k), log(plot1$knnk))
abline(h = log(neutral))


ggplot(plot1, aes(x = k, y = knnk)) + 
  geom_point(color = "firebrick", shape = "diamond", size = 2) + 
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "k", y = "knn(k)") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic")) + 
  ggtitle("Temperatures in Chicago") + 
  geom_hline(yintercept=neutral)

log_plot1 = data.frame(k=log(plot1$k),
                       knnk=log(plot1$knnk))

#Therefore knn(k) is the average degree of the neighbors of all degree-k nodes.
#To quantify degree correlations we inspect the dependence of knn(k) on k. 

p1 = ggplot(log_plot1, aes(x = k, y = knnk)) + 
  geom_point(color = "firebrick", shape = "diamond", size = 2) + 
  #geom_smooth(method = "lm", se = FALSE) +
  
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic")) + 
  ggtitle("Degree correlation") + 
  #geom_hline(yintercept=log(neutral)) +
  geom_hline(aes(yintercept=log(neutral)), colour = 'black') + 
  labs(x = "k", y = "knn(k)")

p1

summary(lm(plot1$knnk ~ plot1$k))$coefficients
sqrt(mean(degree_g)*690)

ggplot() +
  geom_point(aes(k, knnk), color = 'Variable', log_plot1,
             alpha = 1, shape = "diamond", size = 2) +
  geom_hline(aes(yintercept=log(neutral), color = "black"),
             linetype = 1) +
 
  theme(legend.position = "bottom") +
  scale_colour_manual(name = "random",
                      values = "black") +
  scale_linetype_manual(name = "",
                        values = "dashed")



ggplot(data = log_plot1) +
  geom_point(aes(x = k,
                y = knnk,
                colour = "Variable",
                ), 
             size = 2, shape = "diamond") +
  geom_hline(aes(yintercept = log(neutral),
                 linetype = "Legend")) +

  theme(legend.position = 'right') +
  scale_colour_manual(name = "",
                      values = "firebrick") +
  scale_linetype_manual(name = "",
                        values = 1)+
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic")) +
  labs(x = "k", y = "knn(k)")






g_rs = rewire(g, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_rs, directed = FALSE)
g_rm = rewire(g, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm, directed = FALSE)
igraph::assortativity(g_rm, igraph::V(g_rm)$Popularity, types2 = NULL, directed = FALSE)
igraph::assortativity(g_rs, igraph::V(g_rs)$Popularity, types2 = NULL, directed = FALSE)



knn_rs = knn(
  g_rs,
  vids = V(g_rs),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)

knnk_rs = knn_rs$knnk
knnk_rs = knnk_rs[!is.na(knnk_rs)]
log_plot2 = data.frame(k=log(plot1$k),
                       knnk=log(knnk_rs))


p2 = p1 + 
  geom_point(data = log_plot2, shape = 'diamond', size = 2, color = 'orange',
             mapping = aes(x=k, y= knnk)) +
  geom_smooth(method = "lm", se = FALSE)
p2
p1
summary(lm(log_plot2$knnk ~ log_plot2$k))$coefficients

# knn(k) but for popularity: is it possible? Let's find out:

#V(g)$name <- V(g) 
V(g)$name
# add degree to the graph
#V(g)$degree <- degree(g, loops = TRUE, normalized = FALSE)
#V(g)$degree
# get a list of neighbours, for each node
g_ngh <- neighborhood(g, mindist = 1) 

# write a function that gets the means                       
get.mean <- function(x){
  mean(V(g)$Popularity[x]-1)
}

# apply the function, add result to the graph
V(g)$av_pop_nei <- sapply(g_ngh, get.mean)


# get data into dataframe, if necessary
d_vert_attr <- igraph::as_data_frame(g, what = "vertices")

popularities = sort(unique(V(g)$Popularity))
which(d_vert_attr$Popularity == 7)

pnn = data.frame(p = popularities, pnnp = 0)

count_pnnp = 1
for (i in popularities){
  popol1 = d_vert_attr[which(d_vert_attr$Popularity == i), ]
  print(nrow(popol1))
  
  pnnp_loop = mean(popol1$av_pop_nei)#ifelse(nrow(popol1) > 1, rowMeans(popol1[ ,7]), popol1[,7])
  pnn[count_pnnp,2] = pnnp_loop
  count_pnnp = count_pnnp + 1
}

log_plot_popol = data.frame(p=log(pnn$p),
                       pnnp=log(pnn$pnnp))

ggplot(log_plot_popol, aes(x = p, y = pnnp)) + 
  geom_point(color = "firebrick", shape = "diamond", size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "k", y = "knn(k)") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic")) + 
  ggtitle("Temperatures in Chicago")


### USING THE POPULAR ARTISTS:

populares = dati_artisti_final[which(dati_artisti_final$popularity > 50),]


los_populares = g

igraph::assortativity_degree(los_populares, directed = FALSE)


### USING MULTI LINKS:

g2 <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes
)
gsize(g2)
gsize(g)
g2 = set_vertex_attr(g, 'Genre', index = V(g), artisti_genre)
g2 = set_vertex_attr(g, 'Popularity', index = V(g), artisti_popol)
g2 = set_vertex_attr(g, 'Followers', index = V(g), artisti_foll)

V(g)[vertex_attr(g,'Genre') == 'Rap/Hip-Hop']$color = 'deeppink4'
V(g)[vertex_attr(g,'Genre') == 'Indie']$color = 'turquoise4'
V(g)[vertex_attr(g,'Genre') == 'Pop']$color = 'azure4'
V(g)[vertex_attr(g,'Genre') == 'Emo/Punk']$color = 'olivedrab'

V(g2)$degree <- degree(g2)
zero_degree2 = which(degree(g2)==0)

g2 = g2 - zero_degree2

assortativity.degree(g2, directed = FALSE)
assortativity.degree(g, directed = FALSE)


# Clustering :


clusterlouvain <- cluster_louvain(g)
plot(g, vertex.color=rainbow(3, alpha=0.6)[clusterlouvain$membership],
     edge.curverd=.1, arrow.size=.1, 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, 
     vertex.label = NA)

clusterlouvain$memberships

a = clusterlouvain$membership
cane1 = cbind(V(g)[index_specsi]$name, specsi_sort)

V(g)$community <- membership(clusterlouvain)

length(V(g)[which(V(g)$community == 1)])


for (i in seq(17)){
  community_nodes = V(g)[which(V(g)$community == i)]
  community = induced.subgraph(g, community_nodes)
  rap_com = (V(community)[which(V(community)$Genre == 'Rap/Hip-Hop')])
  print(rap_com)
}
community_nodes = V(g)[which(V(g)$community == 1)]
community = induced.subgraph(g, community_nodes)
community_nodes
rap_com = V(community)[which(V(community)$Genre == 'Rap/Hip-Hop')]
print(rap_com)

net = induced_subgraph(g, vids = c(nodes1, nodes2))
spsi = (v <- ssi(net, "Genre"))
spsi = mean(spsi)
specsi_genres[length(specsi_genres)+1]=spsi


#Degree correlation matrix:

dg <- igraph::degree(g)
g.dg <- igraph::graph_from_data_frame(
  with(
    igraph::get.data.frame(g),
    data.frame(dg[from], dg[to])
  ),
  directed = FALSE
)
mat <- igraph::get.adjacency(g.dg, sparse = FALSE)
ord <- order(as.numeric(row.names(mat)))
out <- mat[ord, ord]
out

#Popularity correlation matrix:

pg = setNames(as.character(dati_artisti_final[,4]), dati_artisti_final[,2])
g.pg <- graph_from_data_frame(
  with(
    get.data.frame(g),
    data.frame(pg[from], pg[to])
  ),
  directed = FALSE
)
mat_popol <- get.adjacency(g.pg, sparse = FALSE)
ord_popol <- order(as.numeric(row.names(mat_popol)))
out_popol <- mat_popol[ord_popol, ord_popol]
out_popol

max(out_popol)


# Ok so basically this code here was to plot the matrices

data = melt(out)
'seq_deg = seq(1:max(data$Var1))
missing_deg = setdiff(seq_deg, data$Var1[1:56])
missing_deg_mat = matrix()
out'
library(ggplot2)
ggplot(data = data) +
  theme_bw() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_viridis_b(name = "") +
  labs(x = "k2", y = "k1")


   ### Largest connected component 
components_g <- igraph::clusters(g, mode="weak")
biggest_cluster_id_g <- which.max(components_g$csize)
# ids
vert_ids_g <- V(g)[components_g$membership == biggest_cluster_id_g]
# subgraph
g_v2 = igraph::induced_subgraph(g, vert_ids_g)
plot(g_v2, vertex.size=5, edge.curverd=.1, arrow.size=.1, vertex.color = "orchid", 
     main = "Ueeeueeee",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, vertex.label = NA)


connected_comp = V(g_v2)$name

artisti_cc = (dati_artisti_final[which(dati_artisti_final[,2] %in% connected_comp),])
artisti_cc_genre = as.vector(artisti_cc[,6])
artisti_cc_popol = as.vector(artisti_cc[,4])
artisti_cc_foll = as.vector(artisti_cc[,3])

g_v2 = set_vertex_attr(g_v2, 'Genre', index = V(g_v2), artisti_cc_genre)
g_v2 = set_vertex_attr(g_v2, 'Popularity', index = V(g_v2), artisti_cc_popol)
g_v2 = set_vertex_attr(g_v2, 'Followers', index = V(g_v2), artisti_cc_foll)
get.vertex.attribute(g_v2,'Popularity')
genere = get.vertex.attribute(g_v2,'Genre')
genere = 
plot(g_v2, vertex.size=vertex_attr(g_v2,'Popularity'), edge.curverd=.1, arrow.size=.1, #vertex.color = , 
     main = "Ueeeueeee",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, vertex.label = NA)


V(g_v2)[vertex_attr(g_v2,'Genre') == 'Rap/Hip-Hop']$color = 'deeppink4'
V(g_v2)[vertex_attr(g_v2,'Genre') == 'Indie']$color = 'turquoise4'
V(g_v2)[vertex_attr(g_v2,'Genre') == 'Pop']$color = 'azure4'
V(g_v2)[vertex_attr(g_v2,'Genre') == 'Emo/Punk']$color = 'olivedrab'


layout <- layout_with_lgl(
  g_v2,
  maxiter = 150,
  maxdelta = vcount(g_v2),
  area = 5*vcount(g_v2)^2,
  coolexp = 1.5,
  #repulserad = area * vcount(g_v2),
  #cellsize = sqrt(sqrt(area)),
  root = NULL
)

plot(g_v2, vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)


knn_g_v2 = knn(
  g_v2,
  vids = V(g_v2),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


degree_unsorted = as.data.frame(degree(g_v2, v = V(g_v2)))
plot1_g_v2 = as.data.frame(cbind(degree_unsorted, knn_g_v2[[1]]))


colnames(plot1_g_v2) = c('k', 'knn')
plot(plot1_g_v2$k, plot1_g_v2$knn)


assortativity(g_v2, get.vertex.attribute(g_v2,'Popularity'), types2 = NULL, directed = FALSE)

netseg::plot(
  g_v2,
  #layout = graphlayouts::layout_with_stress,
  #vertex.color = c("pink", "lightskyblue")[match(V(Classroom)$gender, c("Girl", "Boy"))],
  vertex.label = NA,
  edge.arrow.size = 0.5
)

orwg(g_v2, "Genre")

coleman(g_v2, "Genre")




    ### Rap network:

g_rap = igraph::delete_vertices(g, igraph::V(g)[!igraph::vertex_attr(g, "Genre") == 'Rap/Hip-Hop'])
zero_degree_rap = which(igraph::degree(g_rap)==0)

g_rap = g_rap - zero_degree_rap
gsize(g_rap)
#specsi_grap = (v <- ssi(g_rap, "Genre"))
#mean(specsi_grap)
#max(specsi_grap)
#specsi_grap
plot(g_rap,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = igraph::layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_rap = sort(igraph::degree(g_rap, v = igraph::V(g_rap)), decreasing = TRUE)
length(which(degree_rap== 0))
mean(degree_rap)

hist(degree_rap[degree_rap > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

igraph::assortativity_degree(g_rap, directed = FALSE)


quant_rap = quantile(degree_rap[degree_rap>0],0.95)
hubs_rap <- degree_rap[degree_rap>=quant_rap]

hubs_rap

knn_rap = knn(
  g_rap,
  vids = V(g_rap),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)

degree_unsorted_rap = as.data.frame(degree(g_rap, v = V(g_rap)))
degree_unsorted_un_rap = sort(unique(degree_unsorted_rap[,1]))
knnk_rap = knn_rap$knnk
knnk_rap = knnk_rap[!is.na(knnk_rap)]
avg_degree_squared_rap = degree_unsorted_rap^2
avg_degree_squared_rap = mean(avg_degree_squared_rap[,1])
neutral_rap = avg_degree_squared_rap / mean(degree_unsorted_rap[,1])
knnk_rap = append(knnk_rap, neutral_rap)
knnk_rap = (knnk_rap-min(knnk_rap))/(max(knnk_rap)-min(knnk_rap))
neutral_norm_rap = knnk_rap[length(knnk_rap)] #〈k2〉 / 〈k〉
knnk_rap = knnk[1:length(knnk_rap)-1]
plot1_rap = as.data.frame(cbind(unique(degree_unsorted_un_rap), knnk_rap))
colnames(plot1_rap) = c('k', 'knnk')
plot(plot1_rap$k, plot1_rap$knnk)
abline(h = neutral_norm_rap)


ggplot(plot1, aes(x = k, y = knnk)) + 
  geom_point(color = "firebrick", shape = "diamond", size = 2) + 
  labs(x = "k", y = "knn(k)") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic")) + 
  ggtitle("Temperatures in Chicago") + 
  geom_hline(yintercept=neutral_norm)


igraph::assortativity(g_rap, igraph::V(g_rap)$Popularity, directed = FALSE)

g_shuffle_rap = rewire(g_rap, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_rap, directed = FALSE)

g_rm_rap = rewire(g_rap, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_rap, directed = FALSE)

      ### Pop network:


g_pop = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Pop'])
zero_degree_pop = which(degree(g_pop)==0)
gsize(g_pop)
g_pop = g_pop - zero_degree_pop

plot(g_pop,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_pop = sort(igraph::degree(g_pop, v = V(g_pop)), decreasing = TRUE)
length(which(degree_pop== 0))
mean(degree_pop)

hist(degree_pop[degree_pop > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

assortativity_degree(g_pop, directed = FALSE)


quant_pop = quantile(degree_pop[degree_pop>0],0.95)
hubs_pop <- degree_pop[degree_pop>=quant_pop]

hubs_pop

knn_pop = knn(
  g_pop,
  vids = V(g_pop),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


degree_unsorted_pop = as.data.frame(degree(g_pop, v = V(g_pop)))
degree_unsorted_un_pop = sort(unique(degree_unsorted_pop[,1]))
knnk_pop = knn_pop$knnk
knnk_pop = knnk_pop[!is.na(knnk_pop)]
avg_degree_squared_pop = degree_unsorted_pop^2
avg_degree_squared_pop = mean(avg_degree_squared_pop[,1])
neutral_pop = avg_degree_squared_pop / mean(degree_unsorted_pop[,1])
knnk_pop = append(knnk_pop, neutral_pop)
knnk_pop = (knnk_pop-min(knnk_pop))/(max(knnk_pop)-min(knnk_pop))
neutral_norm_pop = knnk_pop[length(knnk_pop)] #〈k2〉 / 〈k〉
knnk_pop = knnk[1:length(knnk_pop)-1]
plot1_pop = as.data.frame(cbind(unique(degree_unsorted_un_pop), knnk_pop))
colnames(plot1_pop) = c('k', 'knnk')
plot(plot1_pop$k, plot1_pop$knnk)
abline(h = neutral_norm_pop)

assortativity(g_pop, get.vertex.attribute(g_pop,'Popularity'), types2 = NULL, directed = FALSE)
igraph::assortativity(g_pop, igraph::V(g_pop)$Popularity, directed = FALSE)

g_shuffle_pop = rewire(g_pop, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_pop, directed = FALSE)

g_rm_pop = rewire(g_pop, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_pop, directed = FALSE)

      ### Indie:

g_indie = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Indie'])
zero_degree_indie = which(degree(g_indie)==0)

g_indie = g_indie - zero_degree_indie

plot(g_indie,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_indie = sort(igraph::degree(g_indie, v = V(g_indie)), decreasing = TRUE)
length(which(degree_indie== 0))
mean(degree_indie)

hist(degree_indie[degree_indie > 0], breaks = 10, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

assortativity_degree(g_indie, directed = FALSE)


quant_indie = quantile(degree_indie[degree_indie>0],0.95)
hubs_indie <- degree_indie[degree_indie>=quant_indie]

hubs_indie

knn_indie = knn(
  g_indie,
  vids = V(g_indie),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


degree_unsorted_indie = as.data.frame(degree(g_indie, v = V(g_indie)))
degree_unsorted_un_indie = sort(unique(degree_unsorted_indie[,1]))
knnk_indie = knn_indie$knnk
knnk_indie = knnk_indie[!is.na(knnk_indie)]
avg_degree_squared_indie = degree_unsorted_indie^2
avg_degree_squared_indie = mean(avg_degree_squared_indie[,1])
neutral_indie = avg_degree_squared_indie / mean(degree_unsorted_indie[,1])
knnk_indie = append(knnk_indie, neutral_indie)
knnk_indie = (knnk_indie-min(knnk_indie))/(max(knnk_indie)-min(knnk_indie))
neutral_norm_indie = knnk_indie[length(knnk_indie)] #〈k2〉 / 〈k〉
knnk_indie = knnk[1:length(knnk_indie)-1]
plot1_indie = as.data.frame(cbind(unique(degree_unsorted_un_indie), knnk_indie))
colnames(plot1_indie) = c('k', 'knnk')
plot(plot1_indie$k, plot1_indie$knnk)
abline(h = neutral_norm_indie)

assortativity(g_indie, get.vertex.attribute(g_indie,'Popularity'), types2 = NULL, directed = FALSE)
igraph::assortativity(g_indie, igraph::V(g_indie)$Popularity, directed = FALSE)

g_shuffle_indie = rewire(g_indie, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_indie, directed = FALSE)

g_rm_indie = rewire(g_indie, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_indie, directed = FALSE)

most_popular_indie = sort(V(g_indie)$Popularity, decreasing = T)
which(V(g_indie)$Popularity == 74)



      ###Emo:

g_emo = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Emo/Punk'])
zero_degree_emo = which(degree(g_emo)==0)

g_emo = g_emo - zero_degree_emo

plot(g_emo,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_emo = sort(igraph::degree(g_emo, v = V(g_emo)), decreasing = TRUE)
length(which(degree_emo== 0))
mean(degree_emo)

hist(degree_emo[degree_emo > 0], breaks = 10, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

assortativity_degree(g_emo, directed = FALSE)


quant_emo = quantile(degree_emo[degree_emo>0],0.95)
hubs_emo <- degree_emo[degree_emo>=quant_emo]

hubs_emo

knn_emo = knn(
  g_emo,
  vids = V(g_emo),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


degree_unsorted_emo = as.data.frame(degree(g_emo, v = V(g_emo)))
degree_unsorted_un_emo = sort(unique(degree_unsorted_emo[,1]))
knnk_emo = knn_emo$knnk
knnk_emo = knnk_emo[!is.na(knnk_emo)]
avg_degree_squared_emo = degree_unsorted_emo^2
avg_degree_squared_emo = mean(avg_degree_squared_emo[,1])
neutral_emo = avg_degree_squared_emo / mean(degree_unsorted_emo[,1])
knnk_emo = append(knnk_emo, neutral_emo)
knnk_emo = (knnk_emo-min(knnk_emo))/(max(knnk_emo)-min(knnk_emo))
neutral_norm_emo = knnk_emo[length(knnk_emo)] #〈k2〉 / 〈k〉
knnk_emo = knnk[1:length(knnk_emo)-1]
plot1_emo = as.data.frame(cbind(unique(degree_unsorted_un_emo), knnk_emo))
colnames(plot1_emo) = c('k', 'knnk')
plot(plot1_emo$k, plot1_emo$knnk)
abline(h = neutral_norm_emo)

assortativity(g_emo, get.vertex.attribute(g_emo,'Popularity'), types2 = NULL, directed = FALSE)
igraph::assortativity(g_emo, igraph::V(g_emo)$Popularity, directed = FALSE)

g_shuffle_emo = rewire(g_emo, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_emo, directed = FALSE)

g_rm_emo = rewire(g_emo, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_emo, directed = FALSE)
  
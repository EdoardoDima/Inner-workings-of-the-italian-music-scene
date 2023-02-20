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


#####
# Loading and preprocessing

setwd('C:/Users/CreCre/Downloads/Assortativity-in-the-italian-music-market')
nodes = read.csv("nodes_2.csv")
edges = read.csv("edges_2_clean.csv")

artist_df_2 = read.csv("artist_df_2.csv")
artist_hiphop = read.csv("artist_hiphop.csv")



artist_df = rbind(artist_df_2, artist_hiphop)


# Manually change missclassified artists:
missclass = c(955, 959, 964, 977, 992, 1011, 2795, 2796, 2798, 2802, 2822, 2826, 
              2841, 2865, 2883, 2954, 2999, 3118, 3272, 3297, 3409)

artist_df[missclass, 6] = 'rap'

# Selecting italian artists:
italians = (which(artist_df$X4 == 1))
dati_artisti = artist_df[italians,]


# Adding the aggregated genre column
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



# Final dataframe and edgelist:

dati_artisti_final = dati_artisti[!duplicated(dati_artisti$X1), -c(4,6)]
colnames(dati_artisti_final) = c('id', 'name', 'followers', 'popularity', 'genre')

edges_unique2 <- edges[!duplicated(apply(edges[,1:2], 1, function(row) paste(sort(row), collapse=""))),]

artisti_genre = as.vector(dati_artisti_final[,6])
artisti_popol = as.vector(dati_artisti_final[,4])
artisti_foll = as.vector(dati_artisti_final[,3])




#####
# Building network

g <- graph_from_data_frame(edges_unique2, directed=FALSE, vertices=nodes
                           )

# Setting attributes
g = set_vertex_attr(g, 'Genre', index = V(g), artisti_genre)
g = set_vertex_attr(g, 'Popularity', index = V(g), artisti_popol)
g = set_vertex_attr(g, 'Followers', index = V(g), artisti_foll)

V(g)[vertex_attr(g,'Genre') == 'Rap/Hip-Hop']$color = 'violetred'#'deeppink4'
V(g)[vertex_attr(g,'Genre') == 'Indie']$color = 'darkturquoise'#'turquoise4'
V(g)[vertex_attr(g,'Genre') == 'Pop']$color = 'darkorange1'#'azure4'
V(g)[vertex_attr(g,'Genre') == 'Emo/Punk']$color = 'slategray4'#'olivedrab'


# Removing some nodes who somehow ended up in the network but which are not italian:

which(V(g)$name == 'Simple Minds') # 563
which(V(g)$name == 'Gary Numan') # 766
which(V(g)$name == 'Killa Fonic') #114
which(V(g)$name == 'NANE') #113
neighbors(g, 97)
g = delete_vertices(g, c(113, 114, 563, 766))


# Computing size, degree, and removing isolated nodes:
gsize(g)
V(g)$degree <- degree(g)
zero_degree = which(degree(g)==0)
g = g - zero_degree



# Plotting degree distribution:


degree_g = sort(igraph::degree(g, v = V(g), mode = "all"), decreasing = TRUE)
length(which(degree_g== 0)) 
mean(degree_g) # Mean degree == 9.56

hist(degree_g[degree_g > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', ylab = 'Count', main = 'Degree histogram')

plot_degree_distribution(g, a = 'all', main = 'Degree')

x = degree_g

plot(log(degree_g[degree_g > 0]))

occur = as.vector(table(x))
occur = occur/sum(occur)
p = occur/sum(occur)
y = rev(cumsum(rev(p)))
x = as.numeric(names(table(x)))
plot(x, y, log="xy", type="l")



hst_to_plot <- function(dataset) {
  
  title <- "Degree distribution (log-log)"
  x <- "k"
  y <- "Count"
  
  deg <- degree(dataset)
  
  deg_y <- NULL
  for (i in 1:max(deg)) {
    deg_y <- c(deg_y, length(which(deg == i)))
  }
  
  
  ris <- tibble(k = 1:max(deg), p_k = deg_y) %>%
    filter(p_k > 0) %>%
    ggplot() +
    geom_point(aes(x = k, y = p_k), size = 1, color = 'firebrick') +
    #geom_smooth(aes(x = k, y= p_k), method = "lm", se = FALSE, color = 'cornflowerblue') +
    scale_x_continuous(trans = "log", labels = scales::label_scientific()) +
    scale_y_continuous(trans = "log", labels = scales::label_scientific()) +
    ggtitle(title) +
    xlab(x) +
    ylab(y)
    return(ris)
}


hst_to_plot(g)


# Plotting number of artists by genre:

length(which(V(g)$Genre == 'Emo/Punk'))

genres_name = c('Rap', 'Pop', 'Indie', 'Alt')
genres_count = c(291, 170, 165, 64)
sum(genres_count)

genre_df<- data.frame(genres_name, genres_count)


ggplot(data=genre_df, aes(x=genres_name, y=genres_count,fill=genres_name))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c('slategray4', 'darkturquoise', 'darkorange1', 'violetred'
                             ))+
  theme(legend.position="none") +
  labs(x = "Genre", y = "Number of artists") +
  ggtitle("Number of artists per genre") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 11),
        axis.title.y = element_text(margin = margin(r = 10), size = 11))
  



# Plotting network by using a backbone algorithm to highlight hubs:

backbone.suggest(g)
?sparsify.with.lspar
g_backbone = sparsify.with.localdegree(g, s = 0.1)
plot(g_backbone, vertex.size=ifelse(degree(g) >= 5, V(g)$degree/60*10, V(g)$degree+2), 
     edge.curverd=.1, arrow.size=.1, 
     main = "",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, 
     vertex.label = NA)#ifelse(degree(g) >= 60, V(g)$name, NA))
legend('topleft', legend=c("Rap", "Pop", 'Indie', 'Alt'),
       col=c( 'violetred', 'darkorange1', 'darkturquoise', 'slategray4'), pch=16, cex=0.8)


# Hubs:

quant = quantile(degree_g[degree_g>0],0.95)
print(quant)
hubs <- degree_g[degree_g>=quant]
hubs2 = V(g)[which(V(g)$degree >=quant)] # Same thing but returning vertices instead of array
length(hubs)

mean(V(g)$Popularity[hubs2]) # Mean hubs popularity = 60
mean(V(g)$Popularity) # Mean network popularity = 44


# ARE POPULAR ARTISTS HUBS?

names(degree_g[1:200])
df2 <- dati_artisti_final[order(dati_artisti_final$popularity,decreasing=TRUE),]

df2[1:36,]$name

intersect(names(degree_g[1:70]), df2[1:70,]$name)

V(g)[which(V(g)$name == "Måneskin")]

neighbors(g, v =116 )

# 95th percentile of popularity scores:
popularity_g = sort(V(g)$Popularity, decreasing = TRUE)
quant_popularity = quantile(popularity_g[popularity_g>0],0.95)
print(quant_popularity)

# 95th percentile of follower numbers:
followers_g = sort(V(g)$Followers, decreasing = TRUE)
quant_foll = quantile(followers_g[followers_g>0],0.95)
print(quant_foll)

# Which hubs have higher popularity and follower scores than these thresholds?

length(which(hubs2$Popularity >= quant_popularity)) #15
length(which(hubs2$Followers >= quant_foll)) # 13

length(which(hubs2$Genre != 'Rap/Hip-Hop')) # 2 hubs not part of the rap genre
hubs2[19]


# Hubs for popularity scores:
hubs_popularity <- V(g)[which(V(g)$Popularity >=quant_popularity)]
mean(hubs_popularity$Popularity) # Mean popularity = 70.5
hubs_popularity$name
names(hubs)

intersect(hubs2$name, hubs_popularity$name) # Hubs in common for degree and popularity




### I computed some metrics for the network, but they are not shown in the 
### slides as they are not really relevant for the analysis


# Clustering coefficient

clust_coeff = igraph::transitivity(
  g,
  type = 'undirected',
  vids = NULL,
  weights = NULL,
  isolates = c("NaN", "zero")
)

clust_coeff # 0.199


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

intersect(names(hubs),names(hubs_clos)) # 20 hubs are also hubs by closeness centrality


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

intersect(names(hubs),names(hubs_bet)) # 16 hubs are also hubs by betweenness centrality




# Segregation of the network:


# Mixing matrix
m = mixingm(g, "Genre", full = FALSE)
m


# Which pop artist and alternative artists are collaborating?


long_cat = igraph::as_long_data_frame(g)
long_cat_inter = long_cat[long_cat[,4] != long_cat[,10],]
long_cat_edges = long_cat_inter[,c(3,9)]

long_cat_inter_pop = long_cat_inter %>%
  filter(from_Genre=='Pop', to_Genre=='Emo/Punk')
long_cat_inter_pop_edges = long_cat_inter_pop[,c(3,10)]
g_inter_pop = igraph::graph_from_data_frame(long_cat_inter_pop_edges, directed=FALSE)

V(g_inter_pop)
gsize(g_inter_pop)

V(g_inter_pop)[c(1:7)]$color = 'darkorange1'
V(g_inter_pop)[c(8:13)]$color = 'slategray4'



plot(g_inter_pop, vertex.label = V(g_inter_pop)$name, layout = layout.fruchterman.reingold,
     vertex.label.dist=2.5, margin=-0.2, vertex.size = 6)
legend('topright', legend=c("Pop", 'Alt'),
       col=c( 'darkorange1', 'slategray4'), pch=16, cex=0.8)





# E-I, Freeman and SSI indexes

ei(g, "Genre")
freeman(g, 'Genre')

#install.packages("cli")
#library(devtools)
#install_github("mbojan/isnar")
#library(isnar)
#isnar::ei(g, "Genre")
                            # Cross checked with another package
                            # To see if results were the same (they are)



# Checking segregation for induced subgraphs containing only two genres:

genres = c('Rap/Hip-Hop', 'Pop', 'Indie', 'Emo/Punk')


ei_genres = c()

for (i in genres){
  #print(i)
  for (j in genres){
    if (i != j){
      nodes1 = V(g)[which(V(g)$Genre == i)]
      nodes2 = V(g)[which(V(g)$Genre == j)]
      net = induced_subgraph(g, vids = c(nodes1, nodes2))
      ei_gen = ei(net, "Genre")
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
      print(c(i, j, freeman_gen))
      freeman_genres[length(freeman_genres)+1]=freeman_gen
    }
  }
}

freeman_genres


# I also tried utilizing the spectral segregation index
# The results are not in the slide since this index is not really suited
# or this kind of network, but I wanted to try it
# Don't give too much weight to this section


specsi = (v <- ssi(g, "Genre"))
mean(specsi)
max(specsi)
hist(specsi, breaks = 100, col = 'azure4',
     xlab = 'SSI', main = 'SSI scores distribution')


      # SSI can be aggregated to group level simply by computing the average value:

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





# Assortativity:
        
igraph::assortativity_degree(g, directed = FALSE) # Assortativity (degree)

      # Assortativity (popularity and followers)
igraph::assortativity(g, igraph::V(g)$Popularity, types2 = NULL, directed = FALSE)

igraph::assortativity(g, igraph::V(g)$Followers, types2 = NULL, directed = FALSE)


# Computing degree correlation

knn_g = knn(
  g,
  vids = V(g),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)


knn_g$knnk

# Building degree correlation dataframe
degree_unsorted = as.data.frame(degree(g, v = V(g)))
degree_unsorted_un = sort(unique(degree_unsorted[,1]))
knnk = knn_g$knnk
knnk = knnk[!is.na(knnk)]

avg_degree_squared = degree_unsorted^2 # Neutral line step 1
avg_degree_squared = mean(avg_degree_squared[,1]) # Neutral line step 2
neutral = avg_degree_squared / mean(degree_unsorted[,1]) # Neutral line step 3

knnk = append(knnk, neutral)
#knnk = (knnk-min(knnk))/(max(knnk)-min(knnk))
#neutral_norm = knnk[57] #〈k2〉 / 〈k〉
knnk = knnk[1:56]






#"knn(k) is the average degree of the neighbors of all degree-k nodes.
#To quantify degree correlations we inspect the dependence of knn(k) on k."

# ...let's do so

plot1 = as.data.frame(cbind(unique(degree_unsorted_un), knnk))
colnames(plot1) = c('k', 'knnk')
#log_plot1 = data.frame(k=log(plot1$k),
#                       knnk=log(plot1$knnk)) # Convert data in log scale to visualize better


p1 = ggplot() + 
  geom_point(aes(x = k, y = knnk), data = plot1, color = "firebrick", shape = "diamond", size = 2) + 
  geom_hline(aes(yintercept=neutral, color = '  Random Prediction')) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "k", y = "knn(k)") +
  scale_x_continuous(trans = "log", labels = scales::label_scientific()) +
  scale_y_continuous(trans = "log", labels = scales::label_scientific()) +
  ggtitle("Degree correlation") + 
  scale_color_manual(name='',
                     breaks=c('  Random Prediction'),
                     values=c('  Random Prediction'='black')) +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic"),
        legend.title=element_text(size=0),
        legend.text=element_text(size=10),
        legend.position=c(0.8, 0.15),
        #legend.key = element_rect(fill = "white", colour = "black"),
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm')) #change legend title font size

p1


summary(lm(plot1$knnk ~ plot1$k))$coefficients # a = 0.14




# Degree preserving rewirings:

g_rs = rewire(g, with = keeping_degseq(loops = F, niter = 10000)) # Rewiring with single-links
igraph::assortativity_degree(g_rs, directed = FALSE) # Degree assortativity goes neutral

g_rm = rewire(g, with = keeping_degseq(loops = T, niter = 10000)) # Rewiring with multi-links
igraph::assortativity_degree(g_rm, directed = FALSE) #Degree assortativity goes neutral

# The popularity assortativity also goes to zero but that's expected...
# ...it's not really a relevant thing right?
igraph::assortativity(g_rm, igraph::V(g_rm)$Popularity, types2 = NULL, directed = FALSE)
igraph::assortativity(g_rs, igraph::V(g_rs)$Popularity, types2 = NULL, directed = FALSE)


# Degree correlation for rewired network

knn_rs = knn(
  g_rs,
  vids = V(g_rs),
  mode = 'all',
  neighbor.degree.mode = 'all',
  weights = NULL
)

knnk_rs = knn_rs$knnk
knnk_rs = knnk_rs[!is.na(knnk_rs)]

plot2 = data.frame(k=plot1$k,
                   knnk=knnk_rs)


summary(lm(knnk_rs ~ plot1$k))$coefficients # a = -0.05



p2 = ggplot() + 
  geom_point(aes(x = k, y = knnk, color = "  Real Network (a = 0.14)"), data = plot1, shape = "diamond", size = 2) + 
  geom_point(aes(x = k, y = knnk, color = "  R-S (a = -0.05)"), data = plot2, shape = "diamond", size = 2) + 
  geom_hline(aes(yintercept=neutral, color = '  Random Prediction')) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "k", y = "knn(k)") +
  scale_x_continuous(trans = "log", labels = scales::label_scientific()) +
  scale_y_continuous(trans = "log", labels = scales::label_scientific()) +
  ggtitle("Degree correlation") + 
  scale_color_manual(name='',
                     breaks=c('  Random Prediction', "  Real Network (a = 0.14)", "  R-S (a = -0.05)"),
                     values=c('  Random Prediction'='black', "  R-S (a = -0.05)"='green4', "  Real Network (a = 0.14)"='firebrick')) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 0, 0) ) ) ) + 
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, face = "italic"),
        legend.title=element_text(size=0),
        legend.text=element_text(size=10),
        legend.position=c(0.8, 0.15),
        #legend.key = element_rect(fill = "white", colour = "black"),
        legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm')) #change legend title font size





p2

summary(lm(log_plot2$knnk ~ log_plot2$k))$coefficients



# knn(k) but for popularity: is it possible? Let's find out:
# This is basically computing the popularity correlation
# I'm not totally sure of the theoretical implications of it but it should make sense


V(g)$name

# get a list of neighbours, for each node
g_ngh <- neighborhood(g, mindist = 1) 

# write a function that gets the means                       
get.mean <- function(x){
  mean(V(g)$Popularity[x]-1)
}

# apply the function, add result to the graph
V(g)$av_pop_nei <- sapply(g_ngh, get.mean)


# get data into dataframe
d_vert_attr <- igraph::as_data_frame(g, what = "vertices")

popularities = sort(unique(V(g)$Popularity))
which(d_vert_attr$Popularity == 7) # Just a check

pnn = data.frame(p = popularities, pnnp = 0)

count_pnnp = 1
for (i in popularities){
  popol1 = d_vert_attr[which(d_vert_attr$Popularity == i), ]
  print(nrow(popol1))
  
  pnnp_loop = mean(popol1$av_pop_nei)#ifelse(nrow(popol1) > 1, rowMeans(popol1[ ,7]), popol1[,7])
  pnn[count_pnnp,2] = pnnp_loop
  count_pnnp = count_pnnp + 1
}


# Putting it in a dataframe to plot:

plot_popol = data.frame(p=pnn$p,
                       pnnp=pnn$pnnp)
plot_popol
p3 = ggplot() + 
  geom_point(aes(x = p, y = pnnp), data = plot_popol, color = "mediumorchid", shape = "diamond", size = 2) + 
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Popularity score", y = "Average neighbors' popularity") +
  scale_x_continuous(trans = "log", labels = scales::label_scientific()) +
  scale_y_continuous(trans = "log", labels = scales::label_scientific()) +
  ggtitle("Popularity score correlation") + 
  
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 13, face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10), size = 13, face = "italic"))
        

p3


# I also run 1000 rewirings and average the results of the assortativity
# to get a better idea 

assort_values = c()
assort_values_rm = c()

for (i in seq(1000)){
  shuffle = rewire(g, with = keeping_degseq(loops = F, niter = 10000))
  assort = igraph::assortativity_degree(shuffle, directed = FALSE)
  assort_values[length(assort_values)+1] = assort
  
}

for (i in seq(1000)){
  shuffle = rewire(g, with = keeping_degseq(loops = T, niter = 10000))
  assort = igraph::assortativity_degree(shuffle, directed = FALSE)
  print
  assort_values_rm[length(assort_values_rm)+1] = assort
  
}

mean(assort_values)
mean(assort_values_rm)




# Here I'm basically doing the same assortativity analysis
# for the different genre sub-networks


    ### Rap network:


g_rap = igraph::delete_vertices(g, igraph::V(g)[!igraph::vertex_attr(g, "Genre") == 'Rap/Hip-Hop'])
zero_degree_rap = which(igraph::degree(g_rap)==0)

g_rap = g_rap - zero_degree_rap
gsize(g_rap)


degree_rap = sort(igraph::degree(g_rap, v = igraph::V(g_rap)), decreasing = TRUE)
length(which(degree_rap== 0))
mean(degree_rap) # <k> = 13
mean(V(g_rap)$Popularity) # 47





quant_rap = quantile(degree_rap[degree_rap>0],0.95)
hubs_rap <- degree_rap[degree_rap>=quant_rap]

hubs_rap

igraph::assortativity_degree(g_rap, directed = FALSE)
igraph::assortativity(g_rap, igraph::V(g_rap)$Popularity, directed = FALSE)

g_shuffle_rap = rewire(g_rap, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_rap, directed = FALSE)

g_rm_rap = rewire(g_rap, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_rap, directed = FALSE)

assort_rap_values = c()
assort_rap_values_rm = c()

for (i in seq(1000)){
  shuffle_rap = rewire(g_rap, with = keeping_degseq(loops = F, niter = 10000))
  assort_rap = igraph::assortativity_degree(shuffle_rap, directed = FALSE)
  assort_rap_values[length(assort_rap_values)+1] = assort_rap
  
}

for (i in seq(1000)){
  shuffle_rap = rewire(g_rap, with = keeping_degseq(loops = T, niter = 10000))
  assort_rap = igraph::assortativity_degree(shuffle_rap, directed = FALSE)
  print
  assort_rap_values_rm[length(assort_rap_values_rm)+1] = assort_rap
  
}

mean(assort_rap_values)
mean(assort_rap_values_rm)


      ### Pop network:


g_pop = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Pop'])
zero_degree_pop = which(degree(g_pop)==0)

g_pop = g_pop - zero_degree_pop
gsize(g_pop)


degree_pop = sort(igraph::degree(g_pop, v = V(g_pop)), decreasing = TRUE)
length(which(degree_pop== 0))
mean(degree_pop)# <k> = 5
mean(V(g_pop)$Popularity) # 52




quant_pop = quantile(degree_pop[degree_pop>0],0.95)
hubs_pop <- degree_pop[degree_pop>=quant_pop]

hubs_pop

assortativity_degree(g_pop, directed = FALSE)
igraph::assortativity(g_pop, igraph::V(g_pop)$Popularity, directed = FALSE)


g_shuffle_pop = rewire(g_pop, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_pop, directed = FALSE)

g_rm_pop = rewire(g_pop, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_pop, directed = FALSE)

assort_pop_values = c()
assort_pop_values_rm = c()

for (i in seq(1000)){
shuffle_pop = rewire(g_pop, with = keeping_degseq(loops = F, niter = 10000))
assort_pop = igraph::assortativity_degree(shuffle_pop, directed = FALSE)
assort_pop_values[length(assort_pop_values)+1] = assort_pop

}

for (i in seq(1000)){
  shuffle_pop = rewire(g_pop, with = keeping_degseq(loops = T, niter = 10000))
  assort_pop = igraph::assortativity_degree(shuffle_pop, directed = FALSE)
  print
  assort_pop_values_rm[length(assort_pop_values_rm)+1] = assort_pop
  
}

mean(assort_pop_values)
mean(assort_pop_values_rm)


sqrt(mean(degree_pop)*length(V(g_pop)))
max(degree_pop)





      ### Indie:

g_indie = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Indie'])
zero_degree_indie = which(degree(g_indie)==0)

g_indie = g_indie - zero_degree_indie
gsize(g_indie)


degree_indie = sort(igraph::degree(g_indie, v = V(g_indie)), decreasing = TRUE)
length(which(degree_indie== 0))
mean(degree_indie) # <k> = 3
mean(V(g_indie)$Popularity) # 40
mean(V(g_indie)$degree)

quant_indie = quantile(degree_indie[degree_indie>0],0.95)
hubs_indie <- degree_indie[degree_indie>=quant_indie]

hubs_indie


assortativity_degree(g_indie, directed = FALSE)
igraph::assortativity(g_indie, igraph::V(g_indie)$Popularity, directed = FALSE)

g_shuffle_indie = rewire(g_indie, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_indie, directed = FALSE)

g_rm_indie = rewire(g_indie, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_indie, directed = FALSE)

most_popular_indie = sort(V(g_indie)$Popularity, decreasing = T)


assort_indie_values = c()
assort_indie_values_rm = c()

for (i in seq(1000)){
  shuffle_indie = rewire(g_indie, with = keeping_degseq(loops = F, niter = 10000))
  assort_indie = igraph::assortativity_degree(shuffle_indie, directed = FALSE)
  assort_indie_values[length(assort_indie_values)+1] = assort_indie
  
}

for (i in seq(1000)){
  shuffle_indie = rewire(g_indie, with = keeping_degseq(loops = T, niter = 10000))
  assort_indie = igraph::assortativity_degree(shuffle_indie, directed = FALSE)
  print
  assort_indie_values_rm[length(assort_indie_values_rm)+1] = assort_indie
  
}

mean(assort_indie_values)
mean(assort_indie_values_rm)


      ###Alt:

g_emo = delete_vertices(g, V(g)[!vertex_attr(g, "Genre") == 'Emo/Punk'])
zero_degree_emo = which(degree(g_emo)==0)

g_emo = g_emo - zero_degree_emo
gsize(g_emo)

plot(g_emo,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_emo = sort(igraph::degree(g_emo, v = V(g_emo)), decreasing = TRUE)
length(which(degree_emo== 0))
mean(degree_emo) #<k> = 1.5
mean(V(g_emo)$Popularity) # 19.4



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


assortativity_degree(g_emo, directed = FALSE)
igraph::assortativity(g_emo, igraph::V(g_emo)$Popularity, directed = FALSE)

g_shuffle_emo = rewire(g_emo, with = keeping_degseq(loops = F, niter = 10000))
igraph::assortativity_degree(g_shuffle_emo, directed = FALSE)

g_rm_emo = rewire(g_emo, with = keeping_degseq(loops = T, niter = 10000))
igraph::assortativity_degree(g_rm_emo, directed = FALSE)


assort_emo_values = c()
assort_emo_values_rm = c()

for (i in seq(1000)){
  shuffle_emo = rewire(g_emo, with = keeping_degseq(loops = F, niter = 10000))
  assort_emo = igraph::assortativity_degree(shuffle_emo, directed = FALSE)
  assort_emo_values[length(assort_emo_values)+1] = assort_emo
  
}

for (i in seq(1000)){
  shuffle_emo = rewire(g_emo, with = keeping_degseq(loops = T, niter = 10000))
  assort_emo = igraph::assortativity_degree(shuffle_emo, directed = FALSE)
  print
  assort_emo_values_rm[length(assort_emo_values_rm)+1] = assort_emo
  
}

mean(assort_emo_values)
mean(assort_emo_values_rm)



  
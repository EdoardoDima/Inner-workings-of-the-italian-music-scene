#install.packages("netseg")
library(igraph)
library(netseg)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dplyr)

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


V(g)$degree <- degree(g)
zero_degree = which(degree(g)==0)

g = g - zero_degree


# Plotting:

plot(g, vertex.size=5, edge.curverd=.1, arrow.size=.1, vertex.color = "orchid", 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, 
     vertex.label = ifelse(degree(g) >= 34, V(g)$name, NA))


degree_g = sort(igraph::degree(g, v = V(g), mode = "all"), decreasing = TRUE)
length(which(degree_g== 0)) 
mean(degree_g)

hist(degree_g[degree_g > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

# Hubs:

quant = quantile(degree_g[degree_g>0],0.95)
print(quant)
hubs <- degree_g[degree_g>=quant]

hubs

g_hubs = subgraph(g, hubs)


# Clustering coefficient

igraph::transitivity(
  g,
  type = 'undirected',
  vids = NULL,
  weights = NULL,
  isolates = c("NaN", "zero")
)

# Closeness centrality

mean(igraph::closeness(
  g,
  vids = igraph::V(g),
  mode = 'total',
  weights = NULL,
  normalized = TRUE,
  cutoff = -1
))


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
long_cat_inter = long_cat[long_cat[,4] != long_cat[,9],]
long_cat_edges = long_cat_inter[,c(3,8)]

long_cat_inter_pop = long_cat_inter %>%
  filter(from_Genre=='Pop', to_Genre=='Emo/Punk')
long_cat_inter_pop_edges = long_cat_inter_pop[,c(3,8)]
g_inter_pop = igraph::graph_from_data_frame(long_cat_inter_pop_edges, directed=FALSE)
plot(g_inter_pop)


# E-I and SSI

ei(g, "Genre")
orwg(g, "Genre")

#install.packages("cli")
#library(devtools)
#install_github("mbojan/isnar")
#library(isnar)
#isnar::ei(g, "Genre")
                            # Cross checked with another package
                            # To see if results were the same






specsi = (v <- ssi(g, "Genre"))
specsi_sort = sort(specsi, decreasing = FALSE)
specsi_sort
 
index_specsi = as.numeric(names(specsi_sort[1:75]))
cane1 = V(g)[index_specsi]$name
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


# Assortativity:
igraph::assortativity_degree(g, directed = FALSE)

igraph::assortativity(g, igraph::V(g)$Popularity, types2 = NULL, directed = FALSE)
#get.vertex.attribute(g,'Popularity')


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
??pivot_longer
data = melt(out)
library(ggplot2)
full_range <- function(x) seq(min(x), max(x))
data %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Var1") %>% 
  tidyr::pivot_longer(-Var1, names_to="Var2") %>% 
  mutate(across(Var1:Var2, as.numeric)) %>% {
    d <- .
    expand_grid(Var1=full_range(d$Var1), Var2=full_range(d$Var2)) %>% 
      left_join(d) %>% 
      replace_na(list(value=0))
  } %>% 
  ggplot() +
  theme_bw() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_viridis_c(name = "") +
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

degree_rap_unsorted = as.data.frame(degree(g_rap, v = V(g_rap)))
plot1_rap = as.data.frame(cbind(degree_rap_unsorted, knn_rap[[1]]))

colnames(plot1_rap) = c('k', 'knn')
plot(plot1_rap$k, plot1_rap$knn)


igraph::assortativity(g_rap, igraph::V(g_rap)$Popularity, directed = FALSE)


      ### Pop network:


g_pop = delete_vertices(g_v2, V(g_v2)[!vertex_attr(g_v2, "Genre") == 'Pop'])
zero_degree_pop = which(degree(g_pop)==0)

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

degree_pop_unsorted = as.data.frame(degree(g_pop, v = V(g_pop)))
plot1_pop = as.data.frame(cbind(degree_pop_unsorted, knn_pop[[1]]))

colnames(plot1_pop) = c('k', 'knn')
plot(plot1_pop$k, plot1_pop$knn)

assortativity(g_pop, get.vertex.attribute(g_pop,'Popularity'), types2 = NULL, directed = FALSE)



      ### Indie:

g_indie = delete_vertices(g_v2, V(g_v2)[!vertex_attr(g_v2, "Genre") == 'Indie'])
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

degree_indie_unsorted = as.data.frame(degree(g_indie, v = V(g_indie)))
plot1_indie = as.data.frame(cbind(degree_indie_unsorted, knn_indie[[1]]))

colnames(plot1_indie) = c('k', 'knn')
plot(plot1_indie$k, plot1_indie$knn)

assortativity(g_indie, get.vertex.attribute(g_indie,'Popularity'), types2 = NULL, directed = FALSE)

most_popular_indie = get.vertex.attribute(g_indie,'Popularity')

      ###Emo:

g_emo = delete_vertices(g, V(g)[!vertex_attr(g_v2, "Genre") == 'Emo/Punk'])
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

degree_emo_unsorted = as.data.frame(degree(g_emo, v = V(g_emo)))
plot1_emo = as.data.frame(cbind(degree_emo_unsorted, knn_emo[[1]]))

colnames(plot1_emo) = c('k', 'knn')
plot(plot1_emo$k, plot1_emo$knn)

assortativity(g_emo, get.vertex.attribute(g_emo,'Popularity'), types2 = NULL, directed = FALSE)



  
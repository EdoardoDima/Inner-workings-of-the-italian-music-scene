library(igraph)

nodes = read.csv("C:/Users/CreCre/Documents/nodes_2.csv")
edges = read.csv("C:/Users/CreCre/Documents/edges_2_clean.csv")

artist_df_2 = read.csv("C:/Users/CreCre/Documents/artist_df_2.csv")
artist_hiphop = read.csv("C:/Users/CreCre/Documents/artist_hiphop.csv")


artist_df = rbind(artist_df_2, artist_hiphop)
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

edges_unique = edges[!duplicated(edges), ]


g <- graph_from_data_frame(edges_unique, directed=FALSE, vertices=nodes)
V(g)$degree <- degree(g)
zero_degree = which(V(g)$degree==0)

g = g - zero_degree

plot(g, vertex.size=5, edge.curverd=.1, arrow.size=.1, vertex.color = "orchid", 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, vertex.label = NA)


degree_g = sort(igraph::degree(g, v = V(g), mode = "all"), decreasing = TRUE)
length(which(degree_g== 0)) # 201 disconnected
mean(degree_g)

hist(degree_g[degree_g > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')



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

g_v2 = set_vertex_attr(g_v2, 'Genre', index = V(g_v2), artisti_cc_genre)
g_v2 = set_vertex_attr(g_v2, 'Popularity', index = V(g_v2), artisti_cc_popol)
get.vertex.attribute(g_v2,'Popularity')

plot(g, vertex.color = pal[as.numeric(as.factor(vertex_attr(g_v2, "Genre")))])

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


g_rap = delete_vertices(g_v2, V(g_v2)[!vertex_attr(g_v2, "Genre") == 'Rap/Hip-Hop'])


plot(g_rap,vertex.size=5, edge.curverd=.1, arrow.size=.1, 
     main = "Ueeeueeee",
     layout = layout.fruchterman.reingold,
     arrow.width=.1, edge.arrow.size=.1, vertex.label = NA)

degree_rap = sort(igraph::degree(g_rap, v = V(g_rap), mode = "all"), decreasing = TRUE)
length(which(degree_rap== 0)) # 201 disconnected
mean(degree_rap)

hist(degree_rap[degree_rap > 0], breaks = 100, col = 'orchid',
     xlab = 'Degree', main = 'UEUEUE')

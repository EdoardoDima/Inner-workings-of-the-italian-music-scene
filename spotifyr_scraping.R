library(devtools)
#remotes::install_github("tiagomendesdantas/Rspotify")
#library(Rspotify)
#install.packages('spotifyr')
library(spotifyr)


Sys.setenv(SPOTIFY_CLIENT_ID = 'd800a5bdbb48410694ea1c81f6f8c43d')
             #'04be970e055a4f57919398dff8223d7c')#'3a1af47d371a44e99298835f767c9cfc')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '5726284a13b248f98ab068c1e3930124')
             #'116121fd5e8247f98134ae310bfaf182')#'ad065f3013c04f8492f870f82c61f794')

access_token <- get_spotify_access_token()



### SCRAPING:

shiva = search_spotify(
  'Shiva',
  type = 'artist',
  market = NULL,
  limit = 1,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

shiva$id

art = get_artist("2K5nCggbhSZ00YCYP5qkZS", authorization = access_token
           )
art
print(art$genre)

#offsets = c(0, 51, 101, 151, 201, 251, 301, 351)

offsets = seq(from = 0 , to = 900, by = 50)
offsets
#offsets_missing = offsets[-1]
#offsets = c(0, offsets)
artist_df = data.frame(1, 2, 3, 4, 5, 6, 7)
artist_df

genres_spot = c('rap', 'indie', 'indie-pop', 'pop', 'emo', 'punk-rock', 'punk')

counter = -1
for (genre in genres_spot){
  print(genre)
  counter = counter + 1
  if(counter > 0) {Sys.sleep(10)} 
  
  for (i in offsets){
  #counter = counter + 1
  print(i)
  #print(counter)
  #if(counter %% 2 == 0) {Sys.sleep(60)} 
    
  a = get_genre_artists(
    genre = genre,
    market = 'IT',
    limit = 50,
    offset = i,
    authorization = access_token
  )

    b = a['id']
  
    for (j in b$id){
    art = get_artist(j, authorization = access_token
    )
    ita_term = c('ita', 'nap', 'neap', 'mil', 'roma')
    matches_ita <- unique (grep(paste(ita_term,collapse="|"), 
                            art$genres, value=TRUE))
    italian = ifelse(length(matches_ita) > 0, 1, 0)
    #print(art$followers$total)
    row_df = data.frame(art$id, art$name, art$followers$total, italian, art$popularity, genre, art$type)
    artist_df[nrow(artist_df) + 1,] = row_df
    }
  }
  print('Completed one lap ya-hoo')
}

#missing_artists = artist_df
#missing_artists2 = missing_artists[-1,]

#artist_df2 = rbind(artist_df, missing_artists2)

#write.csv(artist_df2, "C:/Users/CreCre/Documents/artist_df_2.csv", row.names=FALSE)

#### END SCRAPING





artist_df = read.csv("C:/Users/CreCre/Documents/artist_df_2.csv")



length(which(artist_df$X4 == 1))
length(which(duplicated(artist_df$X1) == TRUE))

art2 = artist_df[!duplicated(artist_df$X1), ]
italians = (which(art2$X4 == 1))
art3 = art2[italians,]
art4 = art3[,-1]


## Loop infernale per prendere artisti con cui hanno collaborato:



nomi = art3[,c(1,2)]
nomi_loop = nomi

offsets_album = seq(0, 400, by = 20)

collab = data.frame(matrix(nrow = 0, ncol = 400))
  #data.frame(matrix(nrow = length(nomi_loop$X2), ncol = 400))
#row.names(collab) <- nomi_loop$X2
#collab


counter_nomi = 1
for (i in nomi_loop$X1[301:836]){
  if(counter_nomi %% 10 == 0) {print('Powernapping')} 
  if(counter_nomi %% 10 == 0) {Sys.sleep(60)} 
  
  counter_arr = 1
  rel_arr = c()
  
  albums_len = c(1)
    
  
  
    for (off in offsets_album){
      if (albums_len[length(albums_len)] > 0){
    
    albums = get_artist_albums(
      i,
      include_groups = "appears_on",
      market = 'IT',
      limit = 20,
      offset = off,
      authorization = access_token,
      include_meta_info = FALSE
    )
    
    #print(off)
    albums = albums$id[which(albums$album_type != 'compilation')]
    
    albums_len[length(albums_len) +1] = length(albums)
    #print(albums_len)
    
    if (length(albums) > 0){
    
    rel = get_albums(albums, market = NULL, authorization = access_token, include_meta_info = FALSE)
    
      for (related in rel[[2]]){
        nome_related = related[3]
        rel_arr[counter_arr] = nome_related[1,]
        counter_arr = counter_arr + 1
      }
     }
    }
   }
  
  if (length(rel_arr) < 400){rel_arr[length(rel_arr):400] = NA}
  
  #row_collab = c(i, rel_arr)
  #print(rel_arr)
  collab[nrow(collab) + 1,] = rel_arr
  counter_nomi = counter_nomi + 1
  print(counter_nomi)
  print(Sys.time())
}


#write.csv(what2, "C:/Users/CreCre/Documents/collab_836.csv", row.names=FALSE)


#what = read.csv("C:/Users/CreCre/Documents/collab_300.csv")
#what2 = rbind(what, collab)

collab = read.csv("C:/Users/CreCre/Documents/collab_836.csv")
#row.names(collab) <- nomi_loop$X2




collab_correct = cbind(nomi_loop$X2, collab)

listozza2 = as.vector(collab_correct[1,2:400])

nodes = as.data.frame(nomi_loop$X2) 
edges = data.frame(matrix(nrow = 334400, ncol = 2))

counter_artista = 1
counterinho = 1
for (row in 1:nrow(collab_correct)) {
  print(counter_artista)
  counter_artista = counter_artista + 1
  
  nome = collab_correct[row, 1]
  listozza = as.vector(collab_correct[row,2:400])
  for(i in listozza){
    edge = c(nome, i)
    #print(edge)
    edges[counterinho, ] = edge
    counterinho = counterinho + 1
    #print(counterinho)
  }
}

write.csv(nodes, "C:/Users/CreCre/Documents/nodes_1.csv", row.names=FALSE)
write.csv(edges_final, "C:/Users/CreCre/Documents/edges_1.csv", row.names=FALSE)


edges_final = edges[complete.cases(edges), ]

print('Geolier' %in% nomi_loop$X2)

library("dplyr")

to_remove_df = c()

counter_remove = 1
for (row in 1:nrow(edges_final)) {
  #listozza = as.vector(collab[row,])
  sec = edges_final[row,2]
  if (sec %in% nomi_loop$X2 == FALSE){
    print(row)
    to_remove_df[counter_remove] = row
    counter_remove = counter_remove + 1
  }
}


edges_final_clean = edges_final[-c(to_remove_df),]


library(igraph)
g <- graph_from_data_frame(edges_final_clean, directed=FALSE, vertices=nodes)

plot(g, vertex.size=5, edge.curverd=.1, arrow.size=.1, vertex.color = "orchid", 
     main = "UEEE",
     arrow.width=.1, edge.arrow.size=.1, layout= layout.fruchterman.reingold, vertex.label = NA)

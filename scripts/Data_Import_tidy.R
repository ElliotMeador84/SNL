

library(tidyverse)
library(RColorBrewer)
library(igraph)
library(jtools)
library(rgexf)  
snl_files <- paste0('C:/R/SNL/New folder/',list.files('C:/R/SNL/New folder/'))



snl_dfs <- map(snl_files,function(x){
  read_csv(x)
}) #impost all files


names(snl_dfs) <- gsub('\\.csv','',list.files('C:/R/SNL/New folder/')) #name the lists

list2env(snl_dfs ,.GlobalEnv)# send all objects to environment



# Create snl_info  --------------------------------------------------------

# snl_info is a dataset that 
# can be used for reference guide to the variables


df_names <- map(snl_dfs,function(x){
  names(x)
}) %>% flatten_chr() # gets the names in vector

longs <- map(snl_dfs,function(x){
  length(x)
}) %>% flatten_chr()  # gets the length in vector


df_name <- map2(longs,names(snl_dfs),function(x,y){
  y = rep(y,x)
}) %>% flatten_chr() # reps the name the length

snl_info <- data_frame(df_name,variables = df_names) 


snl_info$nrow <- map2(snl_dfs,longs,function(x,y){
  rep(nrow(x),y)
}) %>% flatten_dbl() # reps the nrow the length



# Join datasets for graphing ----------------------------------------------



k <- left_join(casts,actors)

y <- left_join(k,seasons)


snl_plot_gender_season <- y %>% 
  filter(gender != 'unknown') %>% 
  rename(season = sid) %>% 
  group_by(season) %>% 
  count(gender) %>% 
  ggplot(.,aes(season,n))+
  geom_point(aes(color = gender)) +
  geom_line(aes(color = gender)) +
  scale_color_manual(values = c('tomato1','skyblue4'))+
  annotate('text',x = 20, y = 12.5,label = 'male') +
  annotate('text',x = 28, y = 5.5,label = 'female') +
  theme_apa(legend.pos = 'none')+
  ggtitle('SNL cast gender by season')


## Build full plots
## 
alpha <- left_join(titles,sketches)

beta <- right_join(alpha,appearances)

theta <- left_join(beta,actors)
delta <- theta %>% 
  filter(category == 'Sketch') %>% 
  select(name,aid,gender,sid)
head(delta)


## Graphs
x <- graph_from_data_frame(delta,directed = F)

V(x)$type <- V(x)$name %in% delta$name

x_projs <- bipartite.projection(x)


x_n <- x_projs$proj1

x_n_att <- data_frame(
  aid =  V(x_n)$name,
  degree = igraph::degree(x_n),
  betweeness = igraph::betweenness(x_n),
  closeness = igraph::closeness(x_n),
  eigenvector = eigen_centrality(x_n)[[1]])


iota <- left_join(x_n_att,delta[c(2,3)])


lambda <- distinct(iota)
kappa <- lambda %>% 
  mutate(color = ifelse(.$gender == 'male','#227066','#FFF07A'))


# kappa graph 
# on  x_n
V(x_n)$label <- NULL                       # vertex label
V(x_n)$color <- adjustcolor(kappa$color,.9) # vertex color
V(x_n)$size <- rescale(kappa$degree,2,25) #vertex size
V(x_n)$gender <- kappa$gender
x_n_fr <- layout.kamada.kawai(x_n)        # layout
E(x_n)$color <- 'black'                  # edge color

par(mar= c(0,0,0,0),bg = 'black') # plot paramaters

plot(x_n,layout = x_n_fr) # full igraph plot

## save to Gephi style .csv

x_full_plot <- to_source_target(x_n)
write_csv(x_full_plot,'x_full_plot.csv',col_names = T)

x_full_plot_labs <- to_source_target_labels(x_n)
write_csv(x_full_plot_labs,'x_full_plot_labs.csv',col_names = T)



## n degree neighborhood of most influential members

females <- kappa_scale %>% 
  filter(gender == 'female') %>% #pull influential 
  top_n(1,degree) %>%           # female members
  pull(aid)

males <- kappa_scale %>% 
  filter(gender == 'male') %>%   #pull influential 
  top_n(1,degree) %>%           #  male members
  pull(aid)

#create sub graphs
female_neighs <- ego(x_n, 1, females, mode = c("all"), #females
                     mindist = 0) %>% flatten_dbl(.)   

female_sub <- induced.subgraph(x_n,female_neighs)


male_neighs <- ego(x_n, 1, males, mode = c("all"), #males
                   mindist = 0) %>% flatten_dbl(.)
male_sub <- induced.subgraph(x_n,male_neighs)


####


# Feb 15, 2018 ------------------------------------------------------------


library(tidyverse)
library(igraph)

source(paste0(getwd(),'/scripts/functions.R'))
file.edit(paste0(getwd(),'/scripts/functions.R'))

write_csv(to_source_target(x_n),'x_n.csv')
write_csv(to_source_target_labels(x_n),'x_n_nodes.csv')


# change the size by controlling for the number of episodes
# still color by the seasons

delta %>% 
  group_by(aid) %>%
  count(name)

file.edit(paste0(getwd(),'/scripts/Data_Import_tidy.R'))



head(delta)


# save(kappa,file = paste0(getwd(),'/Rdata','/kappa.RData'))

attributes(kappa)$info <- 'kappa is the primary data frame for organising the x_n graph characteristics; '

kappa <- kappa %>% 
  mutate(id = 1:nrow(.))

attributes(kappa)$info

kappa <- left_join(
  kappa,
  delta %>% 
    select(aid,name) %>% 
    group_by(aid) %>% 
    count(name) %>% 
    group_by(aid) %>% 
    summarise(sketches = sum(n)) %>% 
    arrange(desc(sketches))
)
V(x_n)$number.sketches <- kappa$sketches

kappa <- left_join(
  kappa,
  delta %>% 
    group_by(aid) %>% 
    summarise(average.tenure = mean(sid))
)

V(x_n)$average.tenure <- kappa$average.tenure

write_csv(to_source_target_labels(x_n),'x_n_nodes.csv')











































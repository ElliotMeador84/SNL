

library(tidyverse)
library(RColorBrewer)
library(igraph)
library(jtools)

snl_files <- paste0('~/Downloads/snldb/',list.files('~/Downloads/snldb/'))



snl_dfs <- map(snl_files,function(x){
  read_csv(x)
}) #impost all files


names(snl_dfs) <- gsub('\\.csv','',list.files('~/Downloads/snldb/')) #name the lists

list2env(snl_dfs ,.GlobalEnv)# send all objects to environment





df_names <- map(snl_dfs,function(x){
  names(x)
}) %>% flatten_chr() 

longs <- map(snl_dfs,function(x){
  length(x)
}) %>% flatten_chr()


df_name <- map2(longs,names(snl_dfs),function(x,y){
  y = rep(y,x)
}) %>% flatten_chr()

snl_info <- data_frame(df_name,variables = df_names) 

snl_info %>% 
  group_by(value) %>% 
  count() %>% 
  arrange(desc(n))

snl_info$nrow <- map2(snl_dfs,longs,function(x,y){
  rep(nrow(x),y)
}) %>% flatten_dbl()

snl_info %>% 
  filter(df_name %in% c('casts',
                        'actors',
                        'seasons')) %>% 
  group_by(variables) %>% 
  count()


map(snl_dfs,function(x){
  glimpse(x)$aid
})

snl_info %>% 
  filter(variables == 'tid')



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
V(x_n)$label <- NA                        # vertex label
V(x_n)$color <- adjustcolor(kappa$color,.9) # vertex color
V(x_n)$size <- rescale(kappa$degree,2,25) #vertex size
x_n_fr <- layout.kamada.kawai(x_n)        # layout
E(x_n)$color <- 'black'                  # edge color

par(mar= c(0,0,0,0),bg = 'black')

plot(x_n,layout = x_n_fr)

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
male_neighs <- ego(x_n, 1, males, mode = c("all"), #males
                   mindist = 0) %>% flatten_dbl(.)



female_sub <- induced.subgraph(x_n,female_neighs)
male_sub <- induced.subgraph(x_n,male_neighs)


plot(female_sub)
plot(male_sub)
#####################################################
## Next steps determine the percents of males and  ##  ## females in one anothers 1st degree networks     ##
#####################################################

# kappa ggplots
# 
xgen <- c('female','male')
kappa_scale <- kappa %>% 
  filter(gender %in% xgen) %>% 
  mutate_if(is.numeric,~scale(.)) 


ggplot(kappa_scale,aes(closeness,degree))+
  geom_jitter(aes(color = gender,fill = gender),size = 2.5,alpha = .9)+
  scale_fill_manual(values = c('#227066','#FFF07A'))+
  scale_color_manual(values = c('#227066','#FFF07A'))+
  ggtitle('Closeness by degree')+
  theme_apa(legend.pos = 'bottom')+
  coord_equal()


names(kappa_scale)
ggplot(kappa_scale,aes(betweeness,degree))+
  geom_jitter(aes(color = gender,fill = gender),size = 2.5,alpha = .9)+
  scale_fill_manual(values = c('#227066','#FFF07A'))+
  scale_color_manual(values = c('#227066','#FFF07A'))+
  ggtitle('Betweenness by degree')+
  theme_apa(legend.pos = 'bottom')+
  coord_equal()

ggplot(kappa_scale,aes(eigenvector,degree))+
  geom_jitter(aes(color = gender,fill = gender),size = 2.5,alpha = .9)+
  scale_fill_manual(values = c('#227066','#FFF07A'))+
  scale_color_manual(values = c('#227066','#FFF07A'))+
  ggtitle('Eigenvector by degree')+
  theme_apa(legend.pos = 'bottom')+
  coord_equal()

#top females
kappa_scale %>% 
  filter(gender == 'female') %>% 
  top_n(10,degree) %>% 
  select(-gender,-color) %>% 
  gather(key,value,-aid) %>% 
  ggplot(.,aes(fct_reorder(aid,value,fun = mean),value)) +
  geom_bar(stat = 'identity') +
  facet_grid(.~key,scales = 'free_x')+
  coord_flip()+
  theme_apa()



#top males
kappa_scale %>% 
  filter(gender == 'male') %>% 
  top_n(10,degree) %>% 
  select(-gender,-color) %>% 
  gather(key,value,-aid) %>% 
  ggplot(.,aes(fct_reorder(aid,value,fun = mean),value)) +
  geom_bar(stat = 'identity') +
  facet_grid(.~key,scales = 'free_x')+
  coord_flip()+
  theme_apa()

save(kappa_scale,file = 'kappa_scale.RData')
d?save












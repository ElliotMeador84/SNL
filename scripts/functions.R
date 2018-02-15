
# Common libraries --------------------------------------------------------
    
    
    # library(tidyverse)
    # library(stringr)
    # library(ggplot2)
    # library(igraph)
    # library(tidycensus)
    # library(viridis)
    # library(sf)
    # library(Hmisc)
    # library(stringr)
# options(tigris_use_cache = T)

# General Functions -------------------------------------------------------


detach_package <- function(pkg, character.only = FALSE)
{
    if(!character.only)
    {
        pkg <- deparse(substitute(pkg))
    }
    search_item <- paste("package", pkg, sep = ":")
    while(search_item %in% search())
    {
        detach(search_item, unload = TRUE, character.only = TRUE)
    }
}

factorise <- function(x){
    x <- as.factor(as.character(x))
}

look <- function(x, y = 15){
    
    message('############################')
    message('############################')
    nm <-deparse(substitute(x))
    print(nm)
    message('-----------------')
    message('Number of variables')
    print(length(x))
    message('Number of rows')
    print(nrow(x))
    message('-----------------')
    is.even <- function(x) x %% 2 == 0
    
    if(is.even(length(x)) == F){
        x$pad <- 1:nrow(x)
    }
    
    
    k <- round(length(x)/2,0)
    l <- k+1
    y <- data_frame(
        Name = str_trunc(names(x)[1:k],y,'right',ellipsis = ""), #35
        Index = 1:k,                                                                #35
        Type = str_trunc(flatten_chr(map(x[1:k],class)),3,'right',ellipsis = ""),# 35
        Missing = flatten_dbl(map(x[1:k],function(p){sum(is.na(p))})), #35
        Name. = str_trunc(names(x)[l:length(x)],y,'right',ellipsis = ""), #35
        Index. = l:length(x) ,#35
        Type. = str_trunc(flatten_chr(map(x[l:length(x)],class)),3,'right',ellipsis = ""),#35
        Missing. = flatten_dbl(map(x[l:length(x)],function(p){sum(is.na(p))})))#35
    
    print(y,n = nrow(y))
    message('++++++++++++++++++++++++++++')
    message('++++++++++++++++++++++++++++')
}

'%!in%' <- function(x,y)!('%in%'(x,y))

clean <- function(x){
    x <- x %>% str_to_title()
    x <- x %>% str_trim()
    x <- gsub('\\.','',x)
    x <- gsub('\\,','',x)
}

clean_map <- function(x){
    x <- gsub(' County','',x)  
    x <- gsub('_',' ',x)  
    x <- x %>% str_to_lower()
    x <- x %>% str_trim()# trims off whitespace
    x <- gsub('\\.','',x)#removes periods
    x <- gsub('\\,','',x)#removes commas
    return(x)
}
rescale <- function(nchar,low,high){
    min_d <- min(nchar)
    max_d <- max(nchar)
    rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d) + low
    rscl
}


# Tidy helpers ------------------------------------------------------------


join.nrow <- function(x,y){
    i <- list()
    i$left <- left_join(x,y) %>% nrow
    i$right <- right_join(x,y) %>% nrow
    i$inner <- inner_join(x,y) %>% nrow
    i$anti <- anti_join(x,y) %>% nrow
    print(nrow(x))
    print(nrow(y))
    return(i)
}





# Network Analysis helpers ------------------------------------------------

to_source_target <- function(i.graph){
    
    a <- sapply(
        dplyr::as_data_frame(
            igraph::as_edgelist(i.graph)), 
        match, 
        table=unique(
            unlist(
                dplyr::as_data_frame(
                    igraph::as_edgelist(i.graph)))))
    
    b <- dplyr::as_data_frame(a) %>% rename(Source = V1,
                                            Target = V2)
    b
} # for use with Gephi, edgelist

to_source_target_labels <- function(i.graph){
    require(tidyverse,quietly = T)
    require(igraph,quietly = T)
    
    
    a <- sapply(
        dplyr::as_data_frame(
            igraph::as_edgelist(i.graph)), 
        match, 
        table=unique(
            unlist(
                dplyr::as_data_frame(
                    igraph::as_edgelist(i.graph)))))
    
    b <- dplyr::as_data_frame(a) %>% rename(Source = V1,
                                            Target = V2)
    
    
    c <- igraph::graph_from_data_frame(b)
    
    c <-b%>%
        gather(key,label) %>%
        select(label) %>%
        distinct()
    d<- dplyr::as_data_frame(as_edgelist(i.graph)) %>%
        gather(key,name) %>%
        select(name) %>%
        distinct()
    
   e <-  dplyr::data_frame(Id = c[[1]],
                      Label = d[[1]])
   
   f <- dplyr::bind_cols(igraph::get.vertex.attribute(i.graph))
    
   bind_cols(e,f)
}# for use with Gephi, node attributes

makemetrics <- function(gr) {
    data.frame(Degree=igraph::degree(gr), 
               Closeness = igraph::closeness(gr), 
               Betweenness = igraph::betweenness(gr))
}

rescale <- function(nchar,low,high){
    min_d <- min(nchar)
    max_d <- max(nchar)
    rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d) + low
    rscl
}


# Tidy Census Helper Functions --------------------------------------------


MO_ACS_Map <- function(vary,bins = 9,legend.title = vary,palette = 'Blues'){
    
    roundUp <- function(x,to=10){
        to*(x%/%to + as.logical(x%%to))
    }
    bin <- function(df_col,levs,rev = FALSE){
        require(Hmisc,quietly = T)
        require(forcats,quietly = T)
        s <- as.factor(as.character(as.numeric(cut2(df_col, g=levs))))
        if (rev == TRUE){
            s <- fct_rev(s)
        }
        s
    }# bins
    
    m <- MO_acs %>%
        filter(variable == vary) %>% 
        select(estimate)
    m[[1]]
    g <- legend.labeler(m[[1]],bins)
    
    MO_acs %>%
        filter(variable == vary) %>% 
        ggplot(aes(fill = bin(estimate,bins,T))) + 
        geom_sf() + 
        scale_fill_brewer(palette = palette,
                          name = legend.title,
                          labels = g)+
        scale_color_brewer(palette = palette)+
        ggtitle(vary)+
        
        theme_blank()
} #Creates MO Maps
legend.labeler <- function(df_col,levs){
    a <- as.character(cut2(df_col, g=levs)) %>% unique
    a <- gsub('\\[','',a)
    a <- gsub('\\]','',a)
    a <- gsub(' ','',a)
    a <- gsub('\\)','',a)
    a <- gsub('\\,',' ',a)
    a <- data_frame(a)
    a <- as_data_frame(as.matrix(str_split_fixed(a$a, ' ',2)))
    a <- map_df(a,as.numeric)
    a <- a %>% arrange(V1)
    a[a == min(a)] <- 0
    a <- map_df(a,function(x){
        k <- roundUp(x,to = 1000)
        prettyNum(k,big.mark = ',')})
    
    c <- str_c(a[[1]],a[[2]],sep =  ' - ')
    c
}
roundUp <- function(x,to=10){
    to*(x%/%to + as.logical(x%%to))
}
bin <- function(df_col,levs,rev = FALSE){
    require(Hmisc,quietly = T)
    require(forcats,quietly = T)
    s <- as.factor(as.character(as.numeric(cut2(df_col, g=levs))))
    if (rev == TRUE){
        s <- fct_rev(s)
    }
    s
}# bins
american_survey <- function(state.abbrev,vary){
    tidycensus::get_acs('county',variables = vary, state = state.abbrev,geometry  = TRUE)
} #grabs data
theme_blank <- function() {theme(panel.border = element_blank(),
                                 legend.key = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.text.x = element_blank(),
                                 panel.grid = element_blank(),
                                 panel.grid.minor = element_blank(), 
                                 panel.grid.major = element_line(colour = 'transparent'),
                                 panel.background = element_blank(),
                                 plot.background = element_rect(fill = "transparent",colour = NA))
} # adds a blank theme, in MO_ACS_Map

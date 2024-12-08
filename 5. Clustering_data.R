cluster_viz <- function(doi, num){
  set.seed(100)
  nodes_1_cluster <- random_walk(sampled_graph, doi, 300)
  nodes_1_cluster <- unique(nodes_1_cluster)
  cluster_1 <- induced_subgraph(sampled_graph, vids = nodes_1_cluster)
  
  start_idx <- which(V(cluster_1)$name == doi)
  ### Color
  # 노드 속성 설정
  V(cluster_1)$color <- rgb(250/255, 250/255, 250/255, alpha = 0.5)  
  V(cluster_1)$size <- 3         
  
  # 강조 노드 색상과 크기 변경
  V(cluster_1)$color[start_idx] <- rgb(178/255, 34/255, 34/255, alpha = 1)
  V(cluster_1)$size[start_idx] <- 7
  
  ### Label
  # Default
  V(cluster_1)$label <- NA
  
  # Highlight
  V(cluster_1)$label[start_idx] <- num
  
  #Viz
  set.seed(100)
  plot(cluster_1,
       layout = layout_with_fr(cluster_1),
        vertex.label.color = "white",  # 라벨 색상
        vertex.label.cex = 1,                
        edge.width = 1,                   
        edge.color = rgb(105/255, 105/255, 105/255, alpha = 0.7),
        edge.arrow.size = 0.1)
  
  csv = nodes_sample[nodes_sample$doi %in% nodes_1_cluster,]
  
  return(csv)
  
}
target_label

nodes_1 <- "10.1038/nature19791"
nodes_2 <- "10.1073/pnas.71.10.4135"
nodes_142 <- "10.1038/nrm2203"
nodes_154 <- "10.1126/science.1060612" 
nodes_166 <- "10.1007/bf02084158"
nodes_221 <- "10.1146/annurev.biochem.73.011303.073723"
nodes_247 <- "10.1038/270301a0"
nodes_279 <- "10.1371/journal.pone.0015004"

# 1
cluster_1_dataframe <- cluster_viz(nodes_1, 1)
write.csv(cluster_1_dataframe, 'cluster1.csv')

# 2
cluster_2_dataframe <- cluster_viz(nodes_2, 2)
write.csv(cluster_2_dataframe, 'cluster2.csv')

# 142
cluster_142_dataframe <- cluster_viz(nodes_142, 142)
write.csv(cluster_142_dataframe, 'cluster142.csv')

# 154
cluster_154_dataframe <- cluster_viz(nodes_154, 154)
write.csv(cluster_154_dataframe, 'cluster154.csv')

# 166
cluster_166_dataframe <- cluster_viz(nodes_166, 166)
write.csv(cluster_166_dataframe, 'cluster166.csv')

# 221
cluster_221_dataframe <- cluster_viz(nodes_221, 221)
write.csv(cluster_221_dataframe, 'cluster221.csv')

# 247
cluster_247_dataframe <- cluster_viz(nodes_247, 247)
write.csv(cluster_247_dataframe, 'cluster247.csv')

# 279
cluster_279_dataframe <- cluster_viz(nodes_279, 279)
write.csv(cluster_279_dataframe, 'cluster279.csv')




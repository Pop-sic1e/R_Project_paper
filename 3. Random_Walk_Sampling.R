## 그래프 데이터 생성
g <- graph_from_data_frame(d = edges, vertices = nodes, 
                           directed = TRUE)

### Random Walk 알고리즘
random_walk <- function(graph, start_node, steps) {
  visited_nodes <- c() # 방문한 노드를 저장할 벡터
  current_node <- start_node
  
  for (i in 1:steps) {
    visited_nodes[i] <- current_node
    neighbors <- neighbors(graph, current_node) # 현재 노드의 이웃 노드 가져오기
    if (length(neighbors) == 0) {
      neighbors <- neighbors(graph, start_node)
    }
    current_node <- sample(as_ids(neighbors), 1)# 무작위로 다음 노드 선택
    
    if (i %% 100 == 0) {
      print(i)
      }
      
  }
  return(visited_nodes) # 방문한 노드 리스트 반환
}



### Random Walk 실행
best_deg_doi <- head(merged_nodes[order(merged_nodes$degree, decreasing = TRUE), c('doi')], 7)
start_node1 <- '10.1038/nature19791' 
start_node2 <- '10.1073/pnas.71.10.4135'

  # Starting Nodes Vector 생성 
best_deg_doi <- unique(c(best_deg_doi, start_node1, start_node2))
steps <- 300 # 이동할 스텝 수

  # Random Walk Loop
sample_nodes <- c()
for (i in 1:length(best_deg_doi)){
  cat('***', i, '***\n')
  result <- random_walk(g, best_deg_doi[i], steps)
  sample_nodes <- append(sample_nodes, result)
}

sample_nodes <- unique(sample_nodes)
length(sample_nodes)


### 샘플링된 노드를 기반으로 부분 그래프 생성
sampled_graph <- induced_subgraph(g, vids = sample_nodes)


#### Check Plot
plot(sampled_graph,
     layout = layout_with_fr(sampled_graph),
     vertex.color = rgb(178/255, 34/255, 34/255, alpha = 0.7),
     vertex.size = 1,                  
     vertex.label = NA,                
     edge.width = 0.7,                   
     edge.color = rgb(105/255, 105/255, 105/255, alpha = 0.7),
     edge.arrow.size = 0.1) 


plot(sampled_graph,
     layout = layout_with_fr(sampled_graph),
     vertex.color = rgb(178/255, 34/255, 34/255, alpha = 0.7),
     vertex.size = 1,                  
     vertex.label = NA,                
     edge.width = 0.7,                   
     edge.color = rgb(105/255, 105/255, 105/255, alpha = 0.7),
     edge.arrow.size = 0.1) 


#write.csv(as.data.frame(sample_nodes), 'sample_nodes.csv')


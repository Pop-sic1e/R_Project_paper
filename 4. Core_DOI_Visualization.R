### 하이라이트 노드 라벨 추출
target_label <- c(best_bet_doi)
highlight_idx <- which(V(sampled_graph)$name %in% target_label)[3:length(target_label)]
start_idx <- which(V(sampled_graph)$name %in% c(start_node1, start_node2))

### Color
  # 노드 속성 설정
V(sampled_graph)$color <- rgb(250/255, 250/255, 250/255, alpha = 0.3)  
V(sampled_graph)$size <- 1.5         

  # 강조 노드 색상과 크기 변경
V(sampled_graph)$color[highlight_idx] <- rgb(0/255, 0/255, 205/255, alpha = 1)
V(sampled_graph)$size[highlight_idx] <- 5

  # 강조 노드 색상과 크기 변경
V(sampled_graph)$color[start_idx] <- rgb(178/255, 34/255, 34/255, alpha = 1)
V(sampled_graph)$size[start_idx] <- 5


### Label
  # Default
V(sampled_graph)$label <- NA

  # Highlight
V(sampled_graph)$label[highlight_idx] <- highlight_idx
V(sampled_graph)$label[start_idx] <- start_idx



### Viz
par(mar = c(0,0,0,0))
set.seed(100)
plot(sampled_graph,
     layout = layout_with_fr(sampled_graph),
     vertex.label.color = "white",  # 라벨 색상
     vertex.label.cex = 0.6,
     vertex.label.dist = 0,
     edge.width = 0.2,                   
     edge.arrow.size = 0.15) 


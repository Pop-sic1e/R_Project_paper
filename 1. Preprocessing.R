library(igraph)
setwd('/Users/noh/Desktop/경희대학교/3-1/R프로그래밍/citiation_network/final')


### Data Load
edges1 <- read.csv('edges_final.csv')
edges2 <- read.csv('edges_final_2.csv')
nodes1 <- read.csv('nodes_final.csv')
nodes2 <- read.csv('nodes_final_2.csv')

edges <- rbind(edges1, edges2)
nodes <- rbind(nodes1, nodes2)


### 중복 엣지제거
edges <- unique(edges)
edges <- edges[edges$cited != edges$citing, ]

### 중복 노드 확인 
  # Edge 데이터 Unique DOI
cited_doi <- unique(edges$cited)
citing_doi <- unique(edges$citing)
edge_doi_vec <- unique(c(cited_doi, citing_doi))
length(edge_doi_vec)

  # Node 데이터 doi 추출
nodes_extract_doi <- unique(nodes$doi)
length(nodes_extract_doi)

  # node 데이터에 중복이 있음
length(nodes_extract_doi) - dim(nodes)[1]

  # 중복된 행 확인
duplicated_rows <- nodes[duplicated(nodes$doi), ]
dim(duplicated_rows)

  # 중복 제거
nodes <- nodes[!duplicated(nodes$doi), ]




### 그래프 데이터 생성
g <- graph_from_data_frame(d = edges, vertices = nodes, 
                           directed = TRUE)


#### 네트워크 전체 구조 시각화 
par(mar = c(0,0,0,0))
set.seed(100)

### Color
  # 노드 속성 설정
V(g)$color <- rgb(250/255, 250/255, 250/255, alpha = 0.3)  
V(g)$size <- 1       

plot(g,
     layout = layout_with_lgl(g),
     vertex.label = NA,                
     edge.width = 0.7,                   
     edge.color = rgb(105/255, 105/255, 105/255, alpha = 0.7),
     edge.arrow.size = 0.1)  



### 네트워크: 랜덤워크 시작 지점 
### Color
### 하이라이트 노드 라벨 추출
target_label <- c(best_bet_doi)
highlight_idx <- which(V(g)$name %in% target_label)[3:length(target_label)]
start_idx <- which(V(g)$name %in% c(start_node1, start_node2))

### Color
  # 노드 속성 설정
V(g)$color <- rgb(250/255, 250/255, 250/255, alpha = 0)  
V(g)$size <- 2    

  # 강조 노드 색상과 크기 변경
V(g)$color[highlight_idx] <- rgb(0/255, 0/255, 205/255, alpha = 1)
V(g)$size[highlight_idx] <- 5

  # 강조 노드 색상과 크기 변경
V(g)$color[start_idx] <- rgb(178/255, 34/255, 34/255, alpha = 1)
V(g)$size[start_idx] <- 5

  # Viz
par(mar = c(0,0,0,0))
set.seed(100)
plot(g,
     layout = layout_with_lgl(g),
     vertex.label = NA,   
     vertex.frame.color = rgb(105/255, 105/255, 105/255, alpha = 0.3),
     edge.width = 0.7,                   
     edge.color = rgb(105/255, 105/255, 105/255, alpha = 0.7),
     edge.arrow.size = 0.1)  


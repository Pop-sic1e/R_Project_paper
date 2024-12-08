### 전체 데이터Centrality 구하기
degree_data <- degree(g, mode = "all")
bet_data <- betweenness(g)
cls_data <- closeness(g)

c_df <- (data.frame(degree = degree_data,
                    betweenness = bet_data,
                    closeness = cls_data))
c_df$doi <- rownames(c_df)
rownames(c_df) <- NULL
c_df <- c_df[ ,c('doi', 'degree','betweenness','closeness')]


  # 두 데이터 프레임 병합
merged_nodes <- merge(nodes, c_df, by = "doi")
print(merged_nodes)
colnames(merged_nodes)

best_deg_df <- as.data.frame(head(merged_nodes[order(merged_nodes$degree, decreasing = TRUE), c('doi', 'title', 'degree')], 7))
View(best_deg_df)
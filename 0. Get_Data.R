# 필요 패키지 설치 및 호출
#install.packages("httr")
#install.packages("jsonlite")
library(httr)
library(jsonlite)
library(rcrossref)


# 인용 데이터 호출 함수
call_API <- function(base_url, doi){
  base_url = base_url
  doi = doi
  # API 요청
  response <- GET(url = paste0(base_url, doi))
  if (status_code(response) == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    citation_data <- fromJSON(content)
    # 인용 관계 데이터 출력 및 데이터 프레임 제작
    if (length(citation_data) > 0) {
      citation_df <- data.frame(
        ## Network - Edge 데이터 구조상 cited를 앞으로 하는게 좋을 듯
        cited = citation_data$cited,
        citing = citation_data$citing,
        stringsAsFactors = FALSE
      )
    } else {
      message("No citation data found for DOI: ", doi)
      return(NULL)
    }
    return(citation_df)
  }
}

# 노드 데이터 제작 함수
make_node <- function(node_vector){

    # 노드 데이터를 위한 빈 데이터 프레임 형성
    nodes <- data.frame(
    doi = character(),
    container.title = character(),
    title = character(),
    author = character(),
    type = character(),
    publisher = character(),
    stringsAsFactors = FALSE
    )

    # 메타 데이터 추출 및 저장
    for (i in seq_along(node_vector)) {
    tryCatch({
        cat("Processing", i, "of", length(node_vector), ":", node_vector[i], "\n") # 과정 출력
        result <- cr_works(dois = node_vector[i]) #데이터 호출
        # 있는 데이터 프레임에 넣기
        if (!is.null(result$data)) {
            row <- data.frame(
                doi = result$data$doi,
                container.title = result$data$'container.title',
                title = result$data$title,
                author = unlist(result$data$author),
                type = result$data$type,
                publisher = result$data$publisher,
                stringsAsFactors = FALSE
            )
            nodes <- rbind(nodes, row)
            }
        }, error = function(e) {
            cat("Error with DOI", node_vector[i], ":", e$message, "\n")
        })
    }

    # author이 데이터 프레임 속 list로 들어가 한 그냥 문자로 바꾸기
    nodes$author <- sapply(nodes$author, function(authors) {
        if (is.null(authors)) {
            return(NA)
        }
        paste(authors, collapse = "; ")
    })
    return(nodes)
}


base_url <- "https://opencitations.net/index/coci/api/v1/citations/" # 링크 앞부분




# 첫번째 사이클 데이터 추출
doi_vec = list("10.1007/978-3-642-35289-8_32", "10.1038/nature19791", "10.1073/pnas.71.10.4135") # 기준이 되는 논문 3개
# 논문 인용 데이터 추출 및 csv 변환
data_list <- list()
for (i in 1:length(doi_vec)) {
  doi <- doi_vec[i]
  new_row <- call_API(base_url, doi)
  data_list[[i]] <- new_row
}
edges <- do.call(rbind, data_list)
write.csv(edges, file='./data/edges.csv')

# edges 데이터에 존재하는 모든 doi 벡터 생성
edges <- read.csv('./data/edges.csv') # Rstudio 용량 문제로 필요하면 전부 삭제 하면, 다시 불러오기를 위한 코드
cited_doi <- unique(edges$cited)
citing_doi <- unique(edges$citing)
node_doi_vec <- unique(c(cited_doi, citing_doi))

# nodes 메타데이터 추출
nodes <- make_node(node_doi_vec)
# Save the data.frame to a CSV file
write.csv(nodes, file = "./data/nodes.csv", row.names = FALSE)


# DOI 정리 - 첫번째 사이클
# 데이터 불러오기
edges <- read.csv('./data/edges.csv')[, 2:3]  # Index 제거
nodes <- read.csv('./data/nodes.csv')
nodes <- nodes[nodes$type == 'journal-article', ]  # Journal만 추출
nodes <- nodes[!duplicated(nodes$doi), ]  # 중복 제거

# DOI 추출
edge_doi_vec <- unique(c(edges$cited, edges$citing))  # Edge의 DOI
nodes_doi <- unique(nodes$doi)  # Node의 DOI

# Edge에서 Node에 없는 DOI 제거
edges <- edges[edges$citing %in% nodes_doi & edges$cited %in% nodes_doi, ]

# Node에서 Edge에 없는 DOI 제거
edge_doi_vec <- unique(c(edges$cited, edges$citing))  # 다시 Edge DOI 계산
nodes <- nodes[nodes$doi %in% edge_doi_vec, ]

# 최종 검증: Node와 Edge DOI 일치 확인
stopifnot(length(unique(nodes$doi)) == length(unique(c(edges$cited, edges$citing))))

# 결과 저장
write.csv(nodes, file = './data/nodes_final.csv', row.names = FALSE)
write.csv(edges, file = './data/edges_final.csv', row.names = FALSE)
cat("Processing completed.\n")




# 데이터 지우고 할 경우
doi_vec <- read.csv('./data/edges_final.csv')
doi_vec <- unique(doi_vec[,2])

# 논문 인용 데이터 추출 및 csv 변환
data_list <- list()
for (i in 1:length(doi_vec)) {
  doi <- doi_vec[i]
  new_row <- call_API(base_url, doi)
  data_list[[i]] <- new_row
}
edges <- do.call(rbind, data_list)
write.csv(edges, file='./data/edges2.csv')

# edges 데이터에 존재하는 모든 doi 벡터 생성
edges <- read.csv('./data/edges2.csv') # Rstudio 용량 문제로 필요하면 전부 삭제 하면, 다시 불러오기를 위한 코드
cited_doi <- unique(edges$cited)
citing_doi <- unique(edges$citing)
node_doi_vec <- unique(c(cited_doi, citing_doi))

# nodes 메타데이터 추출
nodes <- make_node(node_doi_vec)
# Save the data.frame to a CSV file
write.csv(nodes, file = "./data/nodes2.csv", row.names = FALSE)


# DOI 정리 - 두번째 사이클
# 데이터 불러오기
edges <- read.csv('./data/edges2.csv')[, 2:3]  # Index 제거
nodes <- read.csv('./data/nodes2.csv')
nodes <- nodes[nodes$type == 'journal-article', ]  # Journal만 추출
nodes <- nodes[!duplicated(nodes$doi), ]  # 중복 제거

# DOI 추출
edge_doi_vec <- unique(c(edges$cited, edges$citing))  # Edge의 DOI
nodes_doi <- unique(nodes$doi)  # Node의 DOI

# Edge에서 Node에 없는 DOI 제거
edges <- edges[edges$citing %in% nodes_doi & edges$cited %in% nodes_doi, ]

# Node에서 Edge에 없는 DOI 제거
edge_doi_vec <- unique(c(edges$cited, edges$citing))  # 다시 Edge DOI 계산
nodes <- nodes[nodes$doi %in% edge_doi_vec, ]

# 최종 검증: Node와 Edge DOI 일치 확인
stopifnot(length(unique(nodes$doi)) == length(unique(c(edges$cited, edges$citing))))

# 결과 저장
write.csv(nodes, file = './data/nodes_final_2.csv', row.names = FALSE)
write.csv(edges, file = './data/edges_final_2.csv', row.names = FALSE)
cat("Processing completed.\n")
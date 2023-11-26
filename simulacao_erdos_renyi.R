###################### SIMULACAO GRAFO DE ERDOS-RENYI #########################

library(dplyr)
library(igraph)

simulacao_erdos_renyi <- function(vert_n, p = 0.5) {
  
  matriz <- c() # criacao para a representação em matriz
  
  set.seed(11218195) # escolho uma semente para os numeros aleatorios
  
  for (i in c(1:vert_n)) {
    
    # rodo uma bernoulli(p) para cada combinação dois a dois de arestas, para v > v', 
    # para decidir se eles vão ter ligacao
    tem_aresta_i <- c(rep(x = 0, times = i), rbinom(n = vert_n-i, size = 1, prob = p))
    
    # concateno as linhas na matriz
    matriz <- cbind(matriz, tem_aresta_i)
    
  }
  
  # transformo a matriz num data frame e renomeio as colunas
  matriz <- matriz %>% data.frame() %>% `colnames<-`(c(1:vert_n))
  
  # pego os veretices que possuem uma aresta entre elas
  pares_com_aresta <- which(matriz == 1, arr.ind = TRUE) %>% data.frame()
  
  # coloco num vetor as arestas de forma que seja possivel plotar o grafo com a funcao graph
  vetor_pares <- c()
  
  for (i in c(1:length(pares_com_aresta[,1]))) {
    
    vetor_pares <- append(append(vetor_pares, pares_com_aresta[i, 1]), pares_com_aresta[i, 2])
    
  }
  
  # criação do grafo
  grafo <- graph(edges=vetor_pares, n=vert_n, directed=FALSE)
  
  # Atribuindo cores às arestas
  cores_arestas <- rainbow(vert_n)
  E(grafo)$color <- cores_arestas

  return(list(
    M_matriz = matriz,
    G_grafo = grafo
  ))
  
}

simulacao <- simulacao_erdos_renyi(vert_n = 10, p = 0.3)
cat('Matriz da simulação do Grafo Erdos-Renyi \n', '--------------------')
print.data.frame(simulacao$M_matriz)

grau_arestas <- (simulacao$M_matriz %>% colSums()) + (simulacao$M_matriz %>% t() %>% colSums())

grau_total <- sum(teste+teste2)

# plota o grafico
plot(simulacao$G_grafo, layout=layout_in_circle)

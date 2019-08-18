library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

edge_list

letters <- read_csv("https://raw.githubusercontent.com/jessesadler/intro-to-r/master/data/correspondence-data-1585.csv")

sources <-  letters %>%
  distinct(source) %>%  
  rename(label = source)

destination <- letters %>% 
  distinct(destination) %>% 
  rename(label = destination)

nodes <- full_join(sources, destination, by = "label") %>%
  arrange(label) %>% 
  rowid_to_column("id")

per_route <- letters %>% 
  group_by(source, destination) %>% 
  summarise(weight = n()) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

## network package
library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3)

plot(routes_network, vertex.cex = 3, mode = "circle")

## igraph
detach(package:network)
rm(routes_network)
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

routes_igraph

plot(routes_igraph, edge.arrow.size = 0.2)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)


library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = T)

class(routes_tidy)

routes_tidy

# Labelled graph
ggraph(routes_tidy) +
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

# Arc diagram
ggraph(routes_tidy, layout = "linear") +
  geom_edge_arc(aes(width = weight), alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

# visNetwork
library(visNetwork)

visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

# networkD3
library(networkD3)

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

# Classic network
forceNetwork(Links = edges_d3,
             Nodes = nodes_d3,
             Source = "from",
             Target = "to",
             NodeID = "label",
             Group = "id",
             Value = "weight",
             opacity = 1,
             fontSize = 16,
             zoom = TRUE)

# Sankey network
sankeyNetwork(Links = edges_d3,
              Nodes = nodes_d3,
              Source = "from",
              Target = "to",
              NodeID = "label",
              Value = "weight",
              fontSize = 16,
              units = "Letter(s)")


# data.table --------------------------------------------------------------

library(data.table)

lettersDT = as.data.table(letters)

sourcesDT = lettersDT[, .(label = unique(source))]
destinationDT = lettersDT[, .(label = unique(destination))]
nodesDT = merge(sourcesDT, destinationDT, by = "label", all = TRUE)
nodesDT = nodesDT[, .(id = .I, label)]

per_routeDT = lettersDT[, .(weight = .N), by = .(source, destination)]

edgesDT = per_routeDT[nodesDT, on = "source==label", nomatch = 0]
setnames(edgesDT, "id", "from")
edgesDT = edgesDT[nodesDT, on = "destination==label", nomatch = 0]
setnames(edgesDT, "id", "to")
edgesDT = edgesDT[order(from), .(from, to, weight)]




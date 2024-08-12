# Proyecto

#-------------------------------------------------------------------------------

# Importacion de la base de datos

library(readxl)

Base <- read_excel("/Users/annamaria_saavedra//Downloads/Datos proyecto.xlsx", 
                        sheet = "Aristas")

#-------------------------------------------------------------------------------

# Creacion de la red de comercio binaria

library(igraph)

Red <- graph_from_data_frame(d = Base[c(1,2)],
                                  directed = TRUE)

#-------------------------------------------------------------------------------

# Analisis exploratorio

## Conceptos fundamentales de teoria de grafos

# Distribucion de la distancia geodesica de la red binaria

Distancia <- distance_table(Red)$res; names(Distancia) <- 1:length(Distancia)

barplot(prop.table(Distancia), xlab = "Distancia geodésica", 
        ylab = "Frecuencia relativa", border = "deepskyblue3", 
        col = adjustcolor("deepskyblue3", 0.2), main = "",
        ylim = c(0, 0.4))

# Distancia geodesica de la red ponderada

E(Red)$weight <- Base$Valor

Red_no_dirigida <- as.undirected(Red, mode = "collapse", 
                                 edge.attr.comb = "sum")

Distancia <- distances(graph = Red_no_dirigida, v = V(Red_no_dirigida), 
                       to = V(Red_no_dirigida), algorithm = "dijkstra")

#-------------------------------------------------------------------------------

## Caracterizacion de vertices

# Grado

Grado <- degree(Red, mode = "all")

# Grado de entrada

Grado_entrada <- degree(Red, mode = "in")

head(sort(Grado_entrada, decreasing = TRUE), n = 5)

# Grado de salida

Grado_salida <- degree(Red, mode = "out")

head(sort(Grado_salida, decreasing = TRUE), n = 5)

# Distribucion del grado

Distribucion <- degree_distribution(Red)

plot((0:max(Grado_salida))[Distribucion != 0], 
     Distribucion[Distribucion != 0], log = "xy", pch = 16, 
     col = adjustcolor("deeppink4", 0.5),
     xlab = "Logaritmo natural del grado de salida", ylab = "Log-densidad")

# Fuerza de entrada

Fuerza_entrada <- strength(Red, mode = "in")

head(sort(Fuerza_entrada, decreasing = TRUE), n = 5)

# Fuerza de salida

Fuerza_salida <- strength(Red, mode = "out")

head(sort(Fuerza_salida, decreasing = TRUE), n = 5)

# Fuerza

Fuerza <- strength(Red, mode = "all")

# Centralidad de cercania

Distancias <- distances(graph = Red)

head(sort(closeness(graph = Red, vids = V(Red), mode = "all", normalized = FALSE),
          decreasing = TRUE), n = 5)

# Centralidad de intermediacion

head(sort(betweenness(graph = Red, v = V(Red), directed = TRUE,
                      normalized = FALSE), decreasing = TRUE), n = 5)

#-------------------------------------------------------------------------------

## Asortatividad

Caracteristicas_nodales <- read_excel("/Users/annamaria_saavedra//Downloads/Datos proyecto.xlsx", 
                                      sheet = "Atributos_nodos")

V(Red)$PIB <- Caracteristicas_nodales$PIB

V(Red)$`Energia renovable` <- Caracteristicas_nodales$Energia

V(Red)$`Riesgo politico` <- Caracteristicas_nodales$Riesgo

V(Red)$Region <- Caracteristicas_nodales$Region

V(Red)$Idioma <- Caracteristicas_nodales$Idioma

# Caracteristicas categoricas

# Europa y Asia Central

V_Tipo1 <- (V(Red)$Region == "Europe & Central Asia") + 1

V_Tipo1[is.na(V_Tipo1)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo1, directed = TRUE,
                      normalized = TRUE)

# Asia del Este y Pacifico

V_Tipo2 <- (V(Red)$Region == "East Asia & Pacific") + 1

V_Tipo2[is.na(V_Tipo2)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo2, directed = TRUE,
                      normalized = TRUE)

# America Latina y el Caribe

V_Tipo3 <- (V(Red)$Region == "Latin America & Caribbean") + 1

V_Tipo3[is.na(V_Tipo3)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo3, directed = TRUE,
                      normalized = TRUE)

# Oriente Medio y Africa del Norte

V_Tipo4 <- (V(Red)$Region == "Middle East & North Africa") + 1

V_Tipo4[is.na(V_Tipo4)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo4, directed = TRUE,
                      normalized = TRUE)

# America del Norte

V_Tipo5 <- (V(Red)$Region == "North America") + 1

V_Tipo5[is.na(V_Tipo5)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo5, directed = TRUE,
                      normalized = TRUE)

# Asia del Sur

V_Tipo6 <- (V(Red)$Region == "South Asia") + 1

V_Tipo6[is.na(V_Tipo6)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo6, directed = TRUE,
                      normalized = TRUE)

# Africa Subsahariana

V_Tipo7 <- (V(Red)$Region == "Sub-Saharan Africa") + 1

V_Tipo7[is.na(V_Tipo7)] <- 1

assortativity_nominal(graph = Red, types = V_Tipo7, directed = TRUE,
                      normalized = TRUE)

# Caracteristicas numericas

assortativity(graph = Red, values = V(Red)$PIB, values.in = NULL, directed = TRUE,
              normalized = FALSE)

assortativity(graph = Red, values = V(Red)$`Energia renovable`, values.in = NULL, 
              directed = TRUE, normalized = FALSE)

assortativity(graph = Red, values = V(Red)$`Riesgo politico`, values.in = NULL, directed = TRUE,
              normalized = FALSE)

#-------------------------------------------------------------------------------

## Caracterizacion de conectividad

# Frecuencia de clanes

table(sapply(X = cliques(graph = Red), FUN = length))

# Clan maximal

max_cliques(graph = Red)

# Clan maximo

largest_cliques(graph = Red)

# Numero clan

clique_num(graph = Red)

# Densidad

edge_density(Red)

# Transitividad global

transitivity(Red, type = "global")

# Puntos de articulacion

articulation_points(Red)

#-------------------------------------------------------------------------------

## Agrupamiento

# Algoritmo fast-greedy

Red_no_dirigida <- as.undirected(Red, mode = "collapse", edge.attr.comb = "sum")

Fast_greedy <- cluster_fast_greedy(Red_no_dirigida)

# Visualizacion

l <- layout_with_fr(Red)

l <- norm_coords(l, ymin = -1.2, ymax = 1.2, xmin = -1.25, xmax = 1.25)

library(RColorBrewer)

Color <- c(brewer.pal(9,"Set1")[1:9], brewer.pal(8,"Set2")[1:7])

E(Red)$width <- 2



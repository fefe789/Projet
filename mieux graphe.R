#install.packages('igraph')
library(igraph)

# on prend L'algorithme de Kamada-Kawai est une méthode de placement de graphes qui vise à 
# positionner les nœuds d'un graphe dans un espace 2D ou 3D de manière à minimiser la différence 
# entre les distances géométriques des nœuds et leurs distances théoriques dans le graphe

plot_carte <- function(distance_matrix) {
  graphe <- graph_from_adjacency_matrix(distance_matrix, mode = "undirected", weighted = TRUE)
  layout <- layout_with_kk(graphe)  # ca fait des meilleurs paquets
  return(list(graphe,layout))}




plot_carte_type_methode <- function(graphelayout, données, titre) { # plot les élections avec en légende le type de générations
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  # Extraire les méthodes de la données
  methodes <- sapply(données, function(item) item$methode)
  
  # Associer des couleurs aux méthodes
  #couleurs <- rainbow(length(unique(methodes)))
  #couleurs <- c("black", "darkgrey", "blue", "red", "green", "purple", "orange", "pink", "brown", "cyan", "magenta")
  couleurs <- c("black", "darkgrey", "navy", "firebrick", "forestgreen", "gold", "#4B0082", "#008080", "darkorange", "deeppink", "darkviolet")
  
  # Exemple d'utilisation dans un plot

  noms_couleurs <- setNames(couleurs, unique(methodes))
  
  
  # Définir la taille des nœuds
  vertex_size <- rep(5, length(methodes))  # Taille par défaut
  
  # Identifier les indices des méthodes noir et gris foncé
  #indices_gros_points <- which(methodes %in% names(noms_couleurs)[noms_couleurs %in% c("black", "darkgrey")])
  
  # Ajuster la taille pour les nœuds identifiés
  #vertex_size[indices_gros_points] <- 10  
 # Tracer uniquement les nœuds avec les couleurs associées aux méthodes
  
  plot(graphe, layout = layout, vertex.label = "", vertex.size = vertex_size,
       vertex.color = noms_couleurs[methodes], edge.color = "white", edge.width = 0, main = titre)
  
  
  # Créer la légende avec une taille de texte réduite

  plot.new()
  legend("center", legend = names(noms_couleurs), col = couleurs, pch = 19, title = "Méthodes", bty = "n", cex = 0.8)
  
}

données[[i]]$methode


plot_carte_resultat <- function(graphelayout, données) {# plot les élections avec en légende le nombre de gagnant différent
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  
  n <- length(données)
  l <- c()
  
  titre <- paste("Nombre de gagnants différents avec 8 méthodes")
  
  for (i in 1:n) {
    # Comparer les résultats des deux méthodes spécifiées
    l <- c(l, length(unique(données[[i]][-(1:5)])))
  }
  a<-max(l)
  # Normaliser les valeurs de l pour qu'elles soient entre 0 et 1
  l_normalized <- (l - min(l)) / (max(l) - min(l))
  
  # Créer une palette de couleurs allant du rouge au vert
  colors <- colorRampPalette(c("green", "red"))(a)
  
  # Assigner une couleur à chaque valeur de l
  vertex_colors <- colors[round(l_normalized * (a-1)) + 1]
  
  # Tracer le graphe
  plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
       vertex.color = vertex_colors, edge.arrow.size = 0, edge.width = 0, edge.color = "white", main = titre)
  
  # Ajouter une légende avec un dégradé de couleurs
  legend("topright", legend = 1:a, col = colors, pch = 19, title = "Valeurs")
}







bor<-plot_carte(distance_borda)
pos<-plot_carte(distance_pos)



plot_carte_type_methode(bor, données, "Carte pour la Distance de Borda")
plot_carte_resultat(bor, données)


plot_carte_type_methode(pos, données, "Carte pour la Distance positionnelle")

plot_carte_resultat(pos, données)






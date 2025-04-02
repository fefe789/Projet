#install.packages('igraph')

library(igraph)

plot_carte_type_methode <- function(graphelayout, données, titre) {
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  # Extraire les méthodes de la données
  methodes <- sapply(données, function(item) item$methode)
  
  # Associer des couleurs aux méthodes
  couleurs <- rainbow(length(unique(methodes)))
  noms_couleurs <- setNames(couleurs, unique(methodes))
  
  # Tracer uniquement les nœuds avec les couleurs associées aux méthodes
  
  plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
       vertex.color = noms_couleurs[methodes], edge.color = "white", edge.width = 0, main = titre)
  
  
  # Créer la légende avec une taille de texte réduite
  
  plot.new()
  legend("center", legend = names(noms_couleurs), col = couleurs, pch = 19, title = "Méthodes", bty = "n", cex = 0.8)
  
}



logborda <- log1p(distance_borda)
logpos <- log1p(distance_pos)
logpair <- log1p(distance_pair)

# on prend L'algorithme de Kamada-Kawai est une méthode de placement de graphes qui vise à 
# positionner les nœuds d'un graphe dans un espace 2D ou 3D de manière à minimiser la différence 
# entre les distances géométriques des nœuds et leurs distances théoriques dans le graphe
# la norme ca cahnge pasgrand chose
# le log ca donne une forme de cercle,mais ca fait des point plus équidistant donc on voit moins les paquets pas mal pour borda pour voir

bor<-plot_carte(distance_borda)

pos<-plot_carte(distance_pos)

pair<-plot_carte(distance_pair)



plot_carte_type_methode(bor[[1]],bor[[2]], données, "Carte pour distance_borda")

plot_carte_type_methode(pos[[1]],pos[[2]], données, "Carte pour distance_pos")

plot_carte_type_methode(pair[[1]],pair[[2]], données, "Carte pour distance_pair")




plot_carte_comparaison_resultat <- function(graphelayout, données, methode1_index, methode2_index) {
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  
  n <- length(données)
  l <- c()
  
  # Extraire les noms des méthodes à comparer
  methodes <- names(données[[1]])[-(1:5)]
  methode1 <- methodes[methode1_index]
  methode2 <- methodes[methode2_index]
  
  titre <- paste("Comparaison entre", methode1, "et", methode2)
  
  for (i in 1:n) {
    # Comparer les résultats des deux méthodes spécifiées
    l <- c(l, données[[i]][[methode1]] == données[[i]][[methode2]])
  }
  
  # Tracer le graphe

    plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
         vertex.color = ifelse(l, "green", "red"), edge.arrow.size = 0, edge.width = 0, edge.color = "white", main = titre)

  
  # Créer la légende avec une taille de texte réduite

    plot.new()
    legend("center", legend = c("Identique", "Différent"), col = c("green", "red"), pch = 19, title = "Comparaison", bty = "n", cex = 0.8)

  
}

plot_carte_comparaison_resultat(pos, données, 8, 9)




plot_carte_resultat <- function(graphelayout, données) {
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  
  n <- length(données)
  l <- c()
  
  titre <- paste("Nombre de gagnant différent avec 10 méthodes")
  
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



plot_carte_resultat(pos, données)

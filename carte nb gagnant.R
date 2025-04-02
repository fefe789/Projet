plot_carte_resultat <- function(graphelayout, grande_liste) {
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


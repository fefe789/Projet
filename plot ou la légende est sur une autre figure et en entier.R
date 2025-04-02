# on met le graphe et la légende sur deux figure diféérentes pour pas qu'elles se supperposent



plot_carte_type_methode <- function(graphe, layout, grande_liste, titre) {
  # Extraire les méthodes de la grande liste
  methodes <- sapply(grande_liste, function(item) item$methode)
  
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


plot_carte_comparaison_resultat <- function(graphelayout, grande_liste, methode1_index, methode2_index) {
  graphe <- graphelayout[[1]]
  layout <- graphelayout[[2]]
  
  n <- length(grande_liste)
  l <- c()
  
  # Extraire les noms des méthodes à comparer
  methodes <- names(grande_liste[[1]])[-(1:5)]
  methode1 <- methodes[methode1_index]
  methode2 <- methodes[methode2_index]
  
  titre <- paste("Comparaison entre", methode1, "et", methode2)
  
  for (i in 1:n) {
    # Comparer les résultats des deux méthodes spécifiées
    l <- c(l, grande_liste[[i]][[methode1]] == grande_liste[[i]][[methode2]])
  }
  
  # Tracer le graphe

    plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
         vertex.color = ifelse(l, "green", "red"), edge.arrow.size = 0, edge.width = 0, edge.color = "white", main = titre)

  
  # Créer la légende avec une taille de texte réduite

    plot.new()
    legend("center", legend = c("Identique", "Différent"), col = c("green", "red"), pch = 19, title = "Comparaison", bty = "n", cex = 0.8)

  
}



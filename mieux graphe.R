#install.packages('igraph')

library(igraph)

plot_carte <- function(distance_matrix) {
  graphe <- graph_from_adjacency_matrix(distance_matrix, mode = "undirected", weighted = TRUE)
  layout <- layout_with_kk(graphe)  # ca fait des meilleurs paquets
  return(list(graphe,layout))}

# Fonction pour tracer la carte avec des couleurs basées sur les méthodes, sans les lignes
plot_carte_type_methode <- function(graphe,layout, grande_liste, titre) {
  
  # Extraire les méthodes de la grande liste
  methodes <- sapply(grande_liste, function(item) item$methode)
  
  # Associer des couleurs aux méthodes
  couleurs <- rainbow(length(unique(methodes)))
  noms_couleurs <- setNames(couleurs, unique(methodes))
  
  # Tracer uniquement les nœuds avec les couleurs associées aux méthodes
  plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
       vertex.color = noms_couleurs[methodes], edge.color = "white", edge.width = 0, main = titre)
  
  # Ajouter une légende en dehors du graphique
  legend("bottomright", inset = c(-0.2, -0.2), legend = names(noms_couleurs), col = couleurs, pch = 19, title = "Méthodes", bty = "n")
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
logbor<-plot_carte(logborda)

pos<-plot_carte(distance_pos)
logpos<-plot_carte(logpos)

pair<-plot_carte(distance_pair)
logpair<-plot_carte(logpair)



plot_carte_type_methode(bor[[1]],bor[[2]], données, "Carte pour distance_borda")
plot_carte_type_methode(logbor[[1]],logbor[[2]], données, "Carte pour distance_borda log")

plot_carte_type_methode(pos[[1]],pos[[2]], données, "Carte pour distance_pos")
plot_carte_type_methode(logpos[[1]],logpos[[2]], données, "Carte pour distance_pos log")

plot_carte_type_methode(pair[[1]],pair[[2]], données, "Carte pour distance_pair")
plot_carte_type_methode(logpair[[1]],logpair[[2]], données, "Carte pour distance_pos log")




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
  
  # Tracer le graphe (exemple simple, à adapter selon vos besoins)
  plot(graphe, layout = layout, vertex.label = "", vertex.size = 5,
       vertex.color = ifelse(l, "green", "red"), edge.arrow.size = 0, edge.width = 0,edge.color = "white", main = titre)
  
  # Ajouter une légende
  legend("topright", legend = c("Identique", "Différent"), col = c("green", "red"), pch = 19, title = "Comparaison")
}


plot_carte_comparaison_resultat(pos, données, 8, 9)


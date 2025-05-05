nb_candidats<-10
nb_electeurs<-51

# Initialiser une grande liste pour contenir tous les éléments
données <- list()

# Liste des fonctions de génération de votes
generation_functions <- list(
  Identite,
  Oppose, 
  Impartial_Culture,
  Urn_Model,
  mallows_election,
  cube_1,
  Cercle,
  Disque,
  Cube_3, 
  Single_peak_walsh, 
  Single_peak_conitzer
)

# Liste des méthodes correspondantes
methodes <- c(
  "ID","Opposé","IC","Urn","Mallows", "Cube 1D","Cercle","Disque","Cube 3D","Single peak walsh","Single peak conitzer"
)

# Indices personnalisés pour chaque méthode
sizes <- c(1,1,20,60,60,20,20,20,20,20,20)
indices <-  generate_indices(sizes)

# Fonction pour ajouter des votes à la liste de données
ajouter_votes <- function(start_idx, end_idx, generation_func, methode, liste) {
  for (i in start_idx:end_idx) {
    votes <- generation_func(nb_candidats, nb_electeurs)
    score_borda <- calculer_score_borda(votes)
    matrice_classement <-calculer_matrice_preferences(votes)
    
    # Ajouter à la liste
    liste[[i]] <- list(
      votes = votes,
      methode = methode,
      score_borda = score_borda,
      matrice_classement = matrice_classement,
      vote_pluralite = Vote_à_la_pluralité(votes),
      vote_pluralite_2tours = plurality_second_tour(votes),
      borda = borda_method(votes),
      altenative = alternative_vote(votes),
      coombs = coombs_method(votes),
      Bucklin = bucklin_method(votes),
      Minimax = minimax_method(votes),
      Nanson = nanson_method(votes)
    )
  }
  return(liste)
}

# Générer des ensembles de votes pour chaque méthode avec indices personnalisés
for (j in seq_along(generation_functions)) {
  start_idx <- indices[[j]][1]
  end_idx <- indices[[j]][2]
  données<- ajouter_votes(start_idx, end_idx, generation_functions[[j]], methodes[j], données)
}

# Calculer les matrices de distances pour chaque type de distance
n<-length(données)
distance_borda <- matrix(0, nrow = n, ncol = n)
distance_pos <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in i:n) {
    if (i != j) {
      print(i)
      b<-transformer_et_comparer_borda(données[[i]]$score_borda, données[[j]]$score_borda)
      pos<-minimiser_distance_matrices(données[[i]]$matrice_classement, données[[j]]$matrice_classement)  
      distance_borda[i, j] <-b
      distance_borda[j, i] <-b
      distance_pos[i, j] <- pos
      distance_pos[j, i] <- pos
    }
  }
}




saveRDS(données, file = "données.rds")
saveRDS(distance_borda, file = "distance_borda.rds")
saveRDS(distance_pos, file = "distance_pos.rds")


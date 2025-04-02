nb_candidats<-8
nb_electeurs<-50

# Initialiser une grande liste pour contenir tous les éléments
données <- list()

# Liste des fonctions de génération de votes
generation_functions <- list(
  Impartial_Culture,
  cube_1, 
  Cube_2, 
  Cube_3, 
  disque, 
  Identite,
  Oppose, 
  Single_peak_conitzer, 
  Single_peak_walsh, 
  sphere,
  function(nb_candidats, nb_electeurs) Urn_Model(nb_candidats, nb_electeurs, 0.2),
  function(nb_candidats, nb_electeurs) Urn_Model(nb_candidats, nb_electeurs, 0.5),
  function(nb_candidats, nb_electeurs) Urn_Model(nb_candidats, nb_electeurs, 1),
  function(nb_candidats, nb_electeurs) mallows_election(nb_candidats, nb_electeurs, phi = 0.2),
  function(nb_candidats, nb_electeurs) mallows_election(nb_candidats, nb_electeurs, phi = 0.5),
  function(nb_candidats, nb_electeurs) mallows_election(nb_candidats, nb_electeurs, phi = 1)
)

# Liste des méthodes correspondantes
methodes <- c(
  "IC", "cube_1", "cube_2", "cube_3", "cercle", "ID",
  "opposé", "Single_peak_conitzer", "Single_peak_walsh", "Sphere",
  "urn-0.2", "urn-0.5", "urn-1","MAllows-0.2","MAllows-0.5","MAllows-1"
)

# Indices personnalisés pour chaque méthode
sizes <- rep(10,16)
sizes <- c(10,10,10,10,10,1,1,10,10,10,10,10,10,10,10,10)
indices <-  generate_indices(sizes)

# Fonction pour ajouter des votes à la grande liste
ajouter_votes <- function(start_idx, end_idx, generation_func, methode, liste) {
  for (i in start_idx:end_idx) {
    votes <- generation_func(nb_candidats, nb_electeurs)
    score_borda <- calculer_score_borda(votes)
    matrice_classement <- calculer_matrice_preferences(votes)
    matrice_préference <- calculer_preferences_candidats(votes)
    
    # Ajouter à la liste
    liste[[i]] <- list(
      votes = votes,
      methode = methode,
      score_borda = score_borda,
      matrice_classement = matrice_classement,
      matrice_préference = matrice_préference,
      vote_pluralite = Vote_à_la_pluralité(votes),
      vote_pluralite_2tours = plurality_second_tour(votes),
      borda = borda_method(votes),
      altenative = alternative_vote(votes),
      coombs = coombs_method(votes),
      Bucklin = bucklin_method(votes),
      Minimax = minimax_method(votes),
      Nanson = nanson_method(votes),
      copeland = copeland_method(votes),
      black = black_method(votes)
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
distance_pair <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in i:n) {
    if (i != j) {
      print(i)
      b<-transformer_et_comparer_borda(données[[i]]$score_borda, données[[j]]$score_borda)
      pos<-minimiser_distance_matrices(données[[i]]$matrice_classement, données[[j]]$matrice_classement)  
      pair<-minimiser_permutation_matrices(données[[i]]$matrice_préference, données[[j]]$matrice_préference)
      distance_borda[i, j] <-b
      distance_borda[j, i] <-b
      distance_pos[i, j] <- pos
      distance_pos[j, i] <- pos
      distance_pair[i, j] <-pair
      distance_pair[j, i] <-pair
    }
  }
}




saveRDS(données, file = "données.rds")
saveRDS(distance_borda, file = "distance_borda.rds")
saveRDS(distance_pos, file = "distance_pos.rds")
saveRDS(distance_pair, file = "distance_pair.rds")
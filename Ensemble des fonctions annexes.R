###########################
# Fonctions pour générer des élections
###########################

Identite=function(nb_candidats,nb_electeurs){
  a<-sample(x=1:nb_candidats) # génère un classement
  return(replicate(nb_electeurs,a)) # Le réplique nb_electeurs fois
}



Impartial_Culture=function(nb_candidats,nb_electeurs){
  return(replicate(nb_electeurs,sample(x=1:nb_candidats))) # On nb_electeurs vecteur de classment indépendant
}



Oppose=function(nb_candidats,nb_electeurs){
  a<-sample(x=1:nb_candidats) # génère un classement
  return(matrix(c(a,rev(a)),nrow=nb_candidats,ncol=nb_electeurs)) # Le réplique jusqu'a attiendre nb_electeurs le classement et son inverse
}



cube_1=function(nb_candidats,nb_electeurs){
  cand<-replicate(nb_candidats,runif(1)) # génère uniformément la postion des candidats dans [0,1]
  votes<-replicate(nb_electeurs,abs(cand-runif(1))) # génère de la taille nb_electeurs, la distance de l'électeur à chacun des candidats
  for(i in 1:nb_electeurs){
    votes[,i]<-order(votes[,i]) # ordonne les distances pour donner le classement
  }
  return(votes)
}





Cube_3=function(nb_candidats,nb_electeurs){# idem cube_1 en dimension 3
  cand<-replicate(nb_candidats,runif(3))
  votes<-replicate(nb_electeurs,cand-runif(3))
  votes<-apply(votes, c(2, 3), function(x) sum(abs(x)^3)^(1/3))
  for(i in 1:nb_electeurs){
    votes[,i]<-order(votes[,i])
  }
  return(votes)
}






Cercle=function(nb_candidats,nb_electeurs){
  cand<-replicate(nb_candidats,runif(1,min=0,max=2*pi))# génère  pour chaque candidats un nombre  uniformément dans [0,2pi]
  cand<-sapply(cand,FUN= function(x) c(sin(x),cos(x))) # donne la position du candidat sur le cercle
  votes<-replicate(nb_electeurs,runif(1,min=0,max=2*pi)) # idem pour les électeurs
  result <- array(NA, dim = c( 2, nb_candidats,nb_electeurs)) 
  for (i in 1:nb_electeurs) {
    voter_coord <- c(sin(votes[i]), cos(votes[i]))  
    result[, ,i] <- cand - voter_coord  #différence entre les postions des électeurs et candidats
  }
  votes<-apply(result, c(2, 3), function(x) norm(as.matrix(x), type = "2")) # calcule les distances
  for(i in 1:nb_electeurs){
    votes[,i]<-order(votes[,i]) # trie et donc le classment
  }
  return(votes)
}

Disque=function(nb_candidats,nb_electeurs){
  cand<-replicate(nb_candidats,c(runif(1,min=0,max=1),runif(1,min=0,max=2*pi)))# génère  pour chaque candidats un nombre uniformémentdans [0,1] et  dans [0,2pi]
  cand<-apply(cand,2,FUN= function(x) c(x[1]*sin(x[2]),x[1]*cos(x[2]))) # donne la position du candidat sur le cercle
  votes<-replicate(nb_electeurs,c(runif(1,min=0,max=1),runif(1,min=0,max=2*pi))) # idem pour les électeurs
  result <- array(NA, dim = c( 2, nb_candidats,nb_electeurs)) 
  for (i in 1:nb_electeurs) {
    voter_coord <- c(votes[1,i]*sin(votes[2,i]), votes[1,i]*cos(votes[2,i])) 
    result[, ,i] <- cand - voter_coord  #différence entre les postions des électeurs et candidats
  }
  votes<-apply(result, c(2, 3), function(x) norm(as.matrix(x), type = "2")) # calcule les distances
  for(i in 1:nb_electeurs){
    votes[,i]<-order(votes[,i]) # trie et donc le classment
  }
  return(votes)
}






Single_peak_conitzer=function(nb_candidats,nb_electeurs){
  classement<-sample(1:nb_candidats) # génère l'ordre des candidats
  votes<-matrix(NA,nb_candidats,nb_electeurs)
  for(i in 1:nb_electeurs){
    peak<-sample(1:nb_candidats,1) # choisi le pic pour l'électeur
    a<-rep(c(0.1,0.2),times=c(nb_candidats-peak,peak-1)) # fait un vecteur avec le nombre adéquat de candidats à gauche et à droite du pic
    b<-sample(a) # les mélange
    b[b==0.1]<-classement[(peak+1):nb_candidats] # remplace les candidats à droite
    b[b==0.2]<-rev(classement[1:(peak-1)])# remplace les candidats à gauche
    votes[,i]<-c(classement[peak],b)
  }
  return(votes)
}




Single_peak_walsh=function(nb_candidats,nb_electeurs){ # idem Single_peak_conitzer , sauf que le choix du pic n'est pas uniforme mais inversement proportionnel au nombre de classement possible pour chaque pic
  classement<-sample(1:nb_candidats)
  proba <- sapply(0:(nb_candidats-1), function(i) factorial(nb_candidats-1) / (factorial(i) * factorial(nb_candidats-1 - i)))/(2**(nb_candidats-1))
  votes<-matrix(NA,nb_candidats,nb_electeurs)
  for(i in 1:nb_electeurs){
    peak<-sample(1:nb_candidats,1,prob=proba)
    a<-rep(c(0.1,0.2),times=c(nb_candidats-peak,peak-1))
    b<-sample(a)
    b[b==0.1]<-classement[(peak+1):nb_candidats]
    b[b==0.2]<-rev(classement[1:(peak-1)])
    votes[,i]<-c(classement[peak],b)
  }
  return(votes)
}




Urn_Model=function(nb_candidats,nb_electeurs){ 
  alpha<-rgamma(1,0.8,1)
  votes<- matrix(0,nb_candidats,nb_electeurs)
  for(i in 1:nb_electeurs){
    u<-runif(1) # tire une uniforme
    a<-(0:(i-1)*alpha)/((i-1)*alpha+1) # crée un vecteur qui de points entre 0 et 1 qui corresond au probabilité de tirer un classsemtn déjà sortie ou non
    t<-which(a>u) 
    if(is.integer(t) && length(t) == 0){ # tire un classement aléatoire pour l'électeur
      votes[,i]<-sample(x=1:nb_candidats)
    }
    else{# attribue à l'électeur le même classemnt que le min(t)-1 électeur
      votes[,i]<-votes[,min(t)-1]
    }
    
  }
  return(votes)
}



#On ne savait pas faire donc on l'a copié sur ces personne la: github.com/Project-PRAGMA/Normalized-Mallows-ICML-2023.

mallows_vote <- function(nb_candidats, phi) {
  # Function to compute insertion probabilities
  computeInsertionProbas <- function(i, phi) {
    return(phi^((i:0)))  # Generates [phi^i, phi^(i-1), ..., phi^0]
  }
  
  # Function for weighted random choice
  weighted_choice <- function(weights) {
    total <- sum(weights)
    r <- runif(1, min = 0, max = total)
    upto <- 0
    for (i in seq_along(weights)) {
      upto <- upto + weights[i]
      if (upto >= r) {
        return(i)  # Return the position
      }
    }
    stop("Shouldn't get here")
  }
  
  # Start with a single candidate in the ranking
  vote <- c(0)  # First candidate (index 0 in Python, but R uses 1-based index)
  
  for (i in 1:(nb_candidats - 1)) {
    # Compute insertion probabilities
    insertion_probas <- computeInsertionProbas(i, phi)
    
    # Choose a position in the ranking
    index <- weighted_choice(insertion_probas)
    
    # Insert the candidate in the chosen position
    vote <- append(vote, i, after = index - 1)  # Adjust index for R
  }
  
  return(vote + 1)  # Convert to 1-based indexing for R
}
mallows_vote(10, 0.8)


mallows_election <- function(nb_candidats, nb_electeurs) {
  phi<-runif(1)
  votes <- replicate(nb_electeurs, mallows_vote(nb_candidats, phi))
  return(matrix(unlist(votes), nrow = nb_candidats, byrow = FALSE))
}

generate_indices <- function(sizes) {
  start <- 1
  indices <- list()
  
  for (size in sizes) {
    end <- start + size - 1
    indices <- c(indices, list(c(start, end)))
    start <- end + 1
  }
  
  return(indices)
}



###########################
# Fonctions pour calculer les distances
###########################





calculer_score_borda <- function(votes) { # calcul les score de borda pour une élections
  nb_candidats <- nrow(votes)
  nb_electeurs <- ncol(votes)
  
  # Initialiser le vecteur des scores
  scores <- rep(0, nb_candidats)
  
  # Calculer les points de Borda pour chaque électeur
  for (electeur in 1:nb_electeurs) {
    preferences <- votes[, electeur]
    points <- nb_candidats:1  # Points décroissants
    scores <- scores + points[order(preferences)]
  }
  
  return(scores)
}





# Fonction pour calculer la matrice de préférences entre candidats

calculer_matrice_preferences <- function(votes) {
  nb_candidats <- max(votes)  # Nombre total de candidats
  nb_electeurs <- ncol(votes)  # Nombre d'électeurs
  
  # Initialiser la matrice de comptage
  matrice_preferences <- matrix(0, nrow = nb_candidats, ncol = nb_candidats)
  
  # Parcourir les votes de chaque électeur
  for (electeur in 1:nb_electeurs) {
    preferences <- votes[, electeur]
    for (position in 1:nb_candidats) {
      candidat <- preferences[position]
      matrice_preferences[position, candidat] <- matrice_preferences[position, candidat] + 1
    }
  }
  
  return(matrice_preferences)
}




# Fonction pour transformer un vecteur en EDM
edm <- function(vecteur) {
  sapply(1:length(vecteur), function(i) sum(vecteur[1:i]))
}



# Fonction pour calculer la distance entre deux vecteurs transformés
distance_edm <- function(vecteur1, vecteur2) {
  sum(abs(edm(vecteur1) - edm(vecteur2)))
}



# Fonction pour minimiser la somme des distances entre les colonnes des deux matrices
minimiser_distance_matrices <- function(matrice1, matrice2) {
  nb_colonnes <- ncol(matrice1)
  if (ncol(matrice2) != nb_colonnes) {
    stop("Les deux matrices doivent avoir le même nombre de colonnes.")
  }
  
  # Initialiser les indices des colonnes non associées
  colonnes_non_associees <- 1:nb_colonnes
  distance_totale <- 0
  
  # Parcourir chaque colonne de la matrice1
  for (i in 1:nb_colonnes) {
    # Calculer les distances avec les colonnes restantes de la matrice2
    distances <- sapply(colonnes_non_associees, function(j) {
      distance_edm(matrice1[, i], matrice2[, j])
    })
    
    # Trouver l'indice de la colonne avec la distance minimale
    indice_min <- which.min(distances)
    colonne_min <- colonnes_non_associees[indice_min]
    
    # Ajouter la distance minimale à la distance totale
    distance_totale <- distance_totale + distances[indice_min]
    
    # Retirer la colonne associée de la liste des colonnes non associées
    colonnes_non_associees <- colonnes_non_associees[-indice_min]
  }
  
  return(distance_totale)
}




transformer_et_comparer_borda <- function(score1, score2) {
  # Trier les scores
  score1_sorted <- sort(score1)
  score2_sorted <- sort(score2)
  
  # Remplacer chaque élément par la somme des i premiers éléments
  transformer_vecteur <- function(vecteur) {
    sapply(1:length(vecteur), function(i) sum(vecteur[1:i]))
  }
  
  score1_transformed <- transformer_vecteur(score1_sorted)
  score2_transformed <- transformer_vecteur(score2_sorted)
  
  # Calculer la somme des valeurs absolues des différences terme à terme
  somme_absolue <- sum(abs(score1_transformed - score2_transformed))
  
  return(somme_absolue)
}




######################
# Fonctions pour élire le gagnant de l'éléction
######################





Vote_à_la_pluralité=function(set){
  result<- c()
  for(i in 1:5){result<-c(result,sum(set[1,]==i))}
  return(which.max(result))
}




plurality_second_tour <- function(votes) {
  # Nombre de candidats
  num_candidats <- nrow(votes)
  
  # Nombre d'électeurs
  num_electeurs <- ncol(votes)
  
  # Calcul des premiers choix (vote à la pluralité)
  premier_choix <- table(votes[1, ])
  
  # Identifier les deux candidats ayant le plus de votes
  deux_meilleurs <- names(sort(premier_choix, decreasing = TRUE))[1:2]
  
  # Si un candidat a plus de la moitié des votes, il gagne directement
  if (premier_choix[deux_meilleurs[1]] > num_electeurs / 2) {
    return(as.integer(deux_meilleurs[1]))
  }
  
  # Filtrer les votes pour ne garder que les deux finalistes
  second_tour_votes <- apply(votes, 2, function(col) {
    col[col %in% as.integer(deux_meilleurs)]
  })
  
  # Initialisation correcte du score pour le second tour
  scores_final <- setNames(c(0, 0), deux_meilleurs)
  
  # Compter les préférences entre les deux finalistes
  for (vote in second_tour_votes) {
    if (vote[1] == as.integer(deux_meilleurs[1])) {
      scores_final[1] <- scores_final[1] + 1
    } else {
      scores_final[2] <- scores_final[2] + 1
    }
  }
  
  # Retourner le gagnant
  return(as.integer(names(which.max(scores_final))))
}





borda_method = function(votes) {
  # Nombre de candidats
  num_candidats <- nrow(votes)
  
  # Nombre d'électeurs
  num_electeurs <- ncol(votes)
  
  # Initialisation des scores
  scores <- setNames(rep(0, num_candidats), as.character(1:num_candidats))
  
  # Calcul des scores Borda
  for (i in 1:num_electeurs) {
    classement <- votes[, i]
    for (j in 1:num_candidats) {
      scores[as.character(classement[j])] <- scores[as.character(classement[j])] + (num_candidats - j)
    }
  }
  
  # Retourner le candidat avec le score le plus élevé
  return(as.integer(names(which.max(scores))))
}



alternative_vote =function(votes) {
  num_candidats <- nrow(votes)
  num_electeurs <- ncol(votes)
  candidats <- as.character(1:num_candidats)
  
  while (TRUE) {
    # Calculer les premiers choix
    premier_choix <- table(votes[1, ])
    
    # Vérifier si un candidat a la majorité absolue
    if (any(premier_choix >= num_electeurs / 2)) {
      return(as.integer(names(which.max(premier_choix))))
    }
    
    # Trouver le candidat avec le moins de premiers choix
    candidat_a_eliminer <- names(which.min(premier_choix))
    
    # Supprimer ce candidat des votes
    votes <- apply(votes, 2, function(col) col[col != as.integer(candidat_a_eliminer)])
  }
}



coombs_method =function(votes) {
  num_candidats <- nrow(votes)
  num_electeurs <- ncol(votes)
  
  while (TRUE) {
    # Calculer les premiers choix
    premier_choix <- table(votes[1, ])
    
    # Vérifier si un candidat a la majorité absolue
    if (any(premier_choix >= num_electeurs / 2)) {
      return(as.integer(names(which.max(premier_choix))))
    }
    
    # Calculer les derniers choix
    dernier_choix <- table(votes[nrow(votes), ])
    
    # Trouver le candidat classé dernier par le plus grand nombre d'électeurs
    candidat_a_eliminer <- names(which.max(dernier_choix))
    
    # Supprimer ce candidat des votes
    votes <- apply(votes, 2, function(col) col[col != as.integer(candidat_a_eliminer)])
  }
}




bucklin_method = function(votes) {
  num_candidats <- nrow(votes)
  num_electeurs <- ncol(votes)
  
  # Initialisation des scores des candidats
  scores <- setNames(rep(0, num_candidats), as.character(1:num_candidats))
  
  for (round in 1:num_candidats) {
    # Ajouter les votes de ce niveau de préférence
    for (i in 1:num_electeurs) {
      candidat <- as.character(votes[round, i])
      scores[candidat] <- scores[candidat] + 1
    }
    
    # Vérifier si un candidat a la majorité absolue
    if (any(scores > num_electeurs / 2)) {
      return(as.integer(names(which.max(scores))))
    }
  }
  
  # En cas d'égalité, retourner le candidat avec le plus grand soutien
  return(as.integer(names(which.max(scores))))
}



minimax_method = function(votes) {
  num_candidats <- nrow(votes)
  num_electeurs <- ncol(votes)
  
  # Construire la matrice des duels entre candidats
  duel_matrix <- matrix(0, nrow = num_candidats, ncol = num_candidats)
  
  for (i in 1:num_candidats) {
    for (j in 1:num_candidats) {
      if (i != j) {
        duel_matrix[i, j] <- sum(apply(votes, 2, function(col) which(col == i) < which(col == j)))
      }
    }
  }
  
  # Vérifier s'il y a un vainqueur de Condorcet
  condorcet_winner <- which(rowSums(duel_matrix > (num_electeurs / 2)) == (num_candidats - 1))
  if (length(condorcet_winner) == 1) {
    return(condorcet_winner)
  }
  
  # Calculer le nombre de défaites pour chaque candidat
  defeat_counts <- rowSums(duel_matrix < (num_electeurs / 2))
  min_defeat_count <- min(defeat_counts)
  candidates_with_min_defeat <- which(defeat_counts == min_defeat_count)
  
  # Calculer la pire défaite uniquement (en excluant les victoires)
  worst_defeat <- apply(duel_matrix, 1, function(row) {
    defeats <- row[row < (num_electeurs / 2) & row!=0]  # Ne prendre que les défaites
    if (length(defeats) > 0) {
      return(min(defeats))  # La pire défaite parmi celles où le candidat perd
    } else {
      return(0)  # Si le candidat n'a pas de défaite, sa pire défaite est 0
    }
  })
  
  # Sélectionner le candidat parmi ceux avec les défaites minimales dont la pire défaite est la plus faible
  best_candidate <- candidates_with_min_defeat[which.max(worst_defeat[candidates_with_min_defeat])]
  
  return(best_candidate)
}





nanson_method = function(votes) {
  num_candidats <- nrow(votes)
  candidats <- 1:num_candidats
  
  while (length(candidats) > 1) {
    num_candidats <- nrow(votes)
    num_electeurs <- ncol(votes)
    
    # Calculer les scores de Borda
    scores <- setNames(rep(0, length(candidats)), candidats)
    for (i in 1:num_electeurs) {
      for (j in seq_along(candidats)) {
        scores[as.character(votes[j, i])] <- scores[as.character(votes[j, i])] + (num_candidats - j)
      }
    }
    
    # Calculer la moyenne des scores
    moyenne_score <- mean(scores)
    
    # Identifier les candidats à éliminer
    candidats_a_eliminer <- as.integer(names(scores[scores <= moyenne_score]))
    
    # Supprimer les candidats éliminés
    candidats <- setdiff(candidats, candidats_a_eliminer)
    votes <- apply(votes, 2, function(col) col[col %in% candidats])
  }
  
  return(candidats)
}





















## _________________ Infos générales
## Nom : Moyenne_integree.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 20 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui effectue une moyenne intégrée sur les jours juliens
#' @param x Numérique : les valeurs à inrégrer
#' @param by Numérique : les jours juliens
#' @param from Numérique : jour julien mini
#' @param to Numérique : jour julien maxi
#' @return valeurs moyennée
#
## ---------------------------


Moyenne_integree <- function(x, by, from, to) {
  
  if(any(is.na(x))){
    cat("Les valeurs manquantes sont enlevées = on saute un prélÃƒÂ¨vement\n")
    by <- by[!is.na(x)]
    x <- x[!is.na(x)]
  }
  
  if(length(x) != length(by)) {
    stop("x and by must have the same length\n")
  }
  
  if(!isTRUE(all.equal(by, by[order(by)]))) {
    stop("Argument 'by' needs to be ordered from the lowest to highest. Don't forget to adapt 'x'")
  }
  
  if(length(x) >= 2){
    # je peux faire une moyenne que si j'ai plus de 2 valeurs...
    
    Means <- c()
    for(i in 2:length(x)){
      
      tmp_x <- x[(i-1):i]
      tmp_by <- by[(i-1):i]
      
      # Cas normal
      m <- mean(tmp_x) * abs(diff(tmp_by))
      
      
      # Cas du 1er jour du mois
      if(i == 2){
        #  et from inférieur au 1er jour ou 1er prof
        if(from < by[i]){
          m <- m * (by[i]-from)/abs(diff(tmp_by))
        }
      }
      
      # Cas du dernier (jour du mois ou prof.max > to)
      # Double vérif
      if(i == length(x)){
        if(to < by[i]){
          m <- m * (to-by[i-1])/abs(diff(tmp_by))
        }
      }
      
      Means <- c(Means, m)
    }
    
    out <- sum(Means) / (to-from)
    
  }else{
    out <- NA
  }
    

  return(out)
}


## _________________ Notes
#
#' Fonction qui effectue une moyenne intégrée sur la profondeur
#' @param x Numérique : les valeurs à inrégrer
#' @param by Numérique : les profondeurs
#' @param from Numérique : profondeur mini à intégrer
#' @param to Numérique : profondeur maxi à intégrer
#' @return valeurs moyennée
#
## ---------------------------
Moyenne_integree_profondeur <- function(x, by, from, to) {
  
  if(any(is.na(x))){
    cat("Les valeurs manquantes sont enlevées = on saute un prélèvement\n")
    by <- by[!is.na(x)]
    x <- x[!is.na(x)]
  }
  
  if(length(x) != length(by)) {
    stop("x and by must have the same length\n")
  }
  
  if(!isTRUE(all.equal(by, by[order(by)]))) {
    stop("Argument 'by' needs to be ordered from the lowest to highest. Don't forget to adapt 'x'")
  }
  
  if(length(x) >= 2){
    # je peux faire une moyenne que si j'ai plus de 2 valeurs...
    stop <- FALSE  
    Means <- c()

    for(i in 2:length(x)){
      
      tmp_x <- x[(i-1):i]
      tmp_by <- by[(i-1):i]
      
      # Cas normal
      m <- mean(tmp_x) * abs(diff(tmp_by))
      
      
      # Cas de la 1ere profondeur
      if(i == 2){
        # si la profondeur demandée = celle dispo
        if(from == by[i-1]){
          m <- m
        }
        
        # si la profondeur demandée > ou < celle dispo alors je fais un ratio
        if(from > by[i-1]){
          m <- m * (1-(from - by[i-1])/abs(diff(tmp_by)))
        }
        
        
        if(from < by[i-1]){
          m <- m * (1+(by[i-1] - from)/abs(diff(tmp_by)))
        }
      }
     
      
      # Cas du dernier (jour du mois ou prof.max > to)
      # je ne veux passer ici qu'une seule fois
      if(length(x) == i){
        # Plus basse profondeur = to
        if(to == by[i]){
            m <- m
        }

        # to est moins profond que la plus basse profondeur (ici by[i-1] et pas by[i] car je fais comme si dernière prof = to)
        if (to < by[i]) {
          m <- m * (to - by[i - 1]) / abs(diff(tmp_by))
        }

        #to est plus profond que la plus basse profondeur 
        if (to > by[i]) {
          m <- m * (to - by[i - 1]) / abs(diff(tmp_by))
        }
      }

      
      
      Means <- c(Means, m)
      
      if(isTRUE(stop)) break()
    }
    
    out <- sum(Means) / abs(diff(c(to,from)))
    c
  }else{
    out <- NA
  }
  
  
  return(out)
}


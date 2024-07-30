## _________________ Infos générales
## Nom : Moyennes.verticales
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 24 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui prépare les données pour la moyenne intégrée sur les jours juliens
#' @param data data.frame avec les données, il faut au moins les colonnes : 
#'         - Depth avec des profondeurs
#'         - id.profil avec un identifiant unique pour les profils
#'         - Year = les années
#'         - Month = les mois
#'         - YearTS = date au format décimal
#'         - des paramÃƒÂ¨tres
#'
#' @param col.param : les numéros de colonne des paramÃƒÂ¨tres
#' @param profondeur.mini : la profondeur minimal requise pour traiter le profil
#' @param nb.profondeur.mini : le nombre de profonderus mini dans le profil pour le traiter
#' @param nb.cores : le nombre de coeurs du processeur sur lesquels paralléliser. Par défaut = la moitié
#' @return Un dataframe avec les valeurs intégrées
#
## ---------------------------


Moyennes.verticales <- function(data, col.param, profondeur.mini, nb.profondeur.mini, nb.cores = detectCores()/2) {
  
  out <- 
    do.call(
      'rbind',
      mclapply(
        mc.cores = nb.cores,
        mc.preschedule = TRUE, 
        unique(data$id.profil),
        function(x) {
          
          tmp <- data[data$id.profil == x,]
          
          # Je ne considÃƒÂ¨re que les profils commenÃƒÂ§ant entre 0 et profondeur mini
          prof.ok <- ifelse(min(tmp$Depth) < profondeur.mini, TRUE, FALSE)
          nb.prof.ok <- ifelse(uniqueN(tmp$Depth) > nb.profondeur.mini, TRUE, FALSE) 
          
          TMP.OUT <- data.frame()
          
          if(prof.ok & nb.prof.ok){
           
            for(p in col.param) {
              
              tmp01 <- tmp[,c(-col.param)]
              tmp01 <- data.frame(tmp01, tmp[,p])
              colnames(tmp01)[ncol(tmp01)] <- "Param"
              
              
              # En attendant les réponses, je ne prends qu'une valeur par profondeur
              tmp01 <- tmp01[order(tmp01$Depth),]
              
              tmp01 <- do.call("rbind", 
                               lapply(unique(tmp01$Depth), 
                                      function(d){
                                        out <- tmp01[tmp01$Depth == d,][1,]
                                        out$Param <- mean(tmp01[tmp01$Depth == d,"Param"])
                                        return(out)
                                      })
              )
              
              Mi <- Moyenne_integree_profondeur(x = tmp01[,"Param"], by = tmp01$Depth, from = 5, to = 100)
              
              tmp_out <- data.frame(Year = unique(tmp01$Year), 
                                    Month = unique(tmp01$Month), 
                                    YearTS = unique(tmp01$YearTS),
                                    Site = unique(tmp01$Site),
                                    Param = colnames(tmp)[p], 
                                    Val = Mi,
                                    keep = TRUE)
              TMP.OUT <- rbind(TMP.OUT, tmp_out)
            }
          }else{
            tmp_out <- data.frame(Year = NA, 
                                  Month = NA, 
                                  YearTS = NA,
                                  Site = NA,
                                  Param = NA, 
                                  Val = NA,
                                  keep = FALSE)
            TMP.OUT <- rbind(TMP.OUT, tmp_out)
          }
          
          
          return(TMP.OUT)
          }
      )
    )
  
  out <- out[which(out$keep),]
  out <- out[,colnames(out) != "keep"]
  
  out$Série <- paste(out$Site, out$YearTS, sep="_")
  
  
  # Si plusieurs profils le même jour je les moyenne ici direct
  #x= unique(out$YearTS)[3]
  out01 <- do.call(
    "rbind",
    lapply(
      unique(out$Série),
      function(x) {
        tmp <- out[out$Série==x,]
        
        TMP.out <- data.frame()
        for(p in unique(tmp$Param)){
          tmpp <- tmp[tmp$Param == p,]
          
          if(nrow(tmpp) > 1){
            tmp.out <- tmpp[1,]
            tmp.out$Val <- mean(tmpp$Val)
          }else{
            tmp.out <- tmpp
          }
          
          TMP.out <- rbind(TMP.out, tmp.out)
        }

       

        return(TMP.out)
      }
    )
  )


  
  return(out01)
}












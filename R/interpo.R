## _________________ Infos générales
## Nom : interpo.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 18 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Sript parent de l'interpolation des valeurs manquantes
#' @param Data_TS_month_year_ok Les données
#' @return Les données avec colonnes de valeurs interpolées
#
## ---------------------------

interpo <- function(Data_TS_month_year_ok) {
  
  
  if(sum(is.na(Data_TS_month_year_ok$Val)) == 0) {
    cat("Il n'y a rien ÃƒÂ  faire ici")
    
    Data_TS_ok <- Data_TS_month_year_ok
    Data_TS_ok$isna <- FALSE
    Data_TS_ok$fill <- Data_TS_ok$Val
    
    
  }else{
    Data_TS_ok <- do.call(
      "rbind",
      lapply(
        unique(Data_TS_month_year_ok$Param),
        function(p) {
          
          
          tmp <- Data_TS_month_year_ok[Data_TS_month_year_ok$Param == p,]
          
          
          out <- do.call(
            "rbind",
            lapply(
              unique(tmp$Site),
              function(s) {
                tmp01 <- tmp[tmp$Site == s,]
                
                # Je vérifie pour chaque paramÃƒÂ¨tre s'il y a NA ou pas
                if(sum(is.na(tmp01$Val)) != 0){
                  # print(p)
                  Mois_a_interpo <- unique(tmp01[which(is.na(tmp01$Val)),"Month"])
                  
                  Cycle_saison <- data.frame(Month = Mois_a_interpo,
                                             Val.interpo = sapply(Mois_a_interpo, function(x) mean(tmp01[tmp01$Month == x,"Val"], na.rm=T)))
                  tmp01$isna <- FALSE
                  tmp01[which(is.na(tmp01$Val)),"isna"] <- TRUE
                  tmp01[,"fill"] <- tmp01[,"Val"]
                  tmp01[which(is.na(tmp01$Val)),"fill"] <- Cycle_saison[match(tmp01[which(is.na(tmp01$Val)),"Month"], Cycle_saison$Month),"Val.interpo"]
                  
                }else{
                  
                  tmp01$isna <- FALSE
                  tmp01$fill <- tmp01$Val
                  
                }
                
                return(tmp01)
              }
            )
          )
          
         
          
          return(out)
        }
      )
    )
  }
  
  return(Data_TS_ok)
}

## _________________ Notes
#
#' Fonction qui fait un plot avec les données interpolées
#' @param Data_TS_ok Les données
#' @param name Nom donné à l'image
#' @param path Chemin de sauvegarde
#' @import ggplot2
#' @return Les données avec colonnes de valeurs interpolées
#
## ---------------------------
interpo.plot <- function(Data_TS_ok, name = NA, path) {
  
  g <-  ggplot(data=Data_TS_ok, aes(x=YearTS, y=Val)) + 
    geom_point() + 
    geom_point(data = Data_TS_ok[Data_TS_ok$isna == TRUE,], aes(y=fill), size=2, col="red") +
    geom_line(aes(y=fill), alpha = .7) + 
    facet_wrap(Param~Site, scales = "free") +
    theme_bw()
  
  
  # Sauvegarde dans dossier dédié
  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }
  
  gg_png(g, paste0("TS_interpolees_", name, ".png"), path = path)
 
}

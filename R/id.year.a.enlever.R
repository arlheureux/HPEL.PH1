## _________________ Infos générales
## Nom : id.year.a.enlever.R
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
#' Fonction qui identifie les années/mois pour lesquelles l'évaluation n'est pas possible
#' @param Data_TS_month Les données
#' @return Un dataframe avec les années et mois a enlever
#
## ---------------------------

id.year.enlever <- function(Data_TS_month, nb.mois.min, nb.cores = detectCores()/2) {
 
  mois.non <- Data_TS_month[is.na(Data_TS_month$Val),]
  tab.mois.non <- table(mois.non$Param, mois.non$Year, mois.non$Site)
  id.non <- which(tab.mois.non > 12-nb.mois.min, arr.ind=TRUE)
  
  
  Year.a.enlever <- data.frame(Site = NA, Year=NA, Param=NA)
  if(nrow(id.non) != 0){
    # for(i in 1:nrow(id.non)){
    #   Year.a.enlever[i,"Site"] <- dimnames(tab.mois.non)[[3]][id.non[i,3]]
    #   Year.a.enlever[i,"Year"] <- colnames(tab.mois.non)[id.non[i,2]]
    #   Year.a.enlever[i,"Param"] <- rownames(tab.mois.non)[id.non[i,1]]
    # }
    
    Year.a.enlever <- do.call(
      "rbind",
      parallel::pbmclapply(
        mc.cores = nb.cores,
        1:nrow(id.non),
        function(i) {
          tmp <- data.frame(Site = NA, Year = NA, Param = NA)
          tmp[, "Site"] <- dimnames(tab.mois.non)[[3]][id.non[i, 3]]
          tmp[, "Year"] <- colnames(tab.mois.non)[id.non[i, 2]]
          tmp[, "Param"] <- rownames(tab.mois.non)[id.non[i, 1]]
          return(tmp)
        }
      )
    )

    cat("Il faut enlever les années suivantes pour les paramètres suivants\n\n")
    print(Year.a.enlever)
  }else{
    cat("Toutes les années comportent au minimum la proportion définie, et ce pour tous les paramètres\n\n")
  }
  
  

  
  return(Year.a.enlever)
}


## _________________ Notes
#
#' Fonction qui enlève les années/mois pour lesquelles l'évaluation n'est pas possible
#' @param Data_TS_month Les données
#' @param Year.a.enlever Dataframe issu de id.year.enlever()
#' @param prop.annee.mini proportion d'année mini pour la période d'évaluation
#' @param nb.cores Nombre de coeurs pour la parallélisation
#' @return Les données avec les années / mois en NA
#
## ---------------------------
remove.years <- function(Data_TS_month, Year.a.enlever, prop.annee.mini, nb.cores = detectCores()/2) {
  
  if(nrow(Year.a.enlever) == 1 & sum(is.na(Year.a.enlever)) == ncol(Year.a.enlever)) {
    
    Data_TS_month_year_ok <- Data_TS_month
    Data_TS_month_year_ok$keep <- TRUE
    cat("Il n'y a rien à faire ici")
    
  }else{
    
    
    # J'enlève les années correspondantes
    Data_TS_month$keep <- TRUE


    #METTRE DES SERIES
    Year.a.enlever$Serie <- paste(Year.a.enlever$Site, Year.a.enlever$Year, Year.a.enlever$Param, sep = "_")
    Data_TS_month$Serie <- paste(Data_TS_month$Site, Data_TS_month$Year, Data_TS_month$Param, sep = "_")

    for(i in unique(Data_TS_month$Serie)) {
      if (i %in% Year.a.enlever$Serie) {
        Data_TS_month[Data_TS_month$Serie == i, "keep"] <- FALSE
      }     
    }
    # summary(Data_TS_month)


    Data_TS_month_year_ok <- Data_TS_month[Data_TS_month$keep == TRUE,]
    Prop_y_elim <- 1-table(Data_TS_month_year_ok$Param, Data_TS_month_year_ok$Site)/12/range.year
    
    Prop_y_elim <- data.table::melt(Prop_y_elim)
    colnames(Prop_y_elim) <- c("Param", "Site", "value")
    
    Prop_y_elim$Serie <- paste(Prop_y_elim$Site, Prop_y_elim$Param, sep = "_")
    Data_TS_month_year_ok$Serie <- paste(Data_TS_month_year_ok$Site, Data_TS_month_year_ok$Param, sep = "_")

    Data_TS_month_year_ok <- do.call(
      "rbind",
      pbmclapply(
        mc.cores = nb.cores,
        unique(Data_TS_month_year_ok$Serie),
        function(p) {
          tmp <- Data_TS_month_year_ok[Data_TS_month_year_ok$Serie == p, ]
          
            if (p %in% Prop_y_elim$Serie) {
              tmp[, "Prop_year_elim"] <- unname(Prop_y_elim[Prop_y_elim$Serie == p, "value"])
            }else{
              tmp[, "Prop_year_elim"] <- 0
            }
          
          return(tmp)
        }
      )
    )

    
    boxplot(unique(Data_TS_month_year_ok$Prop_year_elim), main = "Proportion des séries éliminées")
    
    # J'enlève les paramètres dont on ne conserve moins de la proportion d'année limite
    Data_TS_month_year_ok <- Data_TS_month_year_ok[Data_TS_month_year_ok$Prop_year_elim < prop.annee.mini,]
  }

  return(Data_TS_month_year_ok)
}

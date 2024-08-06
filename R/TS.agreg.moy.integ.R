## _________________ Infos générales
## Nom : TS.agreg.moy.integ.R
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
#' Fonction mÃƒÂ¨re de l'agrégation temporelles par moyenne intégrée
#' @param Data_TS Les données
#' @param agg.func Texte : fonction pour aggréger
#' @param year.min Numérique : Année min
#' @param year.max Numérique : Année max
#' @param season Numérique : Numéro de saisons. Par défaut 1:12
#' @param col.param Numérique numéro de colonnes avec les paramÃƒÂ¨tres
#' @param nb.cores le nombre de coeurs ÃƒÂ  utiliser pour paralléliser
#' @return Les données agrégées
#
## ---------------------------



TS.agreg.moy.integ <- function(Data_TS, agg.func, year.min, year.max, season = 1:12, col.param,
                               nb.cores = detectCores()/2){
  
  cat("Aggrégation des séries en utilisant la fonction :", agg.func, "intégrée avec les jours juliens\n")
  
  Out <- 
    do.call(
      "rbind",
      mclapply(
        mc.cores = nb.cores,
        mc.preschedule = TRUE, 
        unique(Data_TS$Year),
        function(a) {
          target_year <- a
          #message_parallel(a)
          #print(a)
          
          Out_s <- do.call(
            "rbind",
            lapply(
              unique(Data_TS$Site),
              function(s) {
                target_site <- s
                
                Out_a <- do.call(
                  "rbind",
                  lapply(
                    unique(Data_TS$Month),
                    function(m) {
                      target_month <- m
                     
                      # je me crée un calendrier
                      Calendrier <- get.calendar(from = target_year-1, to = target_year +1)
                      
                      Jours_extended <- get.buffer.dates(target_month, target_year, Calendrier)
                      tmp00 <- Data_TS[Data_TS$YearTS %in% Jours_extended & Data_TS$Site == s,]
                      
                                            
                      # En attendant des données pour 2006, mon from en 2007 = 9 janvier
                      Out00 <- do.call(
                        "rbind",
                        lapply(
                          col.param,
                          function(i, .tmp00=tmp00, .target_month=target_month){
                            #print(i)

                            # je filtre mon df sur les jours du mois target + respectivement le 1er et dernier jours des mois suivant et précédent
                            r <- which(tmp00$Month == target_month)
                            r <- c(min(r) - 1, r, max(r) + 1)


                            Calendrier_target <- Calendrier[which(Calendrier$Date_decimale == min(tmp00$YearTS)):
                            which(Calendrier$Date_decimale == max(tmp00$YearTS)), ]
                            Calendrier_target_jours <- Calendrier_target$Jour_julien


                            tmp01 <- tmp00[r, ]

                            # Remarqué sur Petits Calvi 2019 février :
                            # La 1ere valeur de mars est un NA
                            # Donc je modifie mon r pour que la 1ere et la derniÃƒÂ¨re ligne
                            # ne soient pas des NA
                            k <- 0
                            while (is.na(tmp01[1,i])) {
                              k <- k + 1
                              r <- c(r[1] - 1, r)

                              Calendrier_target <- Calendrier[which(Calendrier$Date_decimale == min(tmp00$YearTS)):which(Calendrier$Date_decimale == max(tmp00$YearTS)), ]
                              Calendrier_target_jours <- Calendrier_target$Jour_julien

                              tmp01 <- tmp00[r, ]
                              if(k == 10) { break()}
                            }

                           k <- 0
                            while (is.na(tmp01[nrow(tmp01), i])) {
                              k <- k + 1

                              r <- c(r, r[length(r)] + 1)

                              Calendrier_target <- Calendrier[which(Calendrier$Date_decimale == min(tmp00$YearTS)):
                                which(Calendrier$Date_decimale == max(tmp00$YearTS)), ]
                              Calendrier_target_jours <- Calendrier_target$Jour_julien

                              tmp01 <- tmp00[r, ]
                              if(k == 10) { break()}
                            }
                            

                            Mi <- Moyenne_integree(x = tmp01[,i],
                                                   by = Calendrier_target[Calendrier_target$Date_decimale %in% tmp01$YearTS, "Jour_julien"],
                                                   from = min(Calendrier_target[Calendrier_target$Mois == target_month,"Jour_julien"]),
                                                   to = max(Calendrier_target[Calendrier_target$Mois == target_month,"Jour_julien"])
                            )
                            
                            tmp <- data.frame(Year = a, 
                                              Month = m, 
                                              Day = 15, 
                                              YearTS = decimal_date(as.Date(paste(a,m,15,sep="-"))),
                                              Site = s,
                                              Param = colnames(tmp01)[i], 
                                              Val = Mi)
                            
                            
                            return(tmp)
                            
                          }
                        )
                      )
                      
                      return(Out00)
                      
                    }
                  )
                )

                return(Out_a)
              }
            )
          )

          return(Out_s)
        } 
      )
    )
  return(Out)
}






















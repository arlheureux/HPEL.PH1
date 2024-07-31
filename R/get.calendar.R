## _________________ Infos générales
## Nom : get.calendar.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 19 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui crée un calendrier
#' @param from Numérique : année du début du calender au format yyyy
#' @param to Numérique : année du début du calender au format yyyy
#' @import lubridate
#' @return Le caldendrier
#
## ---------------------------

get.calendar <- function(from, to) {
  
  if(is.numeric(from) & is.numeric(to)){
    
    Date <- seq(date_decimal(from, tz = 'Europe/Paris'), date_decimal(to+1, tz = "Europe/Paris"), by = "day")
    Jour <- day(Date)
    Mois <- month(Date)
    Annee <- year(Date)
    Jour_julien <- as.numeric(julian(Date))
    Date_decimale <- decimal_date(Date)
  }
  
  
  out <- data.frame(Date_decimale = Date_decimale,
                    Jour = Jour,
                    Mois = Mois,
                    Annee = Annee,
                    Jour_julien = Jour_julien,
                    Date = Date)
  
  return(out)
  
  
}

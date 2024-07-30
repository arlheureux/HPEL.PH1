## _________________ Infos générales
## Nom : Conversion.taille.R
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
#' Fonction qui sort le tableau des tailles basé sur les pigments
#' @param data Tableau de données de pigments issu de PhytoCly
#' @return Le tableau de données des fractions et biomasses CHLA issues des pigments
#
## ---------------------------

Conversion.taille <- function(data){
  
  do.call(
    "rbind",
    lapply(
      1:nrow(data),
      function(x) {
        
        tmp <- data[x,]
        
        wDP <- 
          1.41*tmp$Fuco + 
          1.41*tmp$Peri + 
          0.60*tmp$Allo + 
          0.35*(tmp$`19'HF` - tmp$Buta) + 
          1.27*(tmp$`19'HF` - tmp$Buta) +
          0.86*tmp$Zea + 
          1.01*(tmp$`Tchl b`+tmp$`Divinyl chl a`-tmp$`Tchl b`)
        
        
        f_micro <- (1.41 * tmp$Fuco + 1.41* tmp$Peri) / wDP
        f_nano <- (0.60*tmp$Allo + 0.35*(tmp$`19'HF` - tmp$Buta) + 1.27*(tmp$`19'HF` - tmp$Buta) ) / wDP
        f_pico <- (0.86*tmp$Zea + 1.01*(tmp$`Tchl b`+tmp$`Divinyl chl a`-tmp$`Tchl b`)) / wDP
        
        CHLA_micro <- f_micro * tmp$`Tchl a`
        CHLA_nano <- f_nano * tmp$`Tchl a`
        CHLA_pico <- f_pico * tmp$`Tchl a`
        
        Grands <- CHLA_micro
        Petits <- CHLA_nano + CHLA_pico
        
        Grands_sur_Petits <- Grands / Petits
        
        out <- 
          data.frame(
            "Date" = tmp$YearTS,
            "wDP" = wDP,
            "f_micro" = f_micro,
            "f_nano" = f_nano,
            "f_pico" = f_pico,
            "CHLA_micro" = CHLA_micro,
            "CHLA_nano" = CHLA_nano,
            "CHLA_pico" = CHLA_pico,
            "Grands" = Grands,
            "Petits" = Petits,
            "Grands_sur_Petits" = Grands_sur_Petits
          )
        
        return(out)
        
      }
    )
  )
}

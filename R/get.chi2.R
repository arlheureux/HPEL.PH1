## _________________ Infos générales
## Nom : get.chi2.R
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
#' Fonction qui effectue un chi2 et exporte en un seul objet différents sorties
#' @param Freq.ano Le tableau de contingence
#' @param simulate.p.value cf fonction chisq.test
#' @param B : cf fonction chisq.test
#' @return Le résultats du chi2
#' @import stats
#
## ---------------------------

get.chi2 <- function(Freq.ano, simulate.p.value, B, ...) {
  
  chi2 <- chisq.test(Freq.ano, simulate.p.value = simulate.p.value, B = B, ...)
  chi2.p <- chi2$p.value
  chi2.x <- chi2$statistic
  # p > 0.05 = pas diff
  chi2E <- chi2$expected
  chi2E <- round(chi2E, 2) 
  
  
  res <- list(chi2 = chi2,
              p = chi2.p,
              x = chi2.x,
              Exp = chi2E)
  
  return(res)
  
}

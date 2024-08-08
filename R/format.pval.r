  ## _________________ Infos générales
  ## Nom : format.pval.R
  ## Auteur : Arnaud Lheureux
  ## Email: arnaud.lheureux@sorbonne-universite.fr
  ##
  ## Date de création : 27 juin 2024
  ##
  ## Copyright (c) Arnaud Lheureux, 2024
  ## R version : 4.4
  ## _________________

## _________________ Notes
#
#' Fonction qui format la pvalue pour les graphiques en < 0.05, < 0.01 et < 0.001
#' @param pval Valeur ou vecteur de pvalues
#' @return pvalues mises en forme
#' @export 
## ---------------------------

format.pval <- function(pval) {
    pval <- 
    ifelse(pval < 0.001, "< 0.001",
        ifelse(pval < 0.01 & pval >= 0.001, "< 0.01",
            ifelse(pval < 0.05 & pval >= 0.01, "< 0.05",
                paste0("= ", round(pval, 2))
            )
        )
    )
}

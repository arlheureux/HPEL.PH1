## _________________ Infos générales
## Nom : autoread.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 13 janvier 2023
##
## Copyright (c) Arnaud Lheureux, 2023
## R version : 4.2
## _________________

## _________________ Notes
#
#' Fonction qui lit automatique les fichiers en détectant tout
#' @param x Un fichier csv
#' @param verbose Logique : écrire les messages ou non
#' @return Plots des fréquences des anomalies
#' @import readr
#' @export
## ---------------------------

autoread <- function(x, verbose=F,...){
  
  
  seps <- c(";", ",", "\t")
  dec <- c(".", ",")
  enc <- invisible(detect_file_encoding(x, verbose=verbose))
  enc <- as.character(as.data.frame(enc$OUT_clean[1,"encoding"]))

  # détection du séparateur
  sep <- names(which.max(sapply(seps, function(y){
                                  a=tryCatch(ncol(read.csv(x, encoding = enc, sep=y, nrows = 10,...)), error=function(e) NULL)
                                  if(is.null(a)) a <- 0
                                  return(a)
                                }
  )))
  
  
  # détection de la décimale
  dat = read.csv(x, encoding = enc, sep=sep, nrows = 10, dec=".",...)
  
  d = sapply(1:nrow(dat), function(x) grep(pattern = "\\.", x = dat[x,]))
  d[sapply(d, length) == 0] <- 0
  
  cols.a.verif <- as.numeric(names(which(table(unlist(d)) == max(table(unlist(d)))))) # pas les 10 premières colonnes
  
  type <- names(which.max(table(sapply(cols.a.verif, function(x){class(dat[,x])}))))
  
  if(type == "numeric"){
    dec = "."
  }else{
    dec = ","
  }
  
  
  # lecture du fichier avec les bons paramètres
  dat = read.csv(x, encoding = enc, sep = sep, dec= dec,...)
  
  return(dat)
}

## _________________ Notes
#
#' Fonction qui détecte l'encodage d'un fichier
#' @param file_path Un fichier csv
#' @param verbose Logique : écrire les messages ou non
#' @return Plots des fréquences des anomalies
#' @import cli dplyr purrr readr stringi
#' @export
## ---------------------------

detect_file_encoding <- function(file_path, verbose = F) {

  # Read file in UTF-8 and detect encodings present
  file_raw = readr::read_file(file_path, locale = locale(encoding = "UTF-8"))
  encodings_found = stringi::stri_enc_detect(file_raw)
  
  # Function to read the file using all the encodings found
  try_all_encodings <- function(file_path, ENCODING) {
    
    FILE = read_file(file_path, locale = locale(encoding = ENCODING))
    HAS_BAD_CHARS = grepl("\u0086", FILE)
    
    if (!HAS_BAD_CHARS) {
      tibble(encoding = ENCODING, 
             content_file = list(FILE))
    } else {
      tibble(encoding = ENCODING, 
             content_file = list("BAD_CHARS detected"))
    }
    
  }
  
  # Safe version of function  
  try_all_encodings_safely = safely(try_all_encodings)
  
  # Loop through all the encodings
  OUT = 1:length(encodings_found[[1]]$Encoding) %>% 
    purrr::map(~ try_all_encodings_safely(file_path, encodings_found[[1]]$Encoding[.x]))
  
  # Create nested clean tibble with all the working encodings and contents 
  OUT_clean = 1:length(OUT) %>% purrr::map(~ OUT[[.x]]$result) %>% dplyr::bind_rows() %>% dplyr::left_join(encodings_found[[1]] %>% dplyr::as_tibble(), by = c("encoding" = "Encoding"))
  
  # Output list
  OUT_final = list(OUT_clean = OUT_clean)
  
  # Output message
  if(isTRUE(verbose)){
    cli::cli_alert_info("Found {nrow(OUT_clean)} potential encodings: {paste(OUT_clean$encoding)} \n")
  }
  
  return(OUT_final)
}






















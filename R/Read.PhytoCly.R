## _________________ Infos générales
## Nom : Read.PhytoCly.R
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
#' Fonction qui lit et charge les données PhytoCly
#' @param xlsx Le chemin vers les données
#' @param nsheet Le nombre de feuilles ÃƒÂ  prendre
#' @return Les données PhytoCly
#
## ---------------------------

Read.PhytoCly <- function(xlsx, nsheet){
  
  pb <- progress_bar$new(
    format = paste("Traitement de la feuille :what : [:bar] :percent Temps restant estimé: :eta (:spin)"),
    total = length(nsheet), clear = FALSE, width= 120
  )
  
  out <- do.call(
    "rbind",
    lapply(
      nsheet,
      function(x) {

        Sys.sleep(.1) # besoin pour la bar de progression
        
      
        tmp <- readxl::read_excel(xlsx, sheet = x, col_names = FALSE, .name_repair = "minimal")
        tmp <- as.data.frame(tmp)
        colnames <- c("Date", "Year", "Chl a", "Divinyl chl a", "Tchl a", "Peri", "Buta", "Fuco", 
                      "Neo", "Prasino", "Viola", "19'HF", "Allo", "Zea", "Lutein", "Tchl b")
        
        
        # Je cherche la ligne de colnames dans tmp row 1:10
        vec <- c()
        for(i in 1:10) {
          row_i <- tmp[i,]
          row_i <- unlist(as.vector(row_i))
          
          vec <- c(vec, sum(row_i %in% colnames, na.rm = TRUE))
        }
        
        row_colnames <- which.max(vec)
        New_colnames <- unname(unlist(tmp[row_colnames,]))
        
        
        # Je prends que les colonnes correspondantes
        col_ordered <- unlist(sapply(1:length(colnames), function(x) which(New_colnames == colnames[x])))
        
        # Je vérifie que personne ne manque 
        
        if(any(!1:length(colnames) %in% col_ordered)) {
          
          Combien <- sum(!1:length(colnames) %in% col_ordered)
          
          # Si un seul manquant :
          if(Combien == 1) {
            ARemettre <- colnames[which(!1:length(colnames) %in% col_ordered)]
            Pos_ARemettre <- grep(ARemettre, as.matrix(tmp))
            
            # Je retrouve sa position dans l'order afin de ne pas faire d'inversion
            Pos_col_ARemettre <- floor(1 + (Pos_ARemettre / nrow(tmp)))
            
            # Je le remets dans mon vecteur colnames
            New_colnames[which(is.na(New_colnames))] <- ARemettre
            col_ordered <- unlist(sapply(1:length(New_colnames), function(x) which(New_colnames == colnames[x])))
            
          }
          
          # Si plusieurs manquants :
          if(Combien > 1) {
            stop("Plusieurs colnames manquants, code ÃƒÂ  faire")
          }
          
        }
        
        
        
        # Et j'applique
        colnames(tmp) <- New_colnames
        tmp <- tmp[-c(1:row_colnames),]
        tmp <- tmp[,col_ordered]
        
        #  Check des lignes metadonnées
        # Je pars du principe que pour avoir une ligne, il me faut une date donc je vire tout ce qui est NA est date
        #NoDate <- which(is.na(tmp[,which(colnames == "Date")]))
        
        # A priori il y a / dans toutes les dates
        NoDate <- grep("/", unlist(tmp[,'Date']), invert = TRUE)
        tmp <- tmp[-c(NoDate),]
        
        
        # Vu en 2020 : Absence d'année, je corrige
        if(any(is.na(tmp$Year))) {
          
          uniqueYearNoNA <- as.numeric(unique(tmp$Year)[!is.na(unique(tmp$Year))])
          
          
          if(length(uniqueYearNoNA) == 1){
            
            tmp[which(is.na(tmp$Year)), "Year"] <- uniqueYearNoNA
            
          }else{
            stop("ProblÃƒÂ¨me avec les années manquantes / dupplicats")
          }
        }
        
        
        # J'en profite pour gérer mon format Date en créant une colonne date décimal : YearTS
        Date <- c()
        for(i in 1:nrow(tmp)){
          Date_tmp <- paste(tmp[i,"Date"], tmp[i,"Year"], sep="/")
          Date <- c(Date, Date_tmp)
        }
        tmp$YearTS <- decimal_date(as.Date(Date, format = "%d/%m/%Y"))
        tmp$Day <-  day(as.Date(Date, format = "%d/%m/%Y"))
        tmp$Month <- month(as.Date(Date, format = "%d/%m/%Y"))
        tmp$Year <- year(as.Date(Date, format = "%d/%m/%Y"))
        #tmp$Day_in_Year <- Day_in_year(as.Date(Date, format = "%d/%m/%Y"))
        tmp$Latitude <- 42.581788
        tmp$Longitude <- 8.727431

        
        
        
        
        
        # Je regarde mes colonnes et je passe en format numérique
        tmp2 <- tmp[,-which(colnames(tmp) %in% c("Date", "Year", "YearTS", "Month", "Day", "Latitude", "Longitude"))]
        tmp3 <- as.data.frame(tmp[,which(colnames(tmp) %in%  c("Year", "YearTS", "Month", "Day", "Latitude", "Longitude"))])
        tmp3 <- tmp3[,c("YearTS", "Day", "Month", "Year", "Latitude", "Longitude")]
        
        
        
        
        for(i in 1:ncol(tmp2)){

          col.n <- colnames(tmp2)[i]
          tmp2[, i] <- gsub("-", NA, tmp2[, i])
          tmp.vec <- as.numeric(unname(unlist(as.vector(tmp2[,i]))))
          
          # if(any(is.na(tmp.vec))) stop()  # pour débug manuellement
          tmp3[,col.n] <- tmp.vec
        }
        
        
        
        
        # Je reviens sur tmp2 ou j'enlÃƒÂ¨ve toutes les lignes pour lesquelles je n'ai aucun paramÃƒÂ¨tre
        FullNa <- which(sapply(1:nrow(tmp2), function(x) sum(is.na(tmp2[x,]))) == ncol(tmp2))
        if(length(FullNa) > 0){
          tmp3 <- tmp3[-FullNa,]
        }
        
        Site <- rep("Calvi", nrow(tmp3))
        tmp3 <- cbind(tmp3[,1:6], Site, tmp3[,7:ncol(tmp3)])
        
        
        # Je passe ÃƒÂ  la limite de détection en valeur minimale
        for(c in 8:ncol(tmp3)) {
          
          # Comme a chaque fois, s'il y a des NA ca plante...
          if(any(is.na(tmp3[,c]))) {tmp3[is.na(tmp3[,c]),c] <- 999999}
          if(any(tmp3[,c] < 0.01)) {tmp3[tmp3[,c] < 0.01, c] <- 0.01}
          if(any(tmp3[,c] == 999999)) {tmp3[tmp3[,c] == 999999, c] <- NA}
          
        }



        
        
        
        Sys.sleep(0.1)
        pb$tick(tokens = list(what=x))
        
        return(tmp3)
      }
      
    ))
  
  return(out)
}

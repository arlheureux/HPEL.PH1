## _________________ Infos générales
## Nom : Read.Pigments
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
#' @param nsheet Le nombre de feuilles à prendre
#' @return Les données PhytoCly
#' @importFrom progress progress_bar
#
## ---------------------------

Read.Pigments <- function(xlsx, nsheet, csv){
  

  # Données Calvi Anne

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
            stop("Plusieurs colnames manquants, code à faire")
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
            stop("Problème avec les années manquantes / dupplicats")
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
      
    )
  )

  # Ifremer :
  enc <- getOption("encoding")
  options(encoding = "utf-8")
  d <- autoread(csv)
  options(encoding = enc)

  # ## --> QUE ANSE BERTRAND
  # # qui a pico tot
  # pt <- d[d$Résultat...Paramètre...Code == "PICO-TOT-INF2",]
  # unique(pt$Lieu...Libellé)
  # # qui a nano tot
  # nt <- d[d$Résultat...Paramètre...Code == "NANO-TOT-SUP2", ]
  # unique(nt$Lieu...Libellé)

  d$Passage...Date <- as.Date(d$Passage...Date, format = "%d/%m/%Y")
  d <- d[d$Résultat...Niveau.de.qualité...Libellé %in% c("Bon", "Non qualifié"), ]

  # Quand plusieurs valeurs par jour :
  Niveaux <- c("Surface (0-1m)", "Mi-profondeur", "de 3 à 5 mètres", "Surface-Fond (profondeur <3 m)")
  d2 <- do.call(
    rbind,
    by(
      d,
      paste(d$Lieu...Mnémonique, d$Passage...Date),
      function(x) {
        # x <- data_rephy[data_rephy$Lieu...Mnémonique == '095-P-002' &
        # data_rephy$Passage...Date == '12/06/2017',]

        if (length(unique(x$Echantillon...Identifiant)) > 1) {
          # Si j'ai qu'un niveau renseigné
          if (length(unique(x$Prélèvement...Niveau)) == 1) {
            # je garde le plus en surface renseigné si c'est pas toujours le même
            if (any(!is.na(x$Prélèvement...Immersion)) & length(unique(x$Prélèvement...Immersion)) != 1) {
              x <- x[!is.na(x$Prélèvement...Immersion), ]
              x <- x[x$Prélèvement...Immersion == min(x$Prélèvement...Immersion, na.rm = TRUE), ]
              # Sinon je prends arbitraitement le plus petit n° échantillon
            } else {
              x <- x[!is.na(x$Echantillon...Identifiant.interne), ]
              x <- x[x$Echantillon...Identifiant.interne == min(x$Echantillon...Identifiant.interne, na.rm = TRUE), ]
            }
            # Si j'ai plus d'un niveau je prends le plus en surface
          } else {
            x <- x[x$Prélèvement...Niveau == Niveaux[Niveaux %in% x$Prélèvement...Niveau][1], ]
          }
          return(x)
        } else {
          return(x)
        }
      }
    )
  )

  d <- d2


  # Je formate mon tableau au format colonne
  colnames(d)
  head(d)
  d$YearTS <- decimal_date(d$Passage...Date)

  data.colonne <- d[, c(
    "YearTS", "Passage...Jour", "Passage...Mois", "Passage...Année", "Passage...Latitude..Max.",
    "Passage...Longitude..Max.", "Lieu...Libellé", "Résultat...Paramètre...Code", "Résultat...Valeur.quantitative"
  )]

  colnames(data.colonne) <- c("YearTS", "Day", "Month", "Year", "Latitude", "Longitude", "Site", "Param", "Val")
  head(data.colonne)

  x <- d$YearTS[1000]
  param <- sort(unique(data.colonne$Param))
  data.colonne2 <- do.call(
    "rbind",
    lapply(
      unique(data.colonne$YearTS),
      function(x) {
        tmp <- data.colonne[data.colonne$YearTS == x, ]

        # s <- unique(tmp$Site)[1]
        out1 <- do.call(
          "rbind",
          lapply(
            unique(tmp$Site),
            function(s) {
              tmp2 <- tmp[tmp$Site == s, ]
              base <- as.data.frame(matrix(NA, 1, length(param)))
              colnames(base) <- param

              for (p in unique(tmp2$Param)) {
                # Il peut rester 2 valeurs pour le même jour
                # j'ai identifié 2 méthodes différentes
                # notamment car parfois juste REPHY, parfois REPHY|SRN...
                # Je prends la médiane)
                base[, colnames(base) == p] <- median(tmp2[tmp2$Param == p, "Val"])
              }

              out2 <- cbind(tmp2[1, 1:7], base)

              return(out2)
            }
          )
        )


        return(out1)
      }
    )
  )
  rownames(data.colonne2) <- NULL

  head(data.colonne2)
  tail(data.colonne2)

  colnames(data.colonne2)[8:22] <- c("Allo", "Buta", "Chl a", "Chl b", "Divinyl chl a", "Fuco", "19'HF", "Lutein", "nano", "Peri", "pico", "Prasino", "Tchl b", "Viola", "Zea")


  data.colonne2$`Tchl a` <- sapply(1:nrow(data.colonne2), function(x) sum(data.colonne2[x, "Chl a"], data.colonne2[x, "Divinyl chl a"], na.rm = T))
 
  
  return(
    list(
      PhytoCly = out,
      Ifremer = data.colonne2
    )
  )
}

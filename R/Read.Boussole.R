## _________________ Infos générales
## Nom : Read.Boussole.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 24 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui lie et charge les données Boussoles
#' @param xlsx le chemin vers les données
#' @param sheet Le numéro de la feuille avec les données
#' @param year.max Année max a considérer
#' @return Les données boussole
#' @importFrom data.table uniqueN
#' @importFrom stringr str_sub str_to_sentence
#
## ---------------------------

Read.Boussole <- function(xlsx, sheet, year.max) {
  
  
  data <- readxl::read_xlsx(path = xlsx, sheet = sheet)
  #summary(data)
  #str(data)
  
  data <- as.data.frame(data)
  
  df <- data.frame(col = colnames(data),
                 nb_na = sapply(1:ncol(data),function(x) sum(data[,x]==-9)))
  
  
  # for(i in 1:ncol(data)){
  #   data[data[,i] == -9,i] <- NA
  # }
  
  data$Date <- as_date(paste(data$year, data$month, data$day, sep="-"))
  data$YearTS <- decimal_date(data$Date)
  #colnames(data)
  
  param <- c(
    "Peridinin", "19'-Butanoyloxyfucoxanthin", "Fucoxanthin", "Neoxanthin", "Prasinoxanthin", "Violaxanthin",
    "19'-Hexanoyloxyfucoxanthin", "Alloxanthin", "Zeaxanthin", "Chlorophyll_b",
    "TChlb", "Divinyl_Chlorophyll_a", "Chla", "Tchla", "picoChla", "nanoChla", "microChla"
  )
  
  data <- data[data$year <= year.max,]
  #decimal_minute_to_string(x = data$time, hour = F)
  
  # Enlever <- c()
  # for(i in unique(data$YearTS)){
  #   tmp <- data[data$YearTS == i,]
  #   
  #   for(j in unique(tmp$CTD)){
  #     tmp01 <- tmp[tmp$CTD == j,]
  #     
  #     if(uniqueN(tmp01$Depth) < 3) {
  #       enlever <- tmp01[tmp01$YearTS == i & tmp01$CTD  == j,"serialnumber"]
  #       Enlever <- c(Enlever, unlist(unname(c(enlever))))
  #     }
  #   }
  # }
  # 
  # data <- data[!data$serialnumber %in% Enlever,]
  
  #data <- data[,!colnames(data) %in% c("serialnumber")]
  
  
  i=unique(data$YearTS)[6]
  # i = 6 --> 1 seul CTD mais réplicats...
  Data_ok <- data.frame()
  for(i in unique(data$YearTS)){
    
    tmp <- data[data$YearTS == i,]
    
    
    # je donne un id de profil a tous les profils
    # cet id est basé sur la colonne serialnumber, afin d'ÃƒÂªtre unique 
    # et différent pour tous
    tmp$id.profil <- NA
    tmp$id.profil.done <- NA
    tmp$multi.profil <- NA
    
    # Parfois plusieurs profils sont présents le mÃƒÂªme jour :
    # identifiés dans la colonne CTD 
    # YearTS = 2003.258
    ct <- tmp$CTD
    ct <- unique(ct)
    
    # Parfois plusieurs profils sont présents le mÃƒÂªme jour :
    # identifiés dans la colonne station comme pour 
    # YearTS = 2003.104
    st <- tmp$STATION
    st <- unique(str_sub(st,1,5))
    #st <- unique(gsub("-.*", "", st))
    
    
    # Si 1 seul ct et 1 seul st alors je n'ai qu'un seul profil ce jour la
    if(uniqueN(ct) == 1 & uniqueN(st) == 1){
      tmp[,"id.profil"] <- as.data.frame(tmp[,"serialnumber"])[1,1]
      tmp[,"id.profil.done"] <- TRUE
      tmp[,"multi.profil"] <- FALSE
    }
    
    
    if(uniqueN(ct) > 1){
      for(c in unique(ct)){
        # je ne veux pas écraser l'id donc je mets un stop
        if(isTRUE(tmp[grep(c, tmp$STATION),"id.profil.done"])) stop("Tentative d'écrasage de l'id profil dans ct")
        
        tmp[grep(c, tmp$CTD),"id.profil"] <- as.data.frame(tmp[grep(c, tmp$CTD),"serialnumber"])[1,1]
        tmp[grep(c, tmp$CTD),"id.profil.done"] <- TRUE
        tmp[grep(c, tmp$CTD),"multi.profil"] <- TRUE
        
      }
    }
    
    
    
    
    if(uniqueN(st) > 1){
      for(s in unique(st)){
        # je ne veux pas écraser l'id donc je mets un stop
        if(isTRUE(tmp[grep(c, tmp$STATION),"id.profil.done"])) stop("Tentative d'écrasage de l'id profil dans st")
        
        tmp[grep(s, tmp$STATION),"id.profil"] <- as.data.frame(tmp[grep(s, tmp$STATION),"serialnumber"])[1,1]
        tmp[grep(s, tmp$STATION),"id.profil.done"] <- TRUE
        tmp[grep(s, tmp$STATION),"multi.profil"] <- TRUE
        
      }
    }
    
    
    
    if(any(is.na(tmp[,"id.profil.done"]))) stop("Pas d'attribution du profil")
    
    
    

    # tmp_melt <- melt(tmp[,-c(1:6, 8,9,10)], id.vars = c("YearTS", "id.profil", "Depth"))
    # 
    # ggplot(data= tmp_melt, aes(x=value, y=-Depth, col = as.factor(id.profil))) +
    #   geom_path() +
    #   facet_wrap(variable~., scales = "free")
    
    
    Data_ok <- rbind(Data_ok,tmp)
    
  }
  
  for(i in unique(Data_ok$id.profil)) {
    tmp <- Data_ok[Data_ok$id.profil == i,]
    Data_ok[Data_ok$id.profil == i,"profil.complet"] <- ifelse(any(tmp$Depth > 100), "TRUE", "FALSE")
  }
  
  
  
  
  #uniqueN(Data_ok$id.profil)
  #nrow(Data_ok[Data_ok$multi.profil == FALSE,])/nrow(Data_ok)
  

  
  # # Je jette un coup d'oeil aux multi-profils
  # tmp <- Data_ok[Data_ok$multi.profil == TRUE,]
  # tab <- table(tmp$YearTS, tmp$id.profil)
  # tab[tab > 0] <- 1
  # 
  # # Récap du nombre de profil par jour
  # apply(tab,1,sum)
  
  
  # Exportation des colonnes voulues
  Data_out <- Data_ok[,c(
    "Site", "YearTS", "year", "month", "Depth", "id.profil", "profil.complet", "latitude", "longitude", param)]
  colnames(Data_out) <- c(
    "Site", "YearTS", "Year", "Month", "Depth", "id.profil", "profil.complet", "Latitude", "Longitude", param)
  
  
  # Boussole pas écrit pareil au long de la période de données
  Data_out$Site <- str_to_sentence(Data_out$Site)
  Data_out <- Data_out[Data_out$Site == "Boussole",]
  
  for(i in param) {
    Data_out[Data_out[,i] == -1,i] <- 999999
    Data_out[Data_out[,i] == -9,i] <- 999999
    
    Data_out[Data_out[,i] == 999999,i] <- NA
  }
  
  return(Data_out)
}

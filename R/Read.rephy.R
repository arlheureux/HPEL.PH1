## _________________ Infos générales
## Nom : Read.rephy.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 25 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________


## _________________ Notes
#
#' Fonction qui groupes les tableaux
#' @param data_rephy Données rephy
#' @param data_phytobs Données phytobs
#' @param CodeTaxon Tableau de regroupement des UT
#' @param TF Base fonctionnelle
#' @return Les données phyto ok
#
## ---------------------------

Read.rephy <- function(data_rephy, data_phytobs, CodeTaxon, TF) {
 
  # Préparation des datasets
  # phytobs

  unique(data_phytobs$site)
  data_phytobs <- data_phytobs[data_phytobs$site %in% c("Marseille", "Sola", "Villefranche"), ]
  head(data_phytobs)
  colnames(data_phytobs)

  data_phytobs <- data_phytobs[!data_phytobs$dataset %in% c("Marseille SOFCOM Frioul-Maximum de fluorescence"), ]
  #data_phytobs2 <- data_phytobs[, c("site", "sampling_date", "chla..ug.l.", "chla_qv", "longitude", "latitude")]

  data_phytobs <- data_phytobs[, c("site", "sampling_date", "taxon_group", "taxon_group_count..cells.l.", "longitude", "latitude", "user_entered_taxon", "single_taxon_count..cells.l.")]
  colnames(data_phytobs) <- c("Site", "Date", "UT", "Nombre", "Longitude", "Latitude", "UT_user", "Nombre_user")
  data_phytobs$Date <- as_date(data_phytobs$Date, format = "%Y-%m-%d")


  data_phytobs_label <- data_phytobs[, c("Site", "Date", "UT", "Nombre", "Longitude", "Latitude")]
  data_phytobs_user <- data_phytobs[, c("Site", "Date", "UT_user", "Nombre_user", "Longitude", "Latitude")]

  data_phytobs_label <- data_phytobs_label[data_phytobs_label$UT != "", ]
  data_phytobs_label <- data_phytobs_label[!is.na(data_phytobs_label$Nombre), ]
  data_phytobs_user <- data_phytobs_user[data_phytobs_user$UT != "", ]
  data_phytobs_user <- data_phytobs_user[!is.na(data_phytobs_user$Nombre_user), ]


  # Préparation des datasets
  # rephy

  data_rephy <- data_rephy[data_rephy$Résultat...Niveau.de.qualité...Libellé %in% c("Bon", "Non qualifié"), ]
  Niveaux <- c("Surface (0-1m)", "Mi-profondeur", "de 3 à 5 mètres", "Surface-Fond (profondeur <3 m)")
  data_rephy <- data_rephy[data_rephy$Prélèvement...Niveau.de.prélèvement...Libellé %in% Niveaux, ]

  data_rephy <- do.call(
    rbind,
    by(
      data_rephy,
      paste(data_rephy$Lieu...Mnémonique, data_rephy$Passage...Date),
      function(x) {

        # x <- data_rephy[data_rephy$Lieu...Mnémonique == '095-P-002' &
        # data_rephy$Passage...Date == '12/06/2017',]

        if (length(unique(x$Echantillon...Identifiant)) > 1) {
          # Si j'ai qu'un niveau renseigné
          if (length(unique(x$Prélèvement...Niveau)) == 1) {
            # je garde le plus en surface renseigné si c'est pas toujours le mÃƒÂªme
            if (any(!is.na(x$Prélèvement...Immersion)) & length(unique(x$Prélèvement...Immersion)) != 1) {
              x <- x[!is.na(x$Prélèvement...Immersion), ]
              x <- x[x$Prélèvement...Immersion == min(x$Prélèvement...Immersion, na.rm = TRUE), ]
              # Sinon je prends arbitraitement le plus petit nÃ‚Â° échantiullon
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




  colnames(data_rephy)
  unique(data_rephy$Lieu...Libellé)
  data_rephy <- data_rephy[, c("Lieu...Libellé", "Passage...Date", "Résultat...Taxon.référent...Libellé", "Résultat...Valeur.quantitative", "Prélèvement...Longitude..Max.", "Prélèvement...Latitude..Max.")]
  colnames(data_rephy) <- c("Site", "Date", "UT", "Nombre", "Longitude", "Latitude")
  data_rephy$Date <- as_date(data_rephy$Date, format = "%d/%m/%Y")

  data_rephy[data_rephy$Site == "Antoine", "Site"] <- "Anse de Carteau 2"
  # data_rephy <- data_rephy[data_rephy$Site != "Ile du soleil",]


  # df <- data.frame(Taxons = sort(unique(data_phyto$UT)))
  # write.table(x = df, file = "Codes_taxons_HPEL.csv", sep = ";", row.names = FALSE)


  # Pour vérifier qu'on est à jour. Si rien ok, sinon les taxons suivants ne sont aps dans le fichier
  sort(unique(data_rephy$UT[which(!data_rephy$UT %in% CodeTaxon$Taxons)]))
  sort(unique(data_phytobs_label$UT[which(!data_phytobs_label$UT %in% CodeTaxon$Taxons)]))
  sort(unique(data_phytobs_user$UT[which(!data_phytobs_user$UT %in% CodeTaxon$Taxons)]))



  colnames(data_phytobs_user)[grep("*user*", colnames(data_phytobs_user))] <- c("UT", "Nombre")








# J'ai un pb avec TF
# Certains taxons sont doublés car plusieurs providers
# je ne garde qu'une ligne, et celle avec le moins de NYA (not yet annonced)


  TF2 <- TF[, c(2, 3, 13:22)]
  x="Chaetoceros didymus"
  TF2 <- do.call(
    "rbind",
    lapply(
      unique(TF2$Taxon),
      function(x) {
        tmp <- TF2[TF2$Taxon == x,]

        if(nrow(tmp) > 1) {
          tmp[tmp=="NYA"] <- NA
          tmp <- tmp[which.min(sapply(1:nrow(tmp), function(x) sum(is.na(tmp[x, ])))),]
        }
        return(tmp)
      }
    )
  )



  # data_phyto <- rbind(data_phytobs_label, data_phytobs_user, data_rephy)
  data_phyto <- rbind(data_phytobs_user, data_rephy)
  data_phyto <- merge(data_phyto, TF2, by.x = "UT", by.y = "Taxon", all.x = TRUE)


  data_phyto[which(!is.na(data_phyto$ProtozoaType)), "PhytoplanktonType"] <- data_phyto[which(!is.na(data_phyto$ProtozoaType)), "ProtozoaType"]
  data_phyto[which(!is.na(data_phyto$ProtozoaSize)), "PhytoplanktonSize"] <- data_phyto[which(!is.na(data_phyto$ProtozoaSize)), "ProtozoaSize"]
  data_phyto[which(!is.na(data_phyto$ProtozoaHabitat)), "PhytoHabitat"] <- data_phyto[which(!is.na(data_phyto$ProtozoaHabitat)), "ProtozoaHabitat"]
  data_phyto[which(!is.na(data_phyto$ProtozoaFeeding)), "PhytoFeedingMech"] <- data_phyto[which(!is.na(data_phyto$ProtozoaFeeding)), "ProtozoaFeeding"]

  data_phyto <- data_phyto[, !colnames(data_phyto) %in% c("ProtozoaType", "ProtozoaSize", "ProtozoaHabitat", "ProtozoaFeeding")]

  data_phyto <- merge(data_phyto, CodeTaxon, by.x = "UT", by.y = "Taxons")
  # View(data_phyto)

  # Mise en forme
  data_phyto$Year <- year(as.Date(data_phyto$Date, format = "%Y-%m-%d"))
  data_phyto$Month <- month(as.Date(data_phyto$Date, format = "%Y-%m-%d"))
  colnames(data_phyto)[colnames(data_phyto) == "Nombre"] <- "Val"
  # A priori nouveau : des résultats = 0, ~ 5000
  # Je les enlève !
  data_phyto <- data_phyto[data_phyto$Val != 0, ]


  # g=ggplot(data = data_phyto, aes(x = Date, y = log10(1+Val))) +
  #   geom_line() +
  #   geom_vline(xintercept = as_date("2015-01-01"), col = "red", lty = 2) +
  #   facet_grid(Site~Param, scales='free') +
  #   theme_bw()

  # gg_png(GG_plot = g, filename = "Séries_rephy.png", path = OUT)

  # Je retourne une liste avec différents data.frame
  # un pour les classes de taille
  # un pour diato/dino
  # un pour diato tox / diato tot
  # un pour dino tox / dino tot
  # un tableau avec tout pour gérer les UT et les afficher quand besoin
















  # Préparation du tableau classes de taille
  data_phyto_ct <- data_phyto[, !colnames(data_phyto) %in% "Class"]
  colnames(data_phyto_ct)[colnames(data_phyto_ct) == "Classe_Taille"] <- "Param"
  data_phyto_ct <- data_phyto_ct[!is.na(data_phyto_ct$Param), ]

  # Préparation du tableau diato / dino
  data_phyto_bd <- data_phyto[, !colnames(data_phyto) %in% "Classe_Taille"]
  colnames(data_phyto_bd)[colnames(data_phyto_bd) == "Class"] <- "Param"
  data_phyto_bd <- data_phyto_bd[data_phyto_bd$Param %in% c("Bacillariophyceae", "Dinophyceae"), ]
  data_phyto_bd <- data_phyto_bd[!is.na(data_phyto_bd$Param), ]


  # Préparation du tableau diato tox
  data_phyto_dt <- data_phyto[, !colnames(data_phyto) %in% "Classe_Taille"]
  colnames(data_phyto_dt)[colnames(data_phyto_dt) == "Class"] <- "Param"
  data_phyto_dt <- data_phyto_dt[data_phyto_dt$Param %in% "Bacillariophyceae", ]
  data_phyto_dt[data_phyto_dt$Toxic_Nuisance %in% c("Nuisance", "Ambigous"), "Param"] <- "Bacillariophyceae toxiques"
  data_phyto_dt <- data_phyto_dt[!is.na(data_phyto_dt$Param), ]

  # Préparation du tableau dino tox
  data_phyto_dtt <- data_phyto[, !colnames(data_phyto) %in% "Classe_Taille"]
  colnames(data_phyto_dtt)[colnames(data_phyto_dtt) == "Class"] <- "Param"
  data_phyto_dtt <- data_phyto_dtt[data_phyto_dtt$Param %in% "Dinophyceae", ]
  data_phyto_dtt[data_phyto_dtt$Toxic_Nuisance %in% c("Nuisance", "Ambigous"), "Param"] <- "Dinophyceae toxiques"
  data_phyto_dtt <- data_phyto_dtt[!is.na(data_phyto_dtt$Param), ]

  # Préparation du tableau UT
  data_phyto_ut <- data_phyto[, !colnames(data_phyto) %in% "Classe_Taille"]
  colnames(data_phyto_ut)[colnames(data_phyto_ut) == "Code"] <- "Param"
  data_phyto_ut <- data_phyto_ut[!is.na(data_phyto_ut$Param), ]


  return(list(
    data_phyto_ut = data_phyto_ut,
    data_phyto_ct = data_phyto_ct,
    data_phyto_bd = data_phyto_bd,
    data_phyto_bt = data_phyto_dt,
    data_phyto_dt = data_phyto_dtt
  ))
}

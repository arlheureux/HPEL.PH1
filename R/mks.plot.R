## _________________ Infos générales
## Nom : mks.plot.R
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
#' Fonction qui fait les plots de mann kendall saisonniers
#' @param Data_full Tableau de données brutes
#' @param Data_TS Tableau de données mensuelles
#' @param mks Les résultats du mann kendall saisonnier
#' @param col.param Numérique qui donne les colonnes de données
#' @param path Chemin de sauvegarde des images
#' @param melt Logique afin de transformer ou pas les données burtes (pourrait ÃƒÂªtre fait avant la fonction..)
#' @param ds Dataset : spécifier de quelle origine sont les données afin de configurer les unités
#' @param Data_full_UT Tableau de données brutes avec les UT en param
#' @param Data_TS_UT_ok Tableau de données mensuelles avec les UT en param
#' @param res.mks.UT Les résultats du mann kendall saisonnier avec les UT en param
#' @param groupe Le type de la base de données utilisée
#' @return Des plots
#' @import ggh4x ggrepel cowplot
#' @export
#
## ---------------------------



mks.plot <- function(Data_full, Data_TS, mks, col.param, path, melt = TRUE, ds, Data_full_UT, Data_TS_UT_ok, res.mks.UT, groupe = NA, year.max) {
  # Mise en place de la bar de progression :
  total <- paste(Data_TS$Site, Data_TS$Param)
  pb <- progress_bar$new(
    format = paste(":list | :site - :param : [:bar] :percent Temps restant estimé: :eta (:spin)"),
    total = uniqueN(total), clear = FALSE, width = 120, show_after = 0
  )

  # Pour nommer dans la barre de progression
  l <-
    ifelse(groupe == "data_phyto_ut", "Unités taxinomiques",
      ifelse(groupe == "data_phyto_ct", "Classes de taille",
        ifelse(groupe == "data_phyto_bd", "Bacillariophyceae et Dinophyceae",
          ifelse(groupe == "data_phyto_bt", "Bacillariophyceae toxiques et Bacillariophyceae totales",
            ifelse(groupe == "data_phyto_dt", "Dinophyceae toxiques et Dinophyceae totales", 
             ifelse(groupe == "Pigments", "Pigments", "NA")
             )
          )
        )
      )
    )





  for (s in unique(Data_TS$Site)) {
    tmp <- Data_TS[Data_TS$Site == s, ]

    for (x in unique(tmp$Param)) {
      # cat("On traite le paramÃƒÂ¨tre :", x, "au site :", s, "\n\n")


      # Mise en place de la bar de progression :
      Sys.sleep(0.01)
      pb$tick(tokens = list(site = s, param = x, list = l))



      data.x <- mks[[paste(x, s, sep = "_")]]$data.x
      mksm <- mks[[paste(x, s, sep = "_")]]$mksm

      data.x$Month <- as.factor(data.x$Month)
      data.x$Month <- Mois.text(data.x$Month)

      Sen_df <- mksm.get.seasonal.estimate(mksm)
      Sen_df <- mksm.fill.Sen.df(data.x, Sen_df, min(data.x$Year), year.max, Month = TRUE)

      # write.table(x = Sen_df, file = "Pentes_TCHLA_Calvi.csv", row.names = F, sep=";")

      Sen_moy <- mksm.get.seasonal.estimate(mksm)[, 1:2]
      Sen_moy <- mksm.fill.Sen.df(data.x, Sen_moy, min(data.x$YearTS), (year.max + 1) - 1 / 12, Month = FALSE)

      #

      Sen_df$x[rep(mksm$p.value.s[2:13] >= 0.05, each = 2)] <- NA
      Sen_df$y[rep(mksm$p.value.s[2:13] >= 0.05, each = 2)] <- NA

      Sen_moy$x[rep(median(mksm$p.value.s[2:13]) >= 0.05, each = 2)] <- NA
      Sen_moy$y[rep(median(mksm$p.value.s[2:13]) >= 0.05, each = 2)] <- NA



      pval <- ifelse(mksm$p.value.s[2:13] < 0.001, "< 0.001",
        ifelse(mksm$p.value.s[2:13] < 0.01 & mksm$p.value.s[2:13] >= 0.001, "< 0.01",
          ifelse(mksm$p.value.s[2:13] < 0.05 & mksm$p.value.s[2:13] >= 0.01, "< 0.05",
            paste0(" = ", round(mksm$p.value.s[2:13], 2))
          )
        )
      )

      pval_moy <- median(mksm$p.value.s[2:13])
      pval_moy <- ifelse(pval_moy < 0.001, "< 0.001",
        ifelse(pval_moy < 0.01 & pval_moy >= 0.001, "< 0.01",
          ifelse(pval_moy < 0.05 & pval_moy >= 0.01, "< 0.05",
            paste0("= ", round(pval_moy, 2))
          )
        )
      )

      lab_df <- data.frame(
        lab = paste0(
         # "Pente Sen = ", round(mksm$seasonal.estimates[, "slope"], 4),
          "p ", pval,
          #"\n% Chgmt période = ", Sen_df$tag[seq(2, nrow(Sen_df), 2)],
          ", Chgmt annuel = ", Sen_df$taa[seq(2, nrow(Sen_df), 2)],"%"
        ),
        Month = unique(data.x$Month)
      )

      lab_df <- lab_df[which(mksm$p.value.s[2:13] < 0.05), ]

      lab_moy_df <- data.frame(
        lab = paste0(
          #"Pente Sen = ", median(round(mksm$seasonal.estimates[, "slope"], 4)),
          "p ", pval_moy,
          #"\n% Chgmt période = ", Sen_moy$tag[1],
          ", Chgmt annuel = ", Sen_moy$taa[1],"%"
        )
      )
      lab_moy_df[which(median(mksm$p.value.s[2:13]) >= 0.05), c("lab")] <- NA


      sig <- unname(which(mksm$p.value.s[-1] < 0.05))
      # strip.fill <- 1:12
      strip.fill2 <- rep("grey90", 12)
      strip.fill2[sig] <- "grey60"

      strip.fill2.moy <- ifelse(median(mksm$p.value.s[-1] < 0.05), "grey60", "grey90")

      strip <- strip_themed(background_x = elem_list_rect(fill = strip.fill2))
      strip.moy <- strip_themed(background_x = elem_list_rect(fill = strip.fill2.moy))

      param <- revalue(x,
        warn_missing = FALSE,
        replace = c(
          "Allo" = "Alloxanthine",
          "Peri" = "Péridinine",
          "Fuco" = "Fucoxanthine",
          "Prasino" = "Prasinoxanthine",
          "CHLA_micro" = "Chlorophylle-a",
          "CHLA_nano" = "Chlorophylle-a",
          "CHLA_pico" = "Chlorophylle-a",
          "Chla a" = "Chlorophylle-a",
          "Divinyl chl a" = "Divinyl Chlorophylle-a",
          "Tchl a" = "Chlorophylle-a totale",
          "Buta" = "19Ã¢â‚¬â„¢-Butanoyloxyfucoxanthine",
          "Neo" = "Neoxanthine",
          "Viola" = "Violaxanthine",
          "19'HF" = "19Ã¢â‚¬â„¢-Hexanoyloxy-fucoxanthine",
          "Zea" = "Zéaxanthine",
          "Lutein" = "Luteine",
          "Tchl b" = "Chlorophylle-b totale",
          "wDP" = "wDP",
          "Alloxanthin" = "Alloxanthine",
          "Peridinin" = "Péridinine",
          "Fucoxanthin" = "Fucoxanthine",
          "Prasinoxanthin" = "Prasinoxanthine",
          "microChla" = "Chlorophylle-a",
          "nanoChla" = "Chlorophylle-a",
          "picoChla" = "Chlorophylle-a",
          "Chla" = "Chlorophylle-a",
          "Tchla" = "Chlorophylle-a totale",
          "19'-Butanoyloxyfucoxanthin" = "19Ã¢â‚¬â„¢-Butanoyloxyfucoxanthine",
          "Neoxanthin" = "Neoxanthine",
          "Violaxanthin" = "Violaxanthine",
          "19'-Hexanoyloxyfucoxanthin" = "19Ã¢â‚¬â„¢-Hexanoyloxy-fucoxanthine",
          "Zeaxanthin" = "Zéaxanthine",
          "Tchlb" = "Chlorophylle-b totale",
          "Chlorophyll_b" = "Chlorophylle-b",
          "DP" = "wDP"
        )
      )



      if (isTRUE(melt)) {
        Data_full_melt <-
          reshape2::melt(
            Data_full[, colnames(Data_full) %in% c(colnames(Data_full)[col.param], "YearTS", "Site")],
            id.vars = c("YearTS", "Site")
          )
      } else {
        Data_full_melt <- Data_full
        colnames(Data_full_melt)[colnames(Data_full_melt) == "Param"] <- "variable"
        colnames(Data_full_melt)[colnames(Data_full_melt) == "Val"] <- "value"
      }

      Data_full_melt <- Data_full_melt[Data_full_melt$variable == x & Data_full_melt$Site == s &
        Data_full_melt$YearTS < year.max + 1, ]
      Data_full_melt$Month <- month(date_decimal(Data_full_melt$YearTS, tz = "Europe/Paris"))
      Data_full_melt$Month <- revalue(as.factor(Data_full_melt$Month),
        replace = c(
          "1" = "Janvier",
          "2" = "Février",
          "3" = "Mars",
          "4" = "Avril",
          "5" = "Mai",
          "6" = "Juin",
          "7" = "Juillet",
          "8" = "Août",
          "9" = "Septembre",
          "10" = "Octobre",
          "11" = "Novembre",
          "12" = "Décembre"
        )
      )


      if (length(seq(min(data.x$Year), max(data.x$Year))) < 15) {
        br <- seq(min(data.x$Year), max(data.x$Year))[seq(min(data.x$Year), max(data.x$Year)) %% 2 == 0]
      } else {
        br <- seq(min(data.x$Year), max(data.x$Year))[seq(min(data.x$Year), max(data.x$Year)) %% 5 == 0]
      }

      if (ds == "Rephy") {
        unit <- bquote("(cell.L"^"-1" * ")")
      } else {
        unit <- bquote("(ng.L"^"-1" * ")")
      }


      if(median(mksm$p.value.s[-1]) < 0.05){
        Data_full_melt$Site2 <- paste(Data_full_melt$Site, lab_moy_df, sep = "\n")
        data.x$Site2 <- paste(data.x$Site, lab_moy_df, sep = "\n")
      }else{
        Data_full_melt$Site2 <- Data_full_melt$Site
        data.x$Site2 <- data.x$Site
      }

      G1 <- ggplot() +
        geom_vline(xintercept = year.max - duree.eval + 1, col = "#ff8a92", lty = 2) +
        geom_point(data = Data_full_melt, aes(YearTS, value), alpha = .5) + # ici tous les points avant régularisation - les outliers
        geom_line(data = data.x, aes(YearTS, fill), col = "#FF9A00", linewidth = 1.2) +
        xlab("Années") +
        facet_wrap2(Site2~., strip = strip.moy) +
        scale_x_continuous(breaks = br, limits = c(min(data.x$Year), (year.max + 1))) +
        #ylab(bquote(.(param) ~ .(unit))) +
        ylab("") +
        ylim(c(
          min(Data_full_melt$value, na.rm = TRUE) - .25 * min(Data_full_melt$value, na.rm = TRUE),
          max(Data_full_melt$value, na.rm = TRUE) + .25 * max(Data_full_melt$value, na.rm = TRUE)
        )) + 
        geom_line(data = Sen_moy, aes(x = x, y = y), lwd = 1.2, col = "#7ABA78", lty = 2) +
        # geom_label(data = lab_moy_df, aes(x = (year.max + min(data.x$Year)) / 2, y = max(Data_full_melt$value, na.rm = TRUE), label = lab), size = 3, fill = "white", alpha = .5) + 
        theme_bw()

      G1



      # je modifie pour mettre la pval et le % dans le strip
      data.x$Month2 <- gsub("NA", "", paste(data.x$Month, lab_df[match(data.x$Month, lab_df$Month), "lab"], sep = "\n"))
      data.x$Month2 <- factor(data.x$Month2, levels = data.x[1:12, "Month2"])

      Data_full_melt$Month2 <- gsub("NA", "", paste(Data_full_melt$Month, lab_df[match(Data_full_melt$Month, lab_df$Month), "lab"], sep = "\n"))
      Data_full_melt$Month2 <- factor(Data_full_melt$Month2, levels = data.x[1:12, "Month2"])


      Sen_df$Month2 <- gsub("NA", "", paste(Sen_df$Month, lab_df[match(Sen_df$Month, lab_df$Month), "lab"], sep = "\n"))
      Sen_df$Month2 <- factor(Sen_df$Month2, levels = data.x[1:12, "Month2"])



      G2 <- ggplot() +
        geom_vline(xintercept = year.max - duree.eval + 1, col = "#ff8a92", lty = 2) +
        geom_point(data = Data_full_melt, aes(floor(YearTS), value), alpha = .5) + # ici tous les points avant régularisation - les outliers
        geom_line(data = data.x, aes(floor(YearTS), fill), col = "#FF9A00", linewidth = 1.2) + # ici données zea regularisee et completee
        ggh4x::facet_wrap2(Month2 ~ ., scales = "free", strip = strip, ncol = 3, axes = FALSE, remove_labels = TRUE) +
        xlab("Années") +
        scale_x_continuous(breaks = br, limits = c(min(data.x$Year), (year.max))) +
        # ylab(bquote("Log"[10]~"1 +"~.(param)*" (ng.L"^"-1"*")")) +
        ylab(bquote(.(param) ~ .(unit))) +
        ylim(c(
          min(Data_full_melt$value, na.rm = TRUE) - .25 * min(Data_full_melt$value, na.rm = TRUE),
          max(Data_full_melt$value, na.rm = TRUE) + .25 * max(Data_full_melt$value, na.rm = TRUE)
        )) +
        geom_line(data = Sen_df, aes(x = x, y = y), lwd = 1.2, col = "#7ABA78", lty = 2) +
        # geom_label(data = lab_df, aes(x = (year.max + min(data.x$Year)) / 2, y = max(Data_full_melt$value, na.rm = TRUE), label = lab), size = 3, fill = "white", alpha = .5) +
        theme_bw() +
        theme(strip)
      G2

      if (x == "nanoChla") {
        G2 <- G2 + ylab(bquote(.(param)[nano] * " (ng.L"^"-1" * ")"))
      }
      if (x == "microChla") {
        G2 <- G2 + ylab(bquote(.(param)[micro] * " (ng.L"^"-1" * ")"))
      }
      if (x == "picoChla") {
        G2 <- G2 + ylab(bquote(.(param)[pico] * " (ng.L"^"-1" * ")"))
      }

      # Je récupÃƒÂ¨re la taille des facets
      # g <- ggplot_gtable(ggplot_build(G2))





      # Je fais ma carte
      Data_full$Longitude <- as.numeric(gsub(",", ".", Data_full$Longitude))
      Data_full$Latitude <- as.numeric(gsub(",", ".", Data_full$Latitude))
      coord <- Data_full[!duplicated(Data_full$Longitude), c("Site", "Longitude", "Latitude")]

      coord <- st_as_sf(coord, coords = c(2:3))
      coord <- st_set_crs(x = coord, value = 4326)
      bbox_proj <- as.data.frame(st_coordinates(st_transform(coord, crs = "epsg:2154")))
      bbox_proj$Site <- coord$Site
      bbox_proj <- bbox_proj[!duplicated(bbox_proj$Site), ]


      xRange <- range(Data_full$Longitude)
      yRange <- range(Data_full$Latitude)

      if(diff(xRange) == 0) {
        xRange <- extra.range(xRange, .5)
      }

      if (diff(yRange) == 0) {
        yRange <- extra.range(yRange, .05)
      }

      gmap <-
        Map.ggplot(xlim = xRange, ylim = yRange) +
        geom_point(data = bbox_proj, aes(x = X, y = Y), size = 5, col = "firebrick") +
        geom_point(data = bbox_proj[bbox_proj$Site == s, ], aes(x = X, y = Y), size = 8, col = "green") +
        geom_label_repel(
          data = bbox_proj[bbox_proj$Site == s, ], aes(x = X, y = Y, label = Site),
          label.padding = .5, box.padding = 1, size = 6, alpha = .8
        ) +
        labs(x = "Longitude", y = "Latitude")
      gmap


      # Si un mois est sig alors je vais chercher les UT pour ce mois
      if (any(!is.na(Sen_df$y)) & ds == "Rephy" & !x %in% c("micro", "nano", "pico") & l != "Unités taxinomiques") {

        if (x == "Dinophyceae") {
          # Alors je suis dans les données avec que les Baci et les Dino
          Data_full_UT_x <- Data_full_UT[Data_full_UT$Class == x & Data_full_UT$Site == s, ]
        }

        if (x == "Bacillariophyceae") {
          # Alors je suis dans les données avec que les Baci et les Dino
          Data_full_UT_x <- Data_full_UT[Data_full_UT$Class == x & Data_full_UT$Site == s, ]
        }

        if (x == "Bacillariophyceae toxiques") {
          # Alors je suis dans les données avec que les Baci et les Dino
          Data_full_UT_x <- Data_full_UT[Data_full_UT$Class == "Bacillariophyceae" & Data_full_UT$Site == s &
            Data_full_UT$Toxicite == TRUE, ]
        }

        if (x == "Dinophyceae toxiques") {
          # Alors je suis dans les données avec que les Baci et les Dino
          Data_full_UT_x <- Data_full_UT[Data_full_UT$Class == "Dinophyceae" & Data_full_UT$Site == s &
            Data_full_UT$Toxicite == TRUE, ]
        }



        plot.list <- list()
        k <- 0
        data.x.UT2 <- data.frame()
        Data_full_melt.UT2 <- data.frame()
        data.x.UT2 <- data.frame()
        Sen_df.UT2 <- data.frame()
        lab_df.UT2 <- data.frame()

        for (u in na.omit(unique(Data_full_UT_x$Param))) {
          data.x.UT <- res.mks.UT[[paste(u, s, sep = "_")]]$data.x
          mksm.UT <- res.mks.UT[[paste(u, s, sep = "_")]]$mksm


          if (is.null(nrow(data.x.UT))) {
            next
          }

          data.x.UT$Month <- as.factor(data.x.UT$Month)
          data.x.UT$Month <- revalue(data.x.UT$Month,
            replace = c(
              "1" = "Janvier",
              "2" = "Février",
              "3" = "Mars",
              "4" = "Avril",
              "5" = "Mai",
              "6" = "Juin",
              "7" = "Juillet",
              "8" = "Août",
              "9" = "Septembre",
              "10" = "Octobre",
              "11" = "Novembre",
              "12" = "Décembre"
            )
          )

          Sen_df.UT <- mksm.get.seasonal.estimate(mksm.UT)
          Sen_df.UT <- mksm.fill.Sen.df(data.x.UT, Sen_df.UT, min(data.x.UT$Year), year.max)


          Sen_df.UT$x[rep(mksm.UT$p.value.s[2:13] >= 0.05, each = 2)] <- NA
          Sen_df.UT$y[rep(mksm.UT$p.value.s[2:13] >= 0.05, each = 2)] <- NA

          # je garde que les mois ou la classe est significative
          Sen_df.UT[which(is.na(Sen_df$y)), c("x", "y")] <- NA

          if (sum(is.na(Sen_df.UT$x)) == nrow(Sen_df.UT)) {
            next
          }


          pval.UT <- ifelse(mksm.UT$p.value.s[2:13] < 0.001, "< 0.001",
            ifelse(mksm.UT$p.value.s[2:13] < 0.01 & mksm.UT$p.value.s[2:13] >= 0.001, "< 0.01",
              ifelse(mksm.UT$p.value.s[2:13] < 0.05 & mksm.UT$p.value.s[2:13] >= 0.01, "< 0.05",
                paste0(" = ", round(mksm.UT$p.value.s[2:13], 2))
              )
            )
          )


          lab_df.UT <- data.frame(
            lab = paste0(
              #"Pente Sen = ", round(mksm.UT$seasonal.estimates[, "slope"], 4),
              "p ", pval,
              #"\n% Chgmt période = ", Sen_df.UT$tag[seq(2, nrow(Sen_df), 2)],
              ", Chgmt annuel = ", Sen_df.UT$taa[seq(2, nrow(Sen_df), 2)], "%"
            ),
            Month = unique(data.x.UT$Month)
          )

          lab_df.UT <- lab_df.UT[unique(as.numeric(Sen_df.UT[which(!is.na(Sen_df.UT$x)), "Month"])), ]

          data.x.UT <- data.x.UT[data.x.UT$Month %in% lab_df.UT$Month, ]

          Data_full_melt.UT <- Data_full_UT
          colnames(Data_full_melt.UT)[colnames(Data_full_melt.UT) == "Param"] <- "variable"
          colnames(Data_full_melt.UT)[colnames(Data_full_melt.UT) == "Val"] <- "value"

          Data_full_melt.UT <- Data_full_melt.UT[Data_full_melt.UT$variable == u & Data_full_melt.UT$Site == s &
            Data_full_melt.UT$YearTS < year.max + 1, ]
          Data_full_melt.UT$Month <- month(date_decimal(Data_full_melt.UT$YearTS, tz = "Europe/Paris"))
          Data_full_melt.UT$Month <- revalue(as.factor(Data_full_melt.UT$Month),
            replace = c(
              "1" = "Janvier",
              "2" = "Février",
              "3" = "Mars",
              "4" = "Avril",
              "5" = "Mai",
              "6" = "Juin",
              "7" = "Juillet",
              "8" = "Août",
              "9" = "Septembre",
              "10" = "Octobre",
              "11" = "Novembre",
              "12" = "Décembre"
            )
          )

          Sen_df.UT <- Sen_df.UT[!is.na(Sen_df.UT$x), ]

          Sen_df.UT$Month <- paste(u, Sen_df.UT$Month, sep = "\n")
          Data_full_melt.UT$Month <- paste(u, Data_full_melt.UT$Month, sep = "\n")
          data.x.UT$Month <- paste(u, data.x.UT$Month, sep = "\n")
          lab_df.UT$Month <- paste(u, lab_df.UT$Month, sep = "\n")


          for(i in lab_df.UT$Month){
        
            i <- gsub("\\+", "\\\\+", i)

            new.lab <- lab_df.UT[grep(i, lab_df.UT$Month), "lab"]

            Sen_df.UT[grep(i, Sen_df.UT$Month), "Month2"] <- paste(Sen_df.UT[grep(i, Sen_df.UT$Month), "Month"], new.lab, sep = "\n")
            Data_full_melt.UT[grep(i, Data_full_melt.UT$Month), "Month2"] <- paste(Data_full_melt.UT[grep(i, Data_full_melt.UT$Month), "Month"], new.lab, sep = "\n")
            data.x.UT[grep(i, data.x.UT$Month), "Month2"] <- paste(data.x.UT[grep(i, data.x.UT$Month), "Month"], new.lab, sep = "\n")
            lab_df.UT[grep(i, lab_df.UT$Month), "Month2"] <- paste(lab_df.UT[grep(i, lab_df.UT$Month), "Month"], new.lab, sep = "\n")
          }


          for (uu in unique(Sen_df.UT$Month)) {
            k <- k + 1

            Data_full_melt.UT2 <- rbind(Data_full_melt.UT2, Data_full_melt.UT[Data_full_melt.UT$Month == uu, ])
            data.x.UT2 <- rbind(data.x.UT2, data.x.UT[data.x.UT$Month == uu, ])
            Sen_df.UT2 <- rbind(Sen_df.UT2, Sen_df.UT[Sen_df.UT$Month == uu, ])
            lab_df.UT2 <- rbind(lab_df.UT2, lab_df.UT[lab_df.UT$Month == uu, ])

            # gU <- ggplot() +
            #   geom_vline(xintercept = year.max - duree.eval + 1, col = "#ff8a92", lty = 2) +
            #   geom_point(data = Data_full_melt.UT[Data_full_melt.UT$Month == uu, ], aes(floor(YearTS), value), alpha = .5) + # ici tous les points avant régularisation - les outliers
            #   geom_line(data = data.x.UT[data.x.UT$Month == uu, ], aes(floor(YearTS), fill), col = col.if, size = 1.2) + # ici données zea regularisee et completee
            #   facet_wrap2(Month ~ ., scales = "free", ncol = 2) +
            #   xlab("Années") +
            #   scale_x_continuous(breaks = br, limits = c(min(Data_full_melt.UT$Year), (year.max+1))) +
            #   # ylab(bquote("Log"[10]~"1 +"~.(param)*" (ng.L"^"-1"*")")) +
            #   # ylab(bquote(.(u) ~ .(unit))) +
            #   ylab("") +
            #   ylim(c(
            #     min(Data_full_melt$value, na.rm = TRUE) - .25 * min(Data_full_melt$value, na.rm = TRUE),
            #     max(Data_full_melt$value, na.rm = TRUE) + .25 * max(Data_full_melt$value, na.rm = TRUE)
            #   )) +
            #   geom_line(data = Sen_df.UT[Sen_df.UT$Month == uu, ], aes(x = x, y = y), lwd = 1.2, col = "#7ABA78", lty = 2) +
            #   geom_label(
            #     data = lab_df.UT[lab_df.UT$Month == uu, ], aes(x = (year.max + year.min) / 2, y = max(Data_full_melt$value, na.rm = TRUE), label = lab),
            #     size = 2.5, fill = "white", alpha = .5
            #   ) +
            #   theme_bw()






            plot.list[[k]] <- data.x.UT2
          }
        }

        # gestion de l'ordre des plots
        unique(Data_full_melt.UT2$Month)
        nchar(unique(Data_full_melt.UT2$Month))


        if (length(plot.list) > 0) {
          Data_full_melt.UT2$Month2 <- factor(Data_full_melt.UT2$Month2, levels = unique(Data_full_melt.UT2$Month2))
          data.x.UT2$Month2 <- factor(data.x.UT2$Month2, levels = unique(data.x.UT2$Month2))
          Sen_df.UT2$Month2 <- factor(Sen_df.UT2$Month2, levels = unique(Sen_df.UT2$Month2))
          lab_df.UT2$Month2 <- factor(lab_df.UT2$Month2, levels = unique(lab_df.UT2$Month2))


          data.x.UT2 <- merge(data.x.UT2, Sen_df.UT2, by = "Month2")

          data.x.UT2[data.x.UT2$taa > 0, "col.if"] <- "blue"
          data.x.UT2[data.x.UT2$taa < 0, "col.if"] <- "red"

          Data_full_melt.UT2 <- Data_full_melt.UT2[!is.na(Data_full_melt.UT2$Month2), ]
          data.x.UT2 <- data.x.UT2[!is.na(data.x.UT2$Month2), ]
          Sen_df.UT2 <- Sen_df.UT2[!is.na(Sen_df.UT2$Month2), ]
          lab_df.UT2 <- lab_df.UT2[!is.na(lab_df.UT2$Month2), ]


          gU2 <- ggplot() +
            geom_vline(xintercept = year.max - duree.eval + 1, col = "#ff8a92", lty = 2) +
            geom_point(data = Data_full_melt.UT2, aes(floor(YearTS), value), alpha = .5) + # ici tous les points avant régularisation - les outliers
            geom_line(data = data.x.UT2, aes(floor(YearTS), y = fill, col = col.if), linewidth = 1.2) + # ici données zea regularisee et completee
            scale_color_manual(values = c("blue" = "blue", "red" = "red")) +
            facet_wrap2(Month2 ~ ., scales = "free", ncol = 3, nrow = 3, axes = "margins", trim_blank = FALSE) +
            xlab("Années") +
            scale_x_continuous(breaks = br, limits = c(min(Data_full_melt.UT$Year), (year.max + 1))) +
            # ylab(bquote("Log"[10]~"1 +"~.(param)*" (ng.L"^"-1"*")")) +
            # ylab(bquote(.(u) ~ .(unit))) +
            ylab("") +
            ylim(c(
              min(Data_full_melt$value, na.rm = TRUE) - .25 * min(Data_full_melt$value, na.rm = TRUE),
              max(Data_full_melt$value, na.rm = TRUE) + .25 * max(Data_full_melt$value, na.rm = TRUE)
            )) +
            geom_line(data = Sen_df.UT2, aes(x = x, y = y), lwd = 1.2, col = "#7ABA78", lty = 2) +
            # geom_label(
            #   data = lab_df.UT2, aes(x = (year.max + year.min) / 2, y = max(Data_full_melt$value, na.rm = TRUE), label = lab),
            #   size = 2, fill = "white", alpha = .5
            # ) +
            theme_bw() +
            theme(legend.position = "none")
          gU2


        }


        if (length(plot.list) >= 1) {
          G3 <- ggarrange(
            ggarrange(
              G1,
              G2,
              ncol = 1,
              heights = c(1, 3)
            ),
            ggarrange(
              gmap,
              gU2,
              ncol = 1,
              heights = c(1, 1.4)
            ),
            widths = c(1.4, 1, .05), # 3e colonne qui fait office de marge
            ncol = 3
          )
        } else {
           G3 <- ggarrange(
            ggarrange(
              G1,
              G2,
              ncol = 1,
              heights = c(1, 3)
            ),
            ggarrange(
              gmap,
              ggplot() +
                theme_minimal(),
              ncol = 1,
              heights = c(1, 1.4)
            ),
            widths = c(1.4, 1, .05), # 3e colonne qui fait office de marge
            ncol = 3
          )
        }
      } else {
           G3 <- ggarrange(
            ggarrange(
              G1,
              G2,
              ncol = 1,
              heights = c(1, 3)
            ),
            ggarrange(
              gmap,
              ggplot() +
                theme_minimal(),
              ncol = 1,
              heights = c(1, 1.4)
            ),
            widths = c(1.4, 1, .05), # 3e colonne qui fait office de marge
            ncol = 3
          )
      }






      # Sauvegarde dans dossier dédié
      if (!dir.exists(file.path(path, s, l))) {
        dir.create(file.path(path, s, l), recursive = TRUE)
      }

      gg_png(G3, paste0(sub(" ", "", x), "_", s, "_mksm.png"), path = file.path(path, s, l), verbose = FALSE, width = 40, height = 25)
    }
  }
}

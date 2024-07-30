## _________________ Infos générales
## Nom : mks.plot.short.R
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
#' @param Data_TS_ok Tableau de données mensuelles
#' @param res.mks Les résultats du mann kendall saisonnier
#' @param col.param Numérique qui donne les colonnes de données
#' @param path Chemin de sauvegarde des images
#' @param melt Logique afin de transformer ou pas les données burtes (pourrait ÃƒÂªtre fait avant la fonction..)
#' @param ds Dataset : spécifier de quelle origine sont les données afin de configurer les unités
#' @return Des plots
#
## ---------------------------



mks.plot.short <- function(Data_full, Data_TS_ok, res.mks, col.param, path, melt = TRUE, ds,
                     Data_full_UT, Data_TS_UT_ok, res.mks.UT) {


    for (s in unique(Data_TS_ok$Site)) {
        tmp <- Data_TS_ok[Data_TS_ok$Site == s, ]

        for (x in unique(tmp$Param)) {
            cat("On traite le paramÃƒÂ¨tre :", x, "au site :", s, "\n\n")


            data.x <- res.mks[[paste(x, s, sep = "_")]]$data.x
            mksm <- res.mks[[paste(x, s, sep = "_")]]$mksm

            data.x$Month <- as.factor(data.x$Month)
            data.x$Month <- revalue(data.x$Month,
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

            Sen_df <- mksm.get.seasonal.estimate(mksm)
            Sen_df <- mksm.fill.Sen.df(data.x, Sen_df, min(data.x$Year), year.max)

            # write.table(x = Sen_df, file = "Pentes_TCHLA_Calvi.csv", row.names = F, sep=";")

            # Sen_moy <-  data.frame(
            #   a= median(mksm$seasonal.estimates[,"slope"]),
            #   b = median(mksm$seasonal.estimates[, "intercept"])
            # )
            #
            # Sen_moy <- rbind(Sen_moy,Sen_moy)
            # Sen_moy$x <- rep(c(year.min, year.max))
            # Sen_moy$y <- Sen_moy$x*Sen_moy$a + Sen_moy$b

            # mksm$p.value
            # mksm$seasonal.estimates
            #

            Sen_df$x[rep(mksm$p.value.s[2:13] >= 0.05, each = 2)] <- NA
            Sen_df$y[rep(mksm$p.value.s[2:13] >= 0.05, each = 2)] <- NA

            # if(mksm$p.value[2] >= 0.05){
            #   Sen_moy$x <- NA
            # }
            # Sen_moy[nrow(Sen_moy)+1,c("x","y")] <- c(0,0)
            #


            pval <- ifelse(mksm$p.value.s[2:13] < 0.001, "< 0.001",
                ifelse(mksm$p.value.s[2:13] < 0.01 & mksm$p.value.s[2:13] >= 0.001, "< 0.01",
                    ifelse(mksm$p.value.s[2:13] < 0.05 & mksm$p.value.s[2:13] >= 0.01, "< 0.05",
                        paste0(" = ", round(mksm$p.value.s[2:13], 2))
                    )
                )
            )


            lab_df <- data.frame(
                lab = paste0(
                    "Pente Sen = ", round(mksm$seasonal.estimates[, "slope"], 4),
                    "\npval ", pval,
                    "\n% Chgmt période = ", Sen_df$tag[seq(2, nrow(Sen_df), 2)],
                    "\n% Chgmt annuel = ", Sen_df$taa[seq(2, nrow(Sen_df), 2)]
                ),
                Month = unique(data.x$Month)
            )

            lab_df <- lab_df[which(mksm$p.value.s[2:13] < 0.05), ]



            sig <- unname(which(mksm$p.value.s[-1] < 0.05))
            # strip.fill <- 1:12
            strip.fill2 <- rep("grey90", 12)
            strip.fill2[sig] <- "grey60"


            strip <- strip_themed(background_x = elem_list_rect(fill = strip.fill2))

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


            # G2 <- ggplot() +
            #   #geom_point(data=dfr, aes(x=decidate, y=log10(1+fillues)), col="#a7e5ce", alpha=.4)+
            #   geom_vline(xintercept = year.max-duree.eval+1, col='#ff8a92', lty=2) +
            #   geom_point(data=data.x[,colnames(data.x) != 'Month'], aes(x=YearTS, y=fill), cex=2, alpha = .5, inherit.aes = FALSE) +
            #   geom_line(data=data.x, aes(x=YearTS, y=fill), alpha=.5) +
            #   geom_point(data=data.x, aes(x=YearTS, y=fill, col=Month), cex=3) +
            #   scale_color_manual(values = Colors) +
            #   xlab("Années") +
            #   #ylab(expression("Log"[10]*" Concentration chlorophylle-a +1 (Ã‚Âµg.L"^"-1"*")")) +
            #   ylab(bquote("Log"[10]~"1 +"~.(param)*" (ng.L"^"-1"*")")) +
            #   xlim(c(year.min, year.max+1)) +
            #   ylim(c(min(data.x$fill)-.25*min(data.x$fill),
            #          max(data.x$fill)+.25*max(data.x$fill))) +
            #   #  geom_text(data=Sen_df[Sen_df$x==2021,], aes(x=2010, y=1.1,
            #   #                                              label=paste0(Sen_df$chgt_mois_ug[2],"Ã‚Âµg.L^- 1{}.mois^- 1 alpha")), parse = TRUE) +
            #   geom_line(data=Sen_df, aes(x=x, y=y, col=Month), lwd=1.2) +
            #   #geom_line(data=Sen_moy, aes(x=x, y=y), col="red", lwd=1.2, lty=2) +
            #
            #
            #
            #   geom_label(data=lab_df,aes(x=2018, y=1.15*max(data.x$fill), label=lab)) +
            #
            #
            #
            #
            #   # labs(col = "Month") +
            #   facet_wrap2(Month~., strip = strip) +
            #   theme_bw()



            # Data_full_melt <- melt(Data_full[,-c(2:4)], id.vars = "YearTS")
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


            G2 <- ggplot() +
                geom_vline(xintercept = year.max - duree.eval + 1, col = "#ff8a92", lty = 2) +
                geom_point(data = Data_full_melt, aes(floor(YearTS), value), alpha = .5) + # ici tous les points avant régularisation - les outliers
                geom_line(data = data.x, aes(floor(YearTS), fill), col = "#FF9A00", linewidth = 1.2) + # ici données zea regularisee et completee
                facet_wrap2(Month ~ ., scales = "free", strip = strip, ncol = 3) +
                xlab("Années") +
                scale_x_continuous(breaks = br, limits = c(min(data.x$Year), (year.max))) +
                # ylab(bquote("Log"[10]~"1 +"~.(param)*" (ng.L"^"-1"*")")) +
                ylab(bquote(.(param) ~ .(unit))) +
                ylim(c(
                    min(Data_full_melt$value, na.rm = TRUE) - .25 * min(Data_full_melt$value, na.rm = TRUE),
                    max(Data_full_melt$value, na.rm = TRUE) + .25 * max(Data_full_melt$value, na.rm = TRUE)
                )) +
                geom_line(data = Sen_df, aes(x = x, y = y), lwd = 1.2, col = "#7ABA78", lty = 2) +
                geom_label(data = lab_df, aes(x = (year.max + min(data.x$Year)) / 2, y = max(Data_full_melt$value, na.rm = TRUE), label = lab), size = 3, fill = "white", alpha = .5) +
                theme_bw()
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


            # Sauvegarde dans dossier dédié
            if (!dir.exists(file.path(path, s))) {
                dir.create(file.path(path, s), recursive = TRUE)
            }

            gg_png(G2, paste0(sub(" ", "", x), "_", s, "_mksm.png"), path = file.path(path, s))
        }
    }
}

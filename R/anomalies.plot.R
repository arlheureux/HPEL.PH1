## _________________ Infos générales
## Nom : anomalies.plot.R
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
#' Fonction qui fait les plots d'anomalies
#' @param Anomalies.R data.frame des anomalies de la période de référence
#' @param Anomalies.E data.frame des anomalies de la période d'évaluation
#' @param quantiles quantiles des anomalies. Résultats de la fonction get.quantile.ano()
#' @param year.min Numérique première année de référence
#' @param year.max Numérique dernière année d'évaluation
#' @param duree.eval Numérique durée de la période d'évaluation
#' @param Freq.ano Fréquence des anomalies. Résultats de la fonction get.anomalies.freq()
#' @param chi2 Résultats du chi2
#' @param path Chemin de sauvegarde des images
#' @return Plots des anomalies
#' @import grid ggpubr ggnewscale
#' @importFrom ggplot2 ggplot aes xlim ylim geom_rect labs scale_fill_manual scale_color_manual theme_bw theme guide_legend geom_col geom_vline scale_x_continuous
#' @export
## ---------------------------

anomalies.plot <- function(Anomalies.R, Anomalies.E, quantiles, year.min, year.max, duree.eval, Freq.ano, chi2, path) {
  
  for (s in unique(Anomalies.R$Site)) {
    for(p in unique(Anomalies.R[Anomalies.R$Site == s,"Param"])) {

      print(s)
      print(p)

      Anomalies.R.s <- Anomalies.R[Anomalies.R$Site == s & Anomalies.R$Param == p, ]
      Anomalies.E.s <- Anomalies.E[Anomalies.E$Site == s & Anomalies.E$Param == p, ]
      quantiles.s <- quantiles[quantiles$Site == s & quantiles$Param == p, ]
      Freq.ano.s <- Freq.ano[Freq.ano$Site == s & Freq.ano$Param == p, ]

      
      p <- gsub("\\(", "\\\\(", p)
      p <- gsub("\\)", "\\\\)", p)
      
      n <- grep(p, names(chi2), value = TRUE)
      n <- grep(s, n, value = TRUE)
      n <- which(names(chi2) %in% n)
      chi2.s <- chi2[n]
      chi2.s <- setNames(chi2.s, gsub("_.*", "", names(chi2.s)))

      Anomalies.R.s$sign <- ifelse(Anomalies.R.s$Ano > 0, "Positive", "Negative")
      Anomalies.E.s$sign <- ifelse(Anomalies.E.s$Ano > 0, "Positive", "Negative")

      lim_sup <- quantiles.s[quantiles.s$Limite == "lim_sup", "Val"]
      LCS <- quantiles.s[quantiles.s$Limite == "LCS", "Val"]
      LSS <- quantiles.s[quantiles.s$Limite == "LSS", "Val"]
      LSI <- quantiles.s[quantiles.s$Limite == "LSI", "Val"]
      LCI <- quantiles.s[quantiles.s$Limite == "LCI", "Val"]
      lim_inf <- quantiles.s[quantiles.s$Limite == "lim_inf", "Val"]

      lim_sup_ext <- quantiles.s[quantiles.s$Limite == "lim_sup_ext", "Val"]
      lim_inf_ext <- quantiles.s[quantiles.s$Limite == "lim_inf_ext", "Val"]



      gC <- ggplot() +
        xlim(year.min, year.max + 1) +
        ylim(min(lim_inf, lim_inf_ext), max(lim_sup, lim_sup_ext)) +
        geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = LCS, ymax = lim_sup, fill = "Extreme", col = "Extreme"
        ), alpha = .7) +
        geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = LSS, ymax = LCS, fill = "Intermédiaire", col = "Intermédiaire"
        ), alpha = .7) +
        geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = LSI, ymax = LSS, fill = "Faible", col = "Faible"
        ), alpha = .7) +
        geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = LCI, ymax = LSI, fill = "Intermédiaire", col = "Intermédiaire"
        ), alpha = .7) +
        geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = lim_inf, ymax = LCI, fill = "Extreme", col = "Extreme"
        ), alpha = .7) +
        labs(x = "Dates", y = "Anomalies") +
        scale_fill_manual("Anomalies",
          values = c(Extreme = "#6693ec", Intermédiaire = "#86cdfa", Faible = "#dfffff"),
          breaks = factor(c("Extreme", "Intermédiaire", "Faible"), levels = c("Extreme", "Intermédiaire", "Faible")),
          guide = guide_legend(override.aes = list(alpha = .7))
        ) +

        # J'ajoute ça pour faire une bordure
        scale_color_manual("Anomalies",
          values = c(Extreme = "#6693ec", Intermédiaire = "#86cdfa", Faible = "#dfffff"),
          breaks = factor(c("Extreme", "Intermédiaire", "Faible"), levels = c("Extreme", "Intermédiaire", "Faible"))
        )


      if (lim_sup_ext > lim_sup) {
        gC <- gC + geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = lim_sup, ymax = lim_sup_ext, col = "Extreme"
        ), fill = NA)
      }

      if (lim_inf_ext < lim_inf) {
        gC <- gC + geom_rect(aes(
          xmin = year.min, xmax = year.max + 1,
          ymin = lim_inf_ext, ymax = lim_inf, col = "Extreme"
        ), fill = NA)
      }


      # gC <- gC + geom_point(data = Anomalies.R, aes(YearTS, Ano)) +
      #   geom_point(data = Anomalies.E, aes(YearTS, Ano), col="purple") +
      #
      #   geom_vline(xintercept = year.max-duree.eval+1, col='red', lty=2) +
      #   theme_bw() +
      #   theme(legend.position = "bottom")
      br <- (year.min:year.max)[(year.min:year.max) %% 2 == 0]

      gC <- gC +
        new_scale_fill() +
        geom_col(data = Anomalies.R.s, aes(YearTS, Ano, fill = sign)) +
        geom_col(data = Anomalies.E.s, aes(YearTS, Ano, fill = sign)) +
        scale_x_continuous(breaks = br) +
        labs(fill = "Signe anomalies") +
        geom_vline(xintercept = year.max - duree.eval + 1, col = "red", lty = 2) +
        scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
        theme_bw() +
        theme(legend.position = "bottom")





      # Travail graphique que les 2 tableaux
      Freq.ano.s <- Freq.ano.s[,1:2]
      rownames(Freq.ano.s) <- c(
        "% Anomalies\nfaibles",
        "% Anomalies\nintermédiaires",
        "% Anomalies\nextrêmes"
      )

      colnames(Freq.ano.s) <- c(
        "Période de référence",
        "Période d'évaluation"
      )
      Freq.ano.s <- round(Freq.ano.s, 2)

      colnames(chi2.s$Exp) <- c(
        "Période de référence",
        "Période d'évaluation"
      )
      rownames(chi2.s$Exp) <- c(
        "% Anomalies\nfaibles",
        "% Anomalies\nintermédiaires",
        "% Anomalies\nextrêmes"
      )


      # Transformation en grob
      Freq.ano.grob <- ggtexttable(Freq.ano.s, theme = ttheme("blank")) %>%
        tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
        tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 3, linetype = 1)

      chi2E.grob <- ggtexttable(chi2.s$Exp, theme = ttheme("blank")) %>%
        tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
        tab_add_hline(at.row = 4, row.side = "bottom", linewidth = 3, linetype = 1)

      pval <- format.pval(chi2.s$p)

      if (chi2.s$p >= 0.05) {
        text <- paste0(
          "Chi2 = ", round(chi2.s$x, 3), "\npvalue", pval,
          "\n--> il n'y a pas de différence entre la proportion d'anomalies de la période de référence et de la période d'évaluation"
        )
      } else {
        text <- paste0(
          "Chi2 = ", round(chi2.s$x, 3), "\npvalue", pval,
          "\n--> il y a une différence entre la proportion d'anomalies de la période de référence et de la période d'évaluation"
        )
      }

      text.g <- ggparagraph(text = text, face = "italic")


      #
      # png(filename = file.path(OUT,"Anomalies.png"), width = 30, height = 20, units = "cm", res = 300)
      # ggarrange(
      #   # First row with line plot
      #   gC,
      #
      #   # Second row
      #   ggarrange(Freq.ano.grob, chi2E.grob, text.g, ncol = 3, label.x = 0, hjust = -0.04, widths = c(.4,.4,.2),
      #             labels = c("b) Fréquences anomalies obtenues", "c) Fréquences anomalies attendues sous H0")),
      #   nrow = 2,
      #   labels = "a)", # Label of the line plot
      #   heights = c(1,.5)
      #
      # )
      # dev.off()
      #

      gC2 <- ggarrange(
        # First row with line plot
        gC,

        # Second row
        ggarrange(Freq.ano.grob, chi2E.grob, text.g,
          ncol = 3, label.x = 0, hjust = -0.04, widths = c(.4, .4, .2),
          labels = c("b) Fréquences anomalies obtenues", "c) Fréquences anomalies attendues sous H0")
        ),
        nrow = 2,
        labels = "a)", # Label of the line plot
        heights = c(1, .5)
      )

            # Sauvegarde dans dossier dédié
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }


      gg_png(grid.draw(gC2), filename = paste0("Anomalies_", p, "_", s, ".png"), path = file.path(path,s))




    }
  }
}






## _________________ Notes
#
#' Fonction qui fait les plots des fréquences d'anomalies
#' @param Freq.ano.an data.frame des fréquences des anomalies. Résultats de la fonction get.anomalies.freq.an()
#' @param path Chemin de sauvegarde des images
#' @return Plots des fréquences des anomalies
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_continuous theme_bw scale_color_discrete scale_fill_discrete labs geom_line
#' @export
## ---------------------------

anomalies.plot.freq <- function(Freq.ano.an, path) {
  
  Freq.ano.an_base <- Freq.ano.an
  
  for (s in unique(Freq.ano.an_base$Site)) {
    for(p in unique(Freq.ano.an_base[Freq.ano.an_base$Site == s,"Param"])) {
      
      Freq.ano.an <- Freq.ano.an_base[Freq.ano.an_base$Param == p & Freq.ano.an_base$Site == s,]
      
      gg <- data.frame()
      for (i in 3:5) {
        g <- Freq.ano.an[, c(1, 2, i)]
        g$Ano <- colnames(Freq.ano.an)[i]
        colnames(g)[3] <- "Ano.val"
        gg <- rbind(gg, g)
      }
      
      br <- seq(min(gg$Year), max(gg$Year))[seq(min(gg$Year), max(gg$Year)) %% 2 == 0]
      
      gg <- gg[order(gg$Year), ]
      
      gg$Ano <- factor(gg$Ano,
                       levels = c("Ano.faibles", "Ano.inter", "Ano.fortes")
      )
      
      g1 <- ggplot(gg, aes(Year, y = Ano.val, fill = Ano)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_bw() +
        scale_x_continuous(breaks = br) +
        scale_color_discrete(
          labels = c("Anomalies\nfaibles\n", "Anomalies\nintermédiaires\n", "Anomalies\nextrêmes")
        ) +
        scale_fill_discrete(
          labels = c("Anomalies\nfaibles\n", "Anomalies\nintermédiaires\n", "Anomalies\nextrêmes")
        ) +
        labs(
          fill = "Type anomalies", col = "Type anomalies\n",
          x = "Années", y = "Fréquence anomalies"
        )
      
      
      
      gg2 <- data.frame()
      for (i in unique(Freq.ano.an$Year)) {
        g <- Freq.ano.an[Freq.ano.an$Year == i, ]
        g$Indice <- g$Ano.fortes / (g$Ano.faibles + g$Ano.inter)
        gg2 <- rbind(gg2, g)
      }
      
      
      g2 <-
        ggplot(data = gg2, aes(x = Year, y = Indice)) +
        geom_line() +
        theme_bw() +
        labs(
          x = "Années", y = "Anomalies extrêmes / (Anomalies faibles + Anomalies intermédiaires)"
        )
      g2
      
      # Sauvegarde dans dossier dédié
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      
      
      gg_png(g1, filename = paste0("Anomalies_frequence_", p, ".png"), path = file.path(path,s))
      gg_png(g2, filename = paste0("Anomalies_indice_", p, ".png"), path = file.path(path,s))
      
    }
  }
}

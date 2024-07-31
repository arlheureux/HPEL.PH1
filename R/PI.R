## _________________ Infos générales
## Nom : PI.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de cré"ation : 18 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
# La fonction get.PI.hull est inspiré des travaux de 
# Holland et Ndah fait dans le cadre d'OSPAR
#' Fonction qui calcul les enveloppes
#' @param xR Numérique : vecteur de coordonnées du groupe fonctionnel en x lors de la période de référence.
#' @param yR Numérique : vecteur de coordonnées du groupe fonctionnel en y lors de la période de référence.
#' @param p Numérique : la valeur de p é considérer pour évaluer les enveloppes. 
#' @param outer Logique : faut-il considérer l'enveloppe externe. Par défaut TRUE.
#' @param inner Logique : faut-il considérer l'enveloppe interne. Par défaut TRUE. 
#' @return les enveloppes du PI
#' @import grDevices
#
## ---------------------------



# Enveloppes du PI
get.PI.hull <- function(xR, yR, p, outer = TRUE, inner = TRUE) {
  
  # Cas général
  # Calcul des med et sd pour préparer la transformation en coord polaires
  medx <- median(xR)
  rx <- sd(xR)
  medy <- median(yR)
  ry <- sd(yR)
  
  # scale by std
  xSC <- (xR-medx) / rx
  ySC <- (yR-medy) / ry
  # get polar coordinates
  ang <- atan2(xSC,ySC)
  vec <- sqrt(abs(xSC)**2+abs(ySC)**2)
  
  # replace zero lengths by small number to avoid dividing by 0
  vec <- replace(vec, vec==0, 0.01)
  
  
  # Je veux les deux enveloppes 
  if(isTRUE(outer) & isTRUE(inner)) {
    
    which.in.hull <- function(vec, p){
      plex = (1 - p)/2
      lx = length(vec)
      
      lr = (max(vec) - min(vec))/1000
      lp = (runif(lx,0,1) - 0.5)* lr
      r = vec + lp
      
      upperindex = ceiling(lx*(1-plex))
      lowerindex = ceiling(lx*plex)
      sortedx = sort(r)
      upperx = sortedx[upperindex]
      lowerx = sortedx[lowerindex]
      out <- (r <= upperx & r >= lowerx)
      return(out)
    }
    
    
  }
  
  
  # Je ne veux que l'enveloppe extérieur
  if(isFALSE(inner)) {
    
    which.in.hull <- function(vec, p){
      plex = (1 - p)
      lx = length(vec)
      
      lr = (max(vec) - min(vec))/1000
      lp = (runif(lx,0,1) - 0.5)* lr
      r = vec + lp
      
      upperindex = ceiling(lx*(1-plex))
      sortedx = sort(r)
      upperx = sortedx[upperindex]
      out <- (r <= upperx)
      return(out)
    }
    
  }
  
  
 

  # trimm data to p lower and 1-p upper 
  wih <- which.in.hull(vec,p)
  xR <- xR[wih]
  yR <- yR[wih]
  ang <- ang[wih]
  vec <- vec[wih]
  
  
  # inverse of norm for inner envelope
  inv1 <- 1/vec
  invx2 <- inv1*cos(ang) 
  invy2 <- inv1*sin(ang) 
  
  
  # inner and outer envelopes by chull
  idxEnvOut <- chull(xR, yR)
  
  if(isTRUE(inner)){
    idxEnvIn <- chull(invx2, invy2)
  }
  
  
 # geometry::convhulln()
 # geometry::inhulln()
  # future options - expand to n-dimensions -> convhulln() and inhulln()
  ptsEnvOutX <- xR[c(idxEnvOut,idxEnvOut[1])]
  ptsEnvOuty <- yR[c(idxEnvOut,idxEnvOut[1])]
  
  if(isTRUE(inner)){
    ptsEnvInX <- xR[c(idxEnvIn,idxEnvIn[1])]
    ptsEnvIny <- yR[c(idxEnvIn,idxEnvIn[1])]
  }
  
  
  if(isTRUE(inner)){
    envelopePts <- list("EnvOuter"=data.frame("outX" = ptsEnvOutX,"outY"=ptsEnvOuty),
                        "EnvInner"=data.frame("inX" = ptsEnvInX,"inY" = ptsEnvIny))
  }else{
    envelopePts <- list("EnvOuter"=data.frame("outX" = ptsEnvOutX,"outY"=ptsEnvOuty))
  }
  
  return(envelopePts)
}


## _________________ Notes
#
# La fonction PI.calc est inspiré des travaux de
# Holland et Ndah fait dans le cadre d'OSPAR
#' Fonction qui calcul le PI
#' @param xE Numérique : vecteur de coordonnées du groupe fonctionnel en x lors de la période d'évaluation.
#' @param yE Numérique : vecteur de coordonnées du groupe fonctionnel en y lors de la période d'évaluation.
#' @param PI.hull Résultats de la fonction get.PI.hull().
#' @param p Numérique : la valeur de p é considérer pour évaluer les enveloppes.
#' @return Les valeurs du PI
#' @import pracma
#
## ---------------------------



# Calcul du PI
PI.calc <- function(xE, yE, PI.hull, p) {
 
  # inner and outer envelope
  xpOut <- PI.hull$EnvOuter$outX 
  ypOut <- PI.hull$EnvOuter$outY
  
  xpIn <- PI.hull$EnvInner$inX
  ypIn <- PI.hull$EnvInner$inY
  
  # find points that are located inside the outer envelope
  inOuterPoly <- inpolygon(xE, yE, xpOut, ypOut, boundary = FALSE)
  # find points that fall into the inner envelope
  inInnerPoly <- inpolygon(xE, yE, xpIn, ypIn, boundary = FALSE)
  
  ## data should fall in outer polygon but not in innner polygone
  n <- sum(inOuterPoly & !inInnerPoly, na.rm = TRUE)
  N <- length(xE)
  
  # Calcul du PI é proprement parler
  PI <- n/N
  
  # Avec ses statistiques
  Out <- c()
  q <- 1-p
  
  for(k in (N-n):N){
    j <- N-k
    pj <- p^j
    qk <- q**k
    
    out <- (pracma::nchoosek(N,k))*pj*qk
    Out <- c(Out,out)
  }
  
  Actual_p <- sum(Out)
  chi2 <- ((N-n)-q*N)^2/(q*N) + (n-p*N)^2/(p*N)
  
  res <- list(PI = PI, 
              pval = Actual_p,
              chi2 = chi2, 
              N = N, 
              n= n)
  
  return(res)
  
}



## _________________ Notes
#
#' Fonction qui fait les plots du PI
#' @param dataR data.frame : Données de la période de référence
#' @param dataE data.frame : Données de la période d'évaluation
#' @param cols Sur quelle colonne effectué les couleurs. Valeurs entre Year et Month
#' @param ds Origine de dataset valeurs entre Rephy, PhytoCly, Boussole
#' @param PI.hull Résultats de la fonction get.PI.hull().
#' @param PI Résultats de la fonction get.PI.calc().
#' @param path Chemin de sauvegarde des images
#' @param GF1 Groupe fonctionnel 1
#' @param GF2 Groupe fonctionnel 2
#' @return les plots du PI
#' @importFrom ggplot2 scale_color_identity xlab ylab geom_path scale_color_gradientn
#' @importFrom grDevices colorRampPalette
#' @importFrom wesanderson wes_palette
#' @import ggConvexHull gridtext grid gridExtra
#
## ---------------------------

# Plot du PI
PI.plot <- function(dataR, dataE, col, ds = NULL, PI.hull, PI, path, GF1, GF2, duree.eval) {
  
  Rx <- range(dataE$GF1, dataR$GF1)
  Ry <- range(dataE$GF2, dataR$GF2)
  
  Rx <- c(.9*Rx[1],1.1*Rx[2])
  Ry <- c(.9*Ry[1],1.1*Ry[2])
  
  Colors <- colorRampPalette(c("#4d4dff", "#00997a", "#ffaa00", "#e60064", "#4d4dff"))(12 + 1)[1:12]

  if(col == "Month") {
    color2 <- color1 <- color <- Colors
  }

  
  if(col == "Year") {
    color1 <- as.vector(wes_palette("Zissou1", n = diff(range(dataR$Year)) - duree.eval, type = "continuous"))
    color2 <- as.vector(wes_palette("Zissou1", n = duree.eval, type = "continuous"))
  }
  
  
  
  gR <- ggplot() + 
    geom_point(data=dataR, aes(GF1, GF2, col = get(col)), alpha=.8, cex=3) + 
    scale_color_gradientn(colours = color1) +
    xlab("") + ylab("") +
    labs(col=col) +  
    geom_path(data=dataR, aes(GF1, GF2), alpha=.6, col = "grey50", lty = 2) +
    geom_convexhull(data = PI.hull$EnvOuter, aes(outX, outY), fill=NA, col="black") +
    geom_convexhull(data = PI.hull$EnvInner, aes(inX, inY), fill=NA, col="black") +  
    xlim(Rx) + ylim(Ry) + 
    theme_bw() +
    labs(col = "AnnÃƒÆ’Ã‚Â©es") +
    theme(legend.position = "bottom",
    legend.key.width = unit(1, "cm"))
gR  

  gE = ggplot() + 
    geom_point(data=dataE, aes(GF1, GF2, col = get(col)), alpha=.8, cex=3) + 
    scale_color_gradientn(colours = color2) +
    geom_path(data=dataE, aes(GF1, GF2), alpha=.6, col = "grey50", lty = 2) +
    geom_convexhull(data = PI.hull$EnvOuter, aes(outX, outY), fill=NA, col="black") +
    geom_convexhull(data = PI.hull$EnvInner, aes(inX, inY), fill=NA, col="black") +   
    xlim(Rx) + ylim(Ry) + 
    xlab("") + ylab("") +
    # annotate("label", x = 5.5, y = 5.3,  label=paste0("PI = ", round(PI,2), "\nPoints = ", N, "\nBinom p = ", 
    #                                                   round(Actual_p,2), "\nChi-sq = ", round(chi2,2))) +
    theme_bw() + 
    labs(col = "AnnÃƒÆ’Ã‚Â©es") +
    theme(legend.position = "bottom",
    legend.key.width = unit(1, "cm"))
  
  
  # gL = ggplot() + 
  #   geom_point(data=rbind(dataR, dataE), aes(GF1, GF2, col = get(col)), alpha=.8, cex=3) + 
  #   scale_color_gradientn(colours = color) +
  #   geom_convexhull(data = PI.hull$EnvOuter, aes(outX, outY), fill=NA, col="black") +
  #   geom_convexhull(data = PI.hull$EnvInner, aes(inX, inY), fill=NA, col="black") +   
  #   xlim(Rx) + ylim(Ry) + 
  #   xlab("") + ylab("") +
  #   theme_bw() + 
  #   labs(col = col) +
  #   theme(legend.position = "bottom")
  
  
  if(round(PI$pval,2) < 0.001){
    pval <- "< 0.001"
  }
  
  if(round(PI$pval,2) >= 0.001 & round(PI$pval,2) < 0.01){
    pval <- "< 0.01"
  }
  
  if(round(PI$pval,2) >= 0.01 & round(PI$pval,2) < 0.05){
    pval <- "< 0.05"
  }
  
  if(round(PI$pval,2) >= 0.05){
    pval <- round(PI$pval,2)
  }
  
  if(ds == "Rephy") {
    unit <- "(cell.L<sup>-1</sup>)"
    var <- "Abondance"
  }else{
    unit <- "(ng.L<sup>-1</sup>)"
    var <- "Biomasse"
  }
  
  
  
  bottom <- c("Log<sub>10</sub> VAR GF1 UNIT")
  left <- c("Log<sub>10</sub> VAR GF2 UNIT")
  
  bottom <- gsub("GF1", GF1, bottom)
  left <- gsub("GF2", GF2, left)
  bottom <- gsub("UNIT", unit, bottom)
  left <- gsub("UNIT", unit, left)
  bottom <- gsub("VAR", var, bottom)
  left <- gsub("VAR", var, left)
  
  
  
  top <- c(paste0("PI = ", round(PI$PI,2), "<br><br><br><br>"), 
           paste0("Binom p = ", pval, ", Chi-sq = ", round(PI$chi2,2),"<br><br>"),
           paste0("N = ", PI$N, ", n = ", PI$n)
           )
  gp.top = gpar(fontsize = c(16, 13, 13))
  
  # g_legend<-function(a.gplot){
  #   tmp <- ggplot_gtable(ggplot_build(a.gplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)
  # }
  
  # mylegend <- g_legend(gL)

  gPI <- grid.arrange(
    arrangeGrob(gR + labs(tag = "a)"),
                gE + labs(tag = "b)"), 
                nrow=1),
    
    bottom = richtext_grob(bottom),
    left = richtext_grob(left, rot =90), 
    top = richtext_grob(top, gp = gp.top)
    #mylegend,
    #heights=c(20,1)
  )
  
  
  # gPI <- ggarrange(
  #   gR ,gE, 
  #   ncol = 2, 
  #   common.legend = TRUE, legend = "bottom"
    
  # )
  
  # gPI <- annotate_figure(gPI, 
  #                 left = richtext_grob(left, rot = 90, vjust = 1, hjust = 0.25),
  #                 bottom = richtext_grob(bottom, vjust = -3), 
  #                 top = richtext_grob(top) 
  #                 )
  

  gg_png(GG_plot = grid.draw(gPI), filename = paste0("PI_", col, ".png"), path = path)
  cat("\t--> PI.png\n")
  
}




## _________________ Notes
#
#' Fonction qui fait les plots du PI anomalies
#' @param dataR data.frame : Données de la période de référence
#' @param dataE data.frame : Données de la période d'évaluation
#' @param ds Origine de dataset valeurs entre Rephy, PhytoCly, Boussole
#' @param PI.hull Résultats de la fonction get.PI.hull().
#' @param PI Résultats de la fonction get.PI.calc().
#' @param path Chemin de sauvegarde des images
#' @param GF1 Groupe fonctionnel 1
#' @param GF2 Groupe fonctionnel 2
#' @return les plots du PI
#' @importFrom ggplot2 scale_color_identity xlab ylab geom_path
#' @import ggConvexHull gridtext
#
## ---------------------------

# Plot du PI
PI.anomalies.plot <- function(dataR, dataE, ds = NULL, PI.hull, PI, path, GF1, GF2) {
  Rx <- range(dataE$GF1, dataR$GF1)
  Ry <- range(dataE$GF2, dataR$GF2)

  Rx <- c(.9 * Rx[1], 1.1 * Rx[2])
  Ry <- c(.9 * Ry[1], 1.1 * Ry[2])


  gR <- ggplot() +
    geom_point(data = dataR, aes(GF1, GF2), alpha = .8, cex = 3) +
    geom_point(data = dataR[dataR$Anomalies.GF1 == TRUE,], aes(GF1, GF2, color = "#d60808e1"), alpha = .8, cex = 3.5) +
    geom_point(data = dataR[dataR$Anomalies.GF2 == TRUE,], aes(GF1, GF2, color = "#00c3ffc5"), alpha = .8, cex = 3.5) +
    geom_point(data = dataR[dataR$Anomalies.GF1 == TRUE & dataR$Anomalies.GF2 == TRUE,], aes(GF1, GF2, color = "#09EC82EF"), alpha = .8, cex = 3.5) +
    scale_color_identity(guide = "legend", name = "",
      breaks = c("#d60808e1", "#00c3ffc5", "#09EC82EF"),
      labels = c(paste("Anomalies", GF1), paste("Anomalies", GF2), paste("Anomalies", GF1, "et", GF2))) +
    xlab("") +
    ylab("") +
    labs(col = col) +
    geom_path(data = dataR, aes(GF1, GF2), alpha = .6, col = "grey50", lty = 2) +
    geom_convexhull(data = PI.hull$EnvOuter, aes(outX, outY), fill = NA, col = "black") +
    geom_convexhull(data = PI.hull$EnvInner, aes(inX, inY), fill = NA, col = "black") +
    xlim(Rx) +
    ylim(Ry) +
    theme_bw() +
    theme(legend.position = "none")
    gR

  gE <- ggplot() +
    geom_point(
      data = dataE, aes(GF1, GF2), 
      alpha = .8, cex = 3
    ) +
    geom_point(
      data = dataE[dataE$Anomalies.GF1 == TRUE, ], 
      aes(GF1, GF2, color = "#d60808e1"), 
      alpha = .8, cex = 3.5
    ) +
    geom_point(
      data = dataE[dataE$Anomalies.GF2 == TRUE, ], 
      aes(GF1, GF2, color = "#00c3ffc5"), 
      alpha = .8, cex = 3.5
    ) +
    geom_point(
      data = dataE[dataE$Anomalies.GF1 == TRUE & dataE$Anomalies.GF2 == TRUE, ], 
      aes(GF1, GF2, color = "#09EC82EF"), 
      alpha = .8, cex = 3.5
    ) +
    scale_color_identity(
      guide = "legend", name = "",
      breaks = c("#d60808e1", "#00c3ffc5", "#09EC82EF"),
      labels = c(paste("Anomalies", GF1), paste("Anomalies", GF2), paste("Anomalies", GF1, "et", GF2))
    ) +
    geom_path(data = dataE, aes(GF1, GF2), alpha = .6, col = "grey50", lty = 2) +
    geom_convexhull(data = PI.hull$EnvOuter, aes(outX, outY), fill = NA, col = "black") +
    geom_convexhull(data = PI.hull$EnvInner, aes(inX, inY), fill = NA, col = "black") +
    xlim(Rx) +
    ylim(Ry) +
    xlab("") +
    ylab("") +
    # annotate("label", x = 5.5, y = 5.3,  label=paste0("PI = ", round(PI,2), "\nPoints = ", N, "\nBinom p = ",
    #                                                   round(Actual_p,2), "\nChi-sq = ", round(chi2,2))) +
    theme_bw() +
    theme(legend.position = "none")


  if (round(PI$pval, 2) < 0.001) {
    pval <- "< 0.001"
  }

  if (round(PI$pval, 2) >= 0.001 & round(PI$pval, 2) < 0.01) {
    pval <- "< 0.01"
  }

  if (round(PI$pval, 2) >= 0.01 & round(PI$pval, 2) < 0.05) {
    pval <- "< 0.05"
  }

  if (round(PI$pval, 2) >= 0.05) {
    pval <- round(PI$pval, 2)
  }

  if (ds == "Rephy") {
    unit <- "(cell.L<sup>-1</sup>)"
    var <- "Abondance"
  } else {
    unit <- "(ng.L<sup>-1</sup>)"
    var <- "Biomasse"
  }



  bottom <- c("Log<sub>10</sub> VAR GF1 UNIT")
  left <- c("Log<sub>10</sub> VAR GF2 UNIT")

  bottom <- gsub("GF1", GF1, bottom)
  left <- gsub("GF2", GF2, left)
  bottom <- gsub("UNIT", unit, bottom)
  left <- gsub("UNIT", unit, left)
  bottom <- gsub("VAR", var, bottom)
  left <- gsub("VAR", var, left)



  top <- c(
    paste0("PI = ", round(PI$PI, 2), "<br><br><br><br>"),
    paste0("Binom p = ", pval, ", Chi-sq = ", round(PI$chi2, 2), "<br><br>"),
    paste0("N = ", PI$N, ", n = ", PI$n)
  )

  gp.top <- gpar(fontsize = c(16, 13, 13))

  gPI <- ggarrange(
    gR + labs(tag = "a)"),
    gE + labs(tag = "b)"),
    ncol = 2,
    common.legend = TRUE, legend = "bottom"
  )

  gPI <- annotate_figure(gPI,
    left = richtext_grob(left, rot = 90, vjust = .5, hjust = 0.5),
    bottom = richtext_grob(bottom, vjust = 0.75),
    top = richtext_grob(top, gp = gp.top),
  )


  gg_png(GG_plot = grid.draw(gPI), filename = paste0("PI_Anomalies.png"), path = path)
  cat("\t--> PI.png\n")
}












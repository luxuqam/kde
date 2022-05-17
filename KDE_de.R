KDE_de <- function(
  ...
) {

  #Ouvrir les packages requis

  require(grid)
  require(magrittr)
  require(KernSmooth)
  require(Luminescence)
  require(RLumShiny)
  require(ggplot2)

  #####################################################################

  #CALCUL DU MODE (KDE) PONDÉRÉ ET NON-PONDÉRÉ DES DOSES ÉQUIVALENTES

  #####################################################################

  #Sélection du fichier de données
  file <- file.choose()
  data_transform <- data.table::fread(file, data.table = FALSE)
  nom_de <- basename(file)

  #Calcul de la pondération des De à partir des incertitudes
  data_transform$V3 <- 1/(data_transform$V2^2)
  data_transform$V4 <- data_transform$V1*data_transform$V3
  data_sumV4 <- sum(data_transform$V4)
  data_transform$V5 <- data_transform$V4/data_sumV4
  DoseEq <- data_transform$V1
  DoseEqw <- data_transform$V5


  #Dessin initial à l'échelle
  plot (density(DoseEq, weights = DoseEqw, width = "SJ-dpi", n=512), main = c("KDE des doses équivalentes pondérées", nom_de), col="white", lty = 1, lwd =1)


  #Construction de l'histogramme
  # Optimal bandwidth
  bin_width <- dpih(DoseEq, scalest = "iqr")
  # Number of bins
  nbins <- seq(min(DoseEq) - bin_width,
               max(DoseEq) + bin_width,
               by = bin_width)
  hist(DoseEq,weights=DoseEqw, breaks= nbins, freq = FALSE, add=T)


  #Calcul des densités avec diverses widths (bw)
  density_curve_SJd <- density(DoseEq, weights = DoseEqw, width = "SJ-dpi", n=512)
  density_curve_SJd_nw <- density(DoseEq, width = "SJ-dpi", n=512)


  #Construction des courbes de densit?
  lines (density_curve_SJd, col="red", lty = 1, lwd=2)
  lines (density_curve_SJd_nw, col="blue", lty=1, lwd=2)


  #Calcul de la valeur des pics de KDE
  pic_du_kdeSJd <- density(DoseEq,weights = DoseEqw, bw="SJ-dpi", n=512)$x[which.max(density(DoseEq,weights = DoseEqw,  bw="SJ-dpi", n=512)$y)]%>%round(2)
  pic_du_kdeSJd_nw <- density(DoseEq,bw="SJ-dpi", n=512)$x[which.max(density(DoseEq,  bw="SJ-dpi", n=512)$y)]%>%round(2)

  #Ligne verticale du pic avec bw=SJdpi
  abline(v=c(pic_du_kdeSJd, pic_du_kdeSJd_nw), col=c("red", "blue"), lty=2, lwd=1)
  L=legend("top", legend = rep(NA,2), col=c("red", "blue"), lty=c(2), lwd=c(1), ncol=1, bty='n', x.intersp=5, inset=0.02)
  legend(x = L$rect$left+60, y = L$rect$top, legend=c(pic_du_kdeSJd, pic_du_kdeSJd_nw), bty='n')
  legend(x = L$rect$left+50, y = L$rect$top, legend = c('       Pic pondéré', '       Pic KDE non-pondéré'), col=rep(NA,), lty=c(1,1), ncol=1, x.intersp = 5, bg = NA, bty='n')
}


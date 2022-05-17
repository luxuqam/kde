KDE_g <- function(
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

  #CALCUL DU MODE (KDE) POND?R? ET NON-POND?R? DES VALEURS G

  #####################################################################

  #S?lection du fichier de donn?es
  fileg <- file.choose()
  data_transform <- data.table::fread(fileg, data.table = FALSE)
  nom_g <- basename(fileg)


  #Calcul des pond?rations
  data_transform$V3 <- 1/(data_transform$V2^2)
  data_transform$V4 <- abs(data_transform$V1)*data_transform$V3
  data_sumV4 <- sum(data_transform$V4)
  data_transform$V5 <- data_transform$V4/data_sumV4
  Valeurg <- data_transform$V1
  Valeurg_w <- data_transform$V5


  #Dessin initial ? l'?chelle
  plot (density(Valeurg, weights = Valeurg_w, width = "SJ-dpi", n=512), main = c("KDE des valeurs g pondérées", nom_g), col="white", lty = 1, lwd =1)


  #Construction de l'histogramme
  # Optimal bandwidth
  bin_width <- dpih(Valeurg)
  # Number of bins
  nbins <- seq(min(Valeurg) - bin_width,
               max(Valeurg) + bin_width,
               by = bin_width)
  hist(Valeurg,weights=Valeurg_w, breaks= nbins, freq = FALSE, add=T)


  #Calcul des densit?s avec diverses widths (bw)
  density_curve_g_SJ <- density(Valeurg,weights = Valeurg_w, width = "SJ-dpi", n=512)
  density_curve_g_SJ_uw <- density(Valeurg,width = "SJ-dpi", n=512)


  #Construction des courbes de densit?
  lines (density_curve_g_SJ, col="red", lty = 2, lwd=2)
  lines (density_curve_g_SJ_uw, col="blue", lty = 2, lwd=2)



  #Calcul de la valeur des pics de KDE
  pic_du_kdeSJd_g <- density(Valeurg,weights = Valeurg_w, bw="SJ-dpi", n=512)$x[which.max(density(Valeurg,weights = Valeurg_w,  bw="SJ-dpi", n=512)$y)]%>%round(2)
  pic_du_kdeSJd_g_uw <- density(Valeurg,bw="SJ-dpi", n=512)$x[which.max(density(Valeurg,bw="SJ-dpi", n=512)$y)]%>%round(2)

  #Ligne verticale du pic avec bw=SJdpi
  abline(v=c(pic_du_kdeSJd_g, pic_du_kdeSJd_g_uw), col=c("red", "blue"), lty=2, lwd=1)




  L=legend("top", legend = rep(NA,2), col=c("red", "blue"), lty=c(2), lwd=c(1), ncol=1, bty='n', x.intersp=5, inset=0.02)
  legend(x = L$rect$left+5, y = L$rect$top, legend=c(pic_du_kdeSJd_g, pic_du_kdeSJd_g_uw), bty='n')
  legend(x = L$rect$left+2, y = L$rect$top, legend = c('       valeur g pondérée', '       valeur g non-pondérée'), col=rep(NA,), lty=c(1,1), ncol=1, x.intersp = 5, bg = NA, bty='n')



}

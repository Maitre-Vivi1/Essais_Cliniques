# 1 fixer une vraie valeur VE
# Sim beta(alpha + nv, beta + n - nv)
# Calculer P(VE > 0.3) > 0.95 (T ou F)
# Calculer P(VE > 0.6) > 0.5 (T ou F)
# T et T ==> GO
# F et F ==> NO GO
# T et F ou F et F ==> indéterminé


sim_p <- function(VE_true, n, VE_seuil1, VE_seuil2) {
  
  theta_true <- retour_theta(VE_true)
  
  n_vaccin <- n * theta_true
  
  n_placebo <- n - n_vaccin
  
  p <- rbeta(n = n, shape1 = 0.700102 + n_vaccin, shape2 = 1 + n - n_vaccin)
  
  p1 <- sum(retour_VE(p) > VE_seuil1)/n
  p2 <- sum(retour_VE(p) > VE_seuil2)/n
  
  return(c(p1, p2))
}




dbl_crit <- function(n, VE_seuil1, VE_seuil2, proba1, proba2, VE_true) {
  
  p <- sim_p(VE_true = VE_true, n = n, VE_seuil1 = VE_seuil1, VE_seuil2 = VE_seuil2)
  
  go <- p[1] >= proba1 & p[2] >= proba2
  
  nogo <- p[1] < proba1 & p[2] < proba2
  
  indeter <- go == F & nogo == F
  
  return(c(go, nogo, indeter))
}



calcul_go_nogo_indeter <- function(n, VE_seuil1, VE_seuil2, proba1, proba2, VE_true, nb_replic) {
  
  liste <- list()
  go <- 0 ; nogo <- 0 ; indeter <- 0
  
  for (i in 1:nb_replic) {
    dbl <- dbl_crit(n, VE_seuil1, VE_seuil2, proba1, proba2, VE_true)
    liste <- append(liste, list(dbl))
  }
  
  for (i in 1:nb_replic) {
    go <- go + liste[[i]][1]/nb_replic
    nogo <- nogo + liste[[i]][2]/nb_replic
    indeter <- indeter + liste[[i]][3]/nb_replic
  }
  
  return(c(go, nogo, indeter))
}


test <- as.data.frame(t(sapply(seq(0.501,0.9,0.001), calcul_go_nogo_indeter, n = 70, VE_seuil1 = 0.3, VE_seuil2 = 0.6, proba1 = 0.99, proba2 = 0.7, nb_replic = 1000)))
names(test) <- c("go", "nogo", "indeter")

sequ <- seq(0.501,0.9,0.001)

plot(x = sequ, y = test$go, "l", ylab = "Probability", xlab = "Vraie VE", main = " n(vaccin) = n(placebo) = 35")
lines(x = sequ, y = test$nogo, "l" , lty=2, lwd=1)
lines(x = sequ, y = test$indeter, "l", lty=2, lwd=3)
abline(v=0.7, col = "red")
abline(h = 0.95, col = "blue")

sequ[which(test$go > 0.95)][1]

test$go <- ksmooth(sequ, test$go, kernel = "normal", bandwidth = 0.01)$y
test$nogo <- ksmooth(sequ, test$nogo, kernel = "normal", bandwidth = 0.01)$y
test$indeter <- ksmooth(sequ, test$indeter, kernel = "normal", bandwidth = 0.01)$y


ggplot(data = test, aes(x = sequ)) +
  geom_line(aes(y = go), col = "steelblue4") +
  geom_line(aes(y = nogo), col = "brown4") +
  geom_line(aes(y = indeter)) +
  geom_hline(yintercept = 0.95, col = "red") +
  geom_vline(xintercept = 0.7, col = "red")

35/0.017/0.8*2

# Protocol :  
# VE = 70% ; n = 53
# 90% proba a  posteriori d'avoir VE > 0.2
# 1.7% incidence dans le placebo et 20% de pdv

# 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 = token de push github


calcul_p <- function(VE_estime, n) {
  
  theta <- (VE_estime - 1)/(VE_estime - 2)
  
  n_vaccin <- n * theta
  
  n_placebo <- n - n_vaccin
  
  simu <- rbeta(n, shape1 = 0.700102 + n_vaccin, shape2 = 1 + n - n_vaccin)
  
  
  p <- pbeta(q = 7/17, shape1 = 0.700102 + n_vaccin, 1 + n - n_vaccin, lower.tail = T) # Pour VE > 0.2
  
  return(p)
}



sec_n <- seq(5,100,1)
vec_sim <- sapply(seq(5,100,1), calcul_p, VE_estime = 0.7) # True valeur


plot(y = sec_n, x = vec_sim, "l", xlab = "proba a posteriori", ylab = "Nbre de sujet")
abline(v=0.95, col = "red")
abline(h = sec_n[which(vec_sim > 0.95)][1]) # 55





theta = 7/17
n_vaccin <- 19 * theta
n_placebo <- 19 - n_vaccin
round(round(n_vaccin + n_placebo + 1)/0.8/0.017)


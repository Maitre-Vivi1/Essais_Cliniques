# Protocol :  
# VE = 70% ; n = 53
# 90% proba a  posteriori d'avoir VE > 0.2
# 1.7% incidence dans le placebo et 20% de pdv


calcul_p <- function(VE_estime, n) {
  
  theta <- (VE_estime - 1)/(VE_estime - 2)
  
  n_vaccin <- n * theta / (1 - theta)
  
  n_placebo <- n - n_vaccin
  
  simu <- rbeta(n, shape1 = 0.700102 + n_vaccin, shape2 = 1 + n - n_vaccin)
  
  
  p <- pbeta(q = 4/9, shape1 = 0.700102 + n_vaccin, 1 + n - n_vaccin, lower.tail = T) # Pour VE > 0.2
  
  return(p)
}



sec_n <- seq(5,100,1)
vec_sim <- sapply(seq(5,100,1), calcul_p, VE_estime = 0.7) # True valeur


plot(y = sec_n, x = vec_sim, "l", xlab = "proba a posteriori", ylab = "Nbre de sujet")
abline(v=0.986, col = "red")
abline(h = 53)



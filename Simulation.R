# Protocol :  
# VE = 70% ; n = 53
# 90% proba a  posteriori d'avoir VE > 0.2
# 1.7% incidence dans le placebo et 20% de pdv

# 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 = token de push github


retour_theta <- function(x){
  (x - 1)/(x - 2)
}

retour_theta(0.3)

calcul_p <- function(VE_true, n, theta_cible) { 
  
  theta_true <- (VE_true - 1)/(VE_true - 2)
  
  n_vaccin <- n * theta_true
  
  n_placebo <- n - n_vaccin
  
  p <- pbeta(q = theta_cible, shape1 = 0.700102 + n_vaccin, 1 + n - n_vaccin, lower.tail = T)
  
  return(p)
}


# 20 ; 30 ; 40


sec_n <- seq(1,100,1)
vec_sim <- sapply(seq(1,100,1), calcul_p, VE_true = 0.7, theta_cible = retour_theta(0.3))


plot(y = sec_n, x = vec_sim, "l", xlab = "proba a posteriori", ylab = "Nbre de sujet")
abline(v=0.95, col = "red")
abline(h = sec_n[which(vec_sim > 0.95)][1])

sec_n[which(vec_sim > 0.99)][1]/0.8/0.017*2
sec_n[which(vec_sim > 0.95)][1]/0.8/0.017*2
sec_n[which(vec_sim > 0.9)][1]/0.8/0.017*2
sec_n[which(vec_sim > 0.7)][1]/0.8/0.017*2
sec_n[which(vec_sim > 0.5)][1]/0.8/0.017*2







n_dbl_crit = 179

sec_ve <- seq(0,1,0.0001)


vec_sim_signif <- sapply(seq(0,1,0.0001), calcul_p, n = n_dbl_crit, theta_cible = retour_theta(0.3))

plot(x = sec_ve, y = vec_sim_signif, "l", xlab = "VE true", ylab = "Probabilité de VE > 0.3")
lines(y = 1-vec_sim_signif, x = sec_ve, "l", col = "red")
abline(v=0.7, col = "blue")
abline(h = 0.95, col = "blue")


vec_sim_relev <- sapply(seq(0,1,0.0001), calcul_p, n = n_dbl_crit, theta_cible = retour_theta(0.6))

plot(x = sec_ve, y = vec_sim_relev, "l", xlab = "VE true", ylab = "Probabilité de VE > 0.6")
lines(y = 1-vec_sim_relev, x = sec_ve, "l", col = "red")
abline(v=0.7, col = "blue")
abline(h = 0.5, col = "blue")


proba_go <- vec_sim_signif*vec_sim_relev
proba_nogo <- (1 - vec_sim_signif) * (1 - vec_sim_relev)
proba_indeter <- 1 - proba_go - proba_nogo
proba_go ; proba_nogo ; proba_indeter

plot(x = sec_ve, y = proba_go, "l", ylab = "Probability", xlab = "Vraie VE", main = " n(vaccin) = n(placebo) = 90")
lines(x = sec_ve, y = proba_nogo, "l" , lty=2, lwd=1)
lines(x = sec_ve, y = proba_indeter, "l", lty=2, lwd=3)
abline(v=0.7, col = "red")
abline(h = 0.95, col = "blue")

proba_go[which(sec_ve == 0.7)] # n = 179

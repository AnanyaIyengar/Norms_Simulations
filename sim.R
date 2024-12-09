#ABAD Simulation

#Setting utility function for one household

household_util <- function(m, power, ap, gamma, an, al, a, t) {
  In <- 0
  if (a <= an) {
    In <- 1
  }
  Il <- 0
  if (a>= al) {
    Il <- 1
  }
  mutil <- (m + t*Il)^power
  dist <- abs(a-ap)
  norm_strength <- gamma*In
  return (mutil - dist + norm_strength)
}

maxima_finder <- function(m, power, ap, gamma, an, al, t) {
  util_ap <- household_util(m, power, ap, gamma, an, al, ap, t)
  util_an <- household_util(m, power, ap, gamma, an, al, an, t)
  util_al <- household_util(m, power, ap, gamma, an, al, al, t)
  utils_possible <- c(util_an, util_al, util_ap)
  print(utils_possible)
  toreturn <- c(an, al, ap)[which(utils_possible == max(utils_possible))][1]
  return (toreturn)
}

maxima_finder(100, 0.5, 17.5, 0.5, 16, 18, 21)

#Distribution of ap

ap_dist <- function(n, min, max, m, power, gamma, an, al, t){
  #aps <- runif(n, min = min, max = max)
  aps <- seq(min, max, by = (max-min)/n)
  optima <- c()
  for (ap in aps) {
    optima <- c(optima, maxima_finder(m, power, ap, gamma, an, al, t))
  }
  return(optima)
}

#Graph-Maker

graphmaker <- function(n, min, max, m, power, gamma, an, al, t){
  baseline_data <- ap_dist(n, min, max, m, power, 0, an, al, 0)
  norms_only <- ap_dist(n, min, max, m, power, gamma, an, al, 0)
  norms_transfer <- ap_dist(n, min, max, m, power, gamma, an, al, t)
  plotdata <- data.frame(baseline_data, norms_only, norms_transfer)
  #Append
  library(ggplot2)
  ecdf <- ggplot(data = plotdata) + stat_ecdf(aes(x = baseline_data), color = "black", linetype = 3, size = 1, alpha = 0.5) + 
    stat_ecdf(aes(x = norms_only), color = "red", size = 1, alpha = 0.4, linetype = 1) + 
    stat_ecdf(aes(x = norms_transfer), color = "blue", size = 1, alpha = 0.4, linetype = 1) + 
    xlab("Age of Marriage") + ylab("F") + theme_bw() 
  return(ecdf)
}

graphmaker(1000, 15, 19, 100, 0.5, 0.5, 16, 18, 21)

graphmaker(1000, 15, 19, 100, 0.5, 2.8, 16, 18, 21)



#for a range of gammas: GRAPH MAKER PRO MAX

twogamma <- function(n, min, max, m, power, an, al, t, gamma1, gamma2){
  baseline_data <- ap_dist(n, min, max, m, power, 0, an, al, 0)
  norm1 <- ap_dist(n, min, max, m, power, gamma1, an, al, 0)
  norm2 <- ap_dist(n, min, max, m, power, gamma2, an, al, 0)
  g1 <- ap_dist(n, min, max, m, power, gamma1, an, al, t)
  g2 <- ap_dist(n, min, max, m, power, gamma2, an, al, t)
  plotdata <- data.frame(baseline_data, norm1, norm2, gamma1, gamma2)
  library(ggplot2)
  ecdf <- ggplot(data = plotdata) + stat_ecdf(aes(x = baseline_data), color = "black", linetype = 3, size = 1, alpha = 0.5) + 
    stat_ecdf(aes(x = norm1), color = "hotpink", size = 1, alpha = 0.6, linetype = 1) + 
    stat_ecdf(aes(x = g1), color = "steelblue1", size = 1, alpha = 0.6, linetype = 1) + 
    stat_ecdf(aes(x = norm2), color = "red", size = 1, alpha = 0.6, linetype = 1) +
    stat_ecdf(aes(x = g2), color = "midnightblue", size = 1, alpha = 0.6, linetype = 1) + 
    xlab("Age of Marriage") + ylab("F") + theme_bw() 
  return(ecdf)
  }


twogamma(1000, 15, 19, 100, 0.5, 16, 18, 21, 0.5, 2.8)

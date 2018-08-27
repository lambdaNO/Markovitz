#############################################################
setwd("~/Desktop/DIVERS_TEMPLATES/MARKOVITZ/Code")
#############################################################
library(quantmod)
library(data.table)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(scales)
#############################################################
lag <- function(x) c(NA, x[1:(length(x) - 1)])
## Script d'extraction des données d'actif depuis le site de Yahoo finance.
## Retour sous forme d'un tableau ET d'un fichier CSV
getData <- function(tickers, long = T) {
  # iterate through the tickers and get the last adjusted price in a data.table
  res <- lapply(tickers, function(x) {
    dat <- getSymbols(x, from = "2000-01-01", auto.assign = F)
    dt <- data.table(date = as.Date(index(dat)),
                     ticker = x,
                     price = as.numeric(Ad(dat)))
    return(dt)
  })
  # combine the list to one data.table
  res <- rbindlist(res)
  # cast the data if the user wants to get the data in a wide format
  if (!long) {
    res <- dcast(res, date ~ ticker)
  }
  # Formatage des données du tableau et ajout de colonnes supplémentaires
  res[, week_id := paste(month(date), year(date), sep = "-")]
  res[, ':=' (mt_price = mean(price),mt_date = min(date)), by = c("ticker", "week_id")]
  res <- res[, .(date = mt_date, ticker, price = mt_price)]
  res <- unique(res)
  res[, date := as.Date(date)]
  ## Création de valeurs indexées
  res[, idx_price := price/price[1], by = ticker]
  ### Création d'une copie du dataframe dans le fichier DATA - Il faudra effectuer les modificaion de format 
  write.csv(res, file = "DATA/fin_data.csv", row.names = F,col.names = T)
  return(res)
}
#############################################################
## Calcul de valeurs aléatoires, ces dernières sont corrélées à celle d'une valeur passée en paramètres.
rmultvar = function(x, r, y_mean, y_sd){ # x : Vecteur de valeurs, r : Corrélation visée, y_mean : moyenne pour les valeurs aléatoire; y_sd : écart type pour les valeurs aléatoires
  x2 <- (x -  mean(x)) / sd(x)
  r2 <- r ^ 2
  ve <- 1 - r2
  SD <- sqrt(ve)
  e <- rnorm(length(x2), mean = 0, sd = SD)
  y <- r*x2 + e
  y <- (y - mean(y)) * y_sd + y_mean
  return(y)
}
#############################################################
## Calculs et affichages d'un PF composé deux actifs pour un vecteur de corrélation donné.
## corVec est le vecteur dans lequel on passe les valeurs de corrélation visées
plot_Cor<- function(nb_Obs,x_mean, x_sd,y_mean,y_sd,corVec){
  set.seed(12345)
  df <- data.table(x = rnorm(nb_Obs, mean = x_mean, sd = x_sd))
  df[, y1 := rmultvar(x, r = corVec[1], y_mean, y_sd)]
  df[, y2 := rmultvar(x, r = corVec[2], y_mean, y_sd)]
  df[, y3 := rmultvar(x, r = corVec[3], y_mean, y_sd)]
  df[, y4 := rmultvar(x, r = corVec[4], y_mean, y_sd)]
  df[, y5 := rmultvar(x, r = corVec[5], y_mean, y_sd)]
  df[, y6 := rmultvar(x, r = corVec[6], y_mean, y_sd)]
  dfl <- melt(df);dfl
  dfx <- data.table(date = 1:nrow(df),
                    ticker = dfl$variable,
                    ret = dfl$value);head(dfx)
  p1 <- plotCombinations(dfx, tickers = c("x", "y1"))
  p2 <- plotCombinations(dfx, tickers = c("x", "y2"))
  p3 <- plotCombinations(dfx, tickers = c("x", "y3"))
  p4 <- plotCombinations(dfx, tickers = c("x", "y4"))
  p5 <- plotCombinations(dfx, tickers = c("x", "y5"))
  p6 <- plotCombinations(dfx, tickers = c("x", "y6"))
  p_all <- grid.arrange(p1, p2, p3, p4, p5, p6)
}
#############################################################
## Méthode d'affichage de plusieurs frontières d'efficiences pour un ensemble d'actifs donné par leurs noms
plotCombinations <- function(dat, tickers) {
  dat <- dat[ticker %in% tickers]
  list1 <- calcEFParamsLong(dat)
  tabs <- dat[, .(mean = mean(ret), sd = sd(ret)), by = "ticker"]
  dfUpper <- data.table(x = seq(from = 0, to = max(tabs$sd), length.out = 10000))
  dfLower <- data.table(x = seq(from = 0, to = min(tabs$sd), length.out = 10000))
  dfUpper[, y := calcEFValues(x, list1, upper = T)]
  dfLower[, y := calcEFValues(x, list1, upper = F)]
  # trim values below the lower point
  y_min <- dat[, mean(ret), by = ticker][, min(V1)]
  dfUpper <- dfUpper[y >= y_min]
  dfLower <- dfLower[y >= y_min]
  correl <- cor(dat[ticker == tickers[1], ret], dat[ticker == tickers[2], ret]) %>% round(2)
  ggplot() +
    geom_line(data = dfUpper, aes(x = x, y = y), linetype = "dashed") +
    geom_line(data = dfLower, aes(x = x, y = y), linetype = "dashed") +
    geom_point(data = tabs, aes(x = sd, y = mean), color = "red", shape = 16) +
    theme_bw() + geom_hline(yintercept = 0, color = "darkgrey") +
    geom_vline(xintercept = 0, color = "darkgrey") +
    ggtitle(paste0("Correlation: ", correl)) +
    xlab("Volatility") + ylab("Expected Returns") +
    scale_y_continuous(label = percent, limits = c(0, max(tabs$mean) * 1.2)) +
    scale_x_continuous(label = percent, limits = c(0, max(tabs$sd) * 1.2))
}
#############################################################
## Fonction de calcul des coefficients \alpha, \beta, \gamma, \delta intervenant dans l'équation de la frontière d'efficience.
## Note : Important, l'ensemble des calculs pour la détermination de ces coefficients s'effectue seulement grâce au vecteur des rendements espérés moyens d'actifs calculés (moyenne sur les valeurs de l'historique des rendements espérés pour chaque actifs)  
calcEFParams <- function(rets) {
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  return(retlist)
}
#############################################################
## Fonction de calcul de la frontière d'efficience.
## Pour un vecteur de x donnés (contenant les risques pour chaque actifs)
## abcd contient les valeurs des coefficients \alpha, \beta, \gamma, \delta de l'équation de la frontière d'efficience obtenue par l'application de la fonction calcEFParams
calcEFValues <- function(x, abcd, upper = T) {
  ## Récupération des valeurs 
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  ## le calcul de la frontière d'efficience utilise un \pm
  if (upper) {
    ## Partie supérieure de la frontière d'efficience (+) -> FE efficace
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    ## Partie inférieure de la frontière d'efficience (-) -> FE inefficace
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  return(retval)
}
##############################################################
## Fonction d'automatisation des calculs pour l'obtention du PF composé de 3 actifs 
## Note : Fonction à revoir pour essayer d'avoir un multi_PF a n-actif s
get_multiPF <- function(nb_Obs,x_mean, x_sd,y_mean,y_sd,z_mean,z_sd){
  set.seed(12345)
  df <- data.table(x = rnorm(nb_Obs, x_mean, x_sd))
  df[, y := rmultvar(x, r = 0, y_mean, y_sd)]
  df[, z := rmultvar(x, r = 0, z_mean, z_sd)]
  setnames(df,c("Actif_x","Actif_y","Actif_z"))
  write.csv(df, file = "DATA/mult_assets.csv", row.names = F)
  return(df) 
}
##############################################################

vecEspRiskAssets <- data.table(mu = c(1,2,3,4,5,6),sigma = c(7,8,9,10,11,12))
# get_n_multiPF <- function(nb_Obs, vecEspRiskAssets){
#   set.seed(12345)
#   nb_Obs = 100
#   S <- dim(vecEspRiskAssets)[1];S
#   ## Initialisation de la première variable 
#   i <- 1
#   var_i = toString(i);var_i
#   lib <- paste("Actif",var_i,sep="_")
#   df <- data.table( x = rnorm(nb_Obs, vecEspRiskAssets[i]$mu, vecEspRiskAssets[i]$sigma));head(df)
#   
#   for (i in 2:S){
#     vecEspRiskAssets[i]$mu
#     df <- df[, rmultvar(df[,1], r = 0, vecEspRiskAssets[i]$mu,vecEspRiskAssets[i]$sigma)];df
#   }
#   df
#   return(df)
# }







cat("Include du fichier des fonctions de calcul des différents portefeuilles\n")
####################################################
affich_compo_PF <- function(symbols,X){
  cat("Le portefeuille est composé de : \n")
  for (i in 1:length(X$W)){
    if(X$W[i]>= 0){
      cat("   ", X$W[i], " % de ", symbols[i], "\n")
    }
  }
  cat("Rendement espéré : ", X$espRend,"\n")
  cat("Risque : ", X$risque)
  
}

####################################################
#### Portefeuille de variance minimale 
getMinVariancePortfolio <- function(mu,covMat,assetSymbols) {
  ### Création d'un vecteur de résultat
  U <- rep(1, length(mu)) # vecteur de 1
  # length(U);head(U,5)
  ### Cacul de l'inverse de la matrice de varCoVar
  O <- solve(covMat)
  # dim(covMat);dim(O)
  ### Calcul du vecteur de poids tq w = \frac{ \Sigma^{-1}u }{\,^t u \Sigma^{-1} u}
  w <- O%*%U /as.numeric(t(U)%*%O%*% U)
  length(w);head(w,5)
  ### La variance du PF mesure le risque au carré, on peut donc calculer le risque tq \sigma_{p} =\sqrt{\,^t w \Sigma w}
  risque <- sqrt(t(w) %*% covMat %*% w)
  # length(risque);risque
  ### La moyenne (esperance) du PF retourne l'espérance de gain  \mu_{p}= \,^t w \mu
  espRend <- t(w) %*% mu
  # espRend
  ### Création de la liste de résultat : W
  W <- `names<-`(round(w, 5), symbols)
  L <- list(W = t(W),espRend = round(as.numeric(espRend), 5),risque = round(as.numeric(risque), 5))
  return(L)
}
####################################################
####################################################

getEfficientPortfolio <- function(mu, covMat, symbols, mu0) {
  ### Création d'un vecteur de résultat
  U <- rep(1, length(mu))
  ### Calcul de l'inverse de la matrice de varCoVar
  O <- solve(covMat)
  ### Calcul des valeurs de A, B et C (CF modélisation numérique) tq A = \,^t u \Sigma^{-1} \mu = \,^t \mu \Sigma^{-1}u; B = \,^t \mu \Sigma^{-1}\mu; C = \,^t u \Sigma^{-1} u
  A <- as.numeric(t(U) %*% O %*% mu)#;A
  B <- as.numeric(t(mu) %*% O %*% mu)#;B
  C <- as.numeric(t(U) %*% O %*% U)#;C
  ### D = B\times C - A^{2} (déterminant de la matrice)
  D <- B * C - A * A
  ### Calcul du vecteur de résultat E correspondant à la part risquée du PF tq E = \frac{1}{D} \Sigma^{-1}(C\mu-Au)
  E <- O %*% ( - A * U + C * mu) / D
  ### Calcul du vecteur de résultat F correspondant à la part sans risque du PF tq F = \frac{1}{D} \Sigma^{-1}(-A\mu + Bu)
  F <- O %*% (B * U - A * mu) / D
  ### Calcul du vecteur de poids tq w = E\times\mu_{0} + F
  w <- E * mu0 + F
  risque <- sqrt(t(w) %*% covMat %*% w) # calcul de l'ecart-type
  espRend <- t(w) %*% mu
  W <- `names<-`(round(w,5), symbols)
  L <- list(W = t(W),espRend = as.numeric(round(espRend,5)),risque = as.numeric(round(risque,5)))
  return(L)
}  


getTangentPortfolio <- function(mu,covMat,symbols,Rf){
  U <- rep(1, length(mu))
  # Calcul de l'excedent de rendement sur les titres tq \mu_{E} = \mu - R_{f}u
  muE <- (mu - Rf * U) 
  ### Cacul de l'inverse de la matrice de varCoVar
  O <- solve(covMat)
  ### Calcul du vecteur de poids tq w = \frac{ \Sigma^{-1}u }{\,^t u \Sigma^{-1} u}
  w <- (O %*% muE)/ as.numeric(t(U)%*%O%*%muE)
  risque <- sqrt(t(w) %*% covMat %*% w)
  espRend <- t(w) %*% mu
  W <- `names<-`(round(w,5), symbols)
  L <- list(W = t(W),espRend = as.numeric(espRend),risque = as.numeric(risque))
  return(L)
}

getCML <- function(mu, covMat, symbols, Rf) {
  mu0 <- seq(Rf,0.08,length.out = 20)#;mu0
  U <- rep(1, length(mu))#;U
  # excedent de rendement
  muE <- (mu - Rf * U)#;muE 
  # excedent sur le rendement du portefeuille
  mu0E <- (mu0 - Rf)#;mu0E
  # inverse de la matrice
  O <- solve(covMat)
  espRend <- NULL
  risque <- NULL
  # On définit le data.frame des poids W
  W <- data.frame()
  w_r <- sapply(mu0E, function(x, muE, O) {
    x * ((O %*% muE) / as.numeric(t(muE) %*% O %*% muE))
  }, O = O, muE = muE)#;w_r
  w_f = 1 - colSums(w_r)#;w_f
  w_r <- `rownames<-`(round(w_r, 5), symbols)#;w_r
  W <- cbind(t(w_r),Rf=w_f)#;W
  espRend <- t(w_r) %*% mu + w_f * Rf#;espRend
  risque <- sqrt(diag(t(w_r) %*% covMat %*% w_r))#;risque
  L <- list(W = data.frame(W), espRend = as.vector(espRend), risque = risque)
  return(L)
}

getEfficientFrontier <- function(mu, covMat, symbols){
  nb_Titre <- length(mu)
  mu0 <- seq(getMinVariancePortfolio(mu, covMat, symbols)$espRend, 0.08, length.out = nb_Titre)#;mu0
  espRend <- NULL
  risque <- NULL
  W <- data.frame()
  for (i in 1:nb_Titre) {
    list <- getEfficientPortfolio(mu, covMat, symbols, mu0[i])
    espRend <- c(espRend, list$espRend)
    risque <- c(risque, list$risque)
    W <- rbind(W, list$W)
  }
  L <- list(W= data.frame(W),espRend = as.vector(espRend), risque = as.vector(risque))
  return(L)
}


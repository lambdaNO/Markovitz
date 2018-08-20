#### Chargement des librairies : 
setwd("~/Desktop/DIVERS_TEMPLATES/Markovitz/Code")
# library(stockPortfolio) - N'est plus disponible sur le CRAN -
# Notons également ue l'API ichart.finance.yahoo.com a visiblement cessé d'exister 
# Une méthode revient à utiliser le package tidyquant
## tidyquant - https://cran.r-project.org/web/packages/tidyquant/index.html
## From https://rviews.rstudio.com/2017/10/11/from-asset-to-portfolio-returns/
library(tidyverse)
library(tidyquant)
library(timetk)
# Graphiques - Gestion des axes (O,x,y) en % (https://cran.r-project.org/web/packages/scales/index.html) - Options supplémentaires graphiques : (https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html)
library(ggplot2)
library(scales) 
library(ggrepel) 
# optionnel pour utilisation de police
library(extrafont);loadfonts() 
#The goal of 'readr' is to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf').
library(readr)
source(file="Markovitz_functions.R")
####################################################
## Récupération des données 
#### Création d'un vecteur de contenant les noms des émetteurs de titres que l'on souhaite utiliser pour composer notre portefeuille
# symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
symbols <- c('YHO','MSFT','GOOGL','AAPL','NKE','SPGI','AMZN','MMM','BAC','BAX','IBM', 'MTB', 'ORCL', 'FDX','AA','DIS','VAR','CAT','USB','JPM')
#### Récupération des données sur l'historique des titres pour chaque émetteur 
prices <- getSymbols(symbols, src = 'yahoo', from = "2012-01-01", auto.assign = TRUE, warnings = FALSE) %>% map(~Ad(get(.))) %>% reduce(merge) %>%`colnames<-`(symbols)
#### Nettoyage des données manquantes (notamment sur YHO)
assetReturns <- na.omit(prices)
## Calcul du rendement espéré (moyenne des historiques des titres pour chaque émetteur)
mu <- colMeans(assetReturns)#;mu
## Calcul de la matrice de varCoVar
covMat <- cov(assetReturns)#;covMat
####################################################
## Paramètres : taux à d'intérêt de l'actif sans risque (à modifier) 
##    annuel 5% 
##    mensuel ~0.4% (5/12)
#### Portefeuille de variance minimum
PF1 <- getMinVariancePortfolio(mu,covMat,symbols)#;W1
affich_compo_PF(symbols,PF1)
#### Portefeuille efficient pour un niveau de rendement donné 
#### Partons sur mu0 = 0.05
mu0 <- 0.05
PF2 <- getEfficientPortfolio(mu,covMat,symbols,mu0)
affich_compo_PF(symbols,PF2)
#### Portefeuille tangent (ou super-efficient)
#### Partons du rendement de l'actif sans risque Rf à 0.005
Rf <- 0.005
PF3 <- getTangentPortfolio(mu,covMat,symbols,Rf)
affich_compo_PF(symbols,PF3)
####################################################
## Calcul de la droite de marché des capitaux
#### Partons du rendement de l'actif sans risque Rf à 0.005
#### \mu_{p} = R_{f} + \sigma_{p} \left[\frac{\mu_{market} - R_{f}}{\sigma_{market}}\right]
Rf <- 0.005
CML <- getCML(mu,covMat,symbols,Rf)#;CML
####################################################
## Frontière d'efficience 
#### On définit le nombre de titre nb_Titre <- length(mu) avec mu, passé en paramètre
EffFront <- getEfficientFrontier(mu,covMat,symbols)#;effFront
############################################################################################################
############################################################################################################
############################################################################################################
#### minimum variance portfolio
MVP <- getMinVariancePortfolio(mu, covMat, symbols)
#### Efficient portfolio with 5% of expected return
m0P <- getEfficientPortfolio(mu, covMat, symbols, mu0 = 0.05)
#### Tangent portfolio
GTP <- getTangentPortfolio(mu, covMat, symbols, Rf = 0.005)
#### Efficient frontier
EFF <- getEfficientFrontier(mu, covMat, symbols)
#### Capital Market line
CML <- getCML(mu, covMat, symbols, Rf = 0.005)
############################################################################################################
############################################################################################################
############################################################################################################
## Représentation graphique : 

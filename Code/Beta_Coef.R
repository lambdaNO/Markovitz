# Calculating Beta in the Capital Asset Pricing Model - https://rviews.rstudio.com/2018/02/08/capm-beta/
## Calcul de la valeur bêta dans le modèle d'évaluation des actifs financiers (MEDAF) - CF notes de cours + CF Modélisation du MEDAF (analyse d'une regression linéaire)
## Objectif : Calculer le coef beta du MEDAF (CAPM)
## Rappel : Le MEDAF estun modèle créé par SHARP et permet d'estimer le rendement d'un actif en fonction du rendement du marché et de la relation linéaire de l'actif avec celui du marché; Cette relation linéaire est le coeficient beta. D'un point de vu empirique, ce modèle est assez mauvais.
## Beta_{i} est calculé comme une regression linéaire des rendements d'un actif i sur ceux du marché.
## Constitution du PF : 
### SPY (S&P500 fund) weighted 25%, EFA (a non-US equities fund) weighted 25%, IJS (a small-cap value fund) weighted 20%, EEM (an emerging-mkts fund) weighted 20%, AGG (a bond fund) weighted 10%
## Définition : Calcul du rendement ordinaire : R = \frac{P_{i}-P_{i-1}}{P_{i}}
## Définition : Calcul du rendement sous forme log return R = \log\left(\frac{P_{i}}{P_{i-1}}\right)
##############################
##############################
setwd("~/Desktop/DIVERS_TEMPLATES/MARKOVITZ/Code")
library(tidyquant) #The 'tidyquant' package provides a convenient wrapper to various 'xts', 'zoo', 'quantmod', 'TTR' and 'PerformanceAnalytics' package functions and returns the objects in the tidy 'tibble' format. 
library(tidyverse) # The tidyverse is an opinionated collection of R packages designed for data science.(ggplot2, dplyr,tidyr,readr,purrr,tibble,stringr,forcats)
library(timetk) # Get the time series index, signature, and summary from time series objects and time-based tibbles. 
############################################################
############################################################
########## Retour sur la constitution d'un PF avec R #######
############################################################
############################################################
## Etape 1 : Import des données sur les valeurs des titres et ajustement des valeurs : (CF introduction a dplyr Wilfried CARIOU/ MEETUP R Nantes)
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
prices <-  getSymbols(symbols, src = 'yahoo', from = "2005-01-01", auto.assign = TRUE, warnings = FALSE) %>% map(~Ad(get(.))) %>% reduce(merge) %>%  `colnames<-`(symbols)
## Etape 2 : Conversion des données quotidienne sous forme de Log Returns (Rendement sous forme d'un quotient de la valeur à l'instant t sur la valeur à l'instant t-1).
## Etape 2.1 : Travaux sur des XTS (format R pour les séries temporelles)
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
## Etape 2.2 : Utilisation des fonctionnalités des packages tidyverse/tidyquant. Conversion des XTS en tibble, une forme spéciale de data_frame permettant un contrôle plus strict et une meilleur formatage des données (par rapport à un dataframe classique) - https://cran.r-project.org/web/packages/tibble/index.html
## Ajout d'une colonne supplémentaire pour le calcul du log return 
asset_returns_long <- prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns))))
### Analyse et commentaire : 
head(asset_returns_xts)
head(asset_returns_long)
## 1) asset_returns_xts est indexé par la date, on les récupère grâce à la fonction : index(asset_returns_xts) 
## asset_returns_long possède une colonne date et on y accède par la méthode d'appel à objet en R asset_returns_long$date
## 2) asset_returns_longs possède une valeur de returns égale à NA pour janvier 2005 et asset_returns_xts ignore totalement la valeur. 
## A note mais peu important pour le sujet.
## Etape 3  Constitution du PF 
## Etape 3.1 : Création du vecteur de poids (pondération des actifs dans le PF)
w <- c(0.25, 0.25, 0.20, 0.20, 0.10)
## Etape 3.2 : Vérification de la cohérence des données (Make sure the weights line up with assets).
asset_weights_sanity_check <- tibble(w, symbols)
asset_weights_sanity_check
## Etape 3.3 : Vérification de la somme des pondération est bien égale à 1 
sum(asset_weights_sanity_check$w)
## Etape 3.4 : Constitution du PF XTS (Basiquement, ça revient à un calcul d'espérance)
## $$Return_{portfolio} = W_{1}\times Return_{asset(1)} + W_{2}\times Return_{asset(2)} + W_{3}\times Return_{asset(3)} + W_{4}\times Return_{asset(4)} + W_{5}\times Return_{asset(5)} = \sum_{i=1}^{nb\_asset}(W_{i}\times Return_{asset(i)})$$
## Etape 3.4.1 : Construction à la main 
w_1 <- w[1];w_2 <- w[2];w_3 <- w[3];w_4 <- w[4];w_5 <- w[5]
## Note : Rappelle indicage (ligne, colonne). L'absence de spéfication de la ligne dans le couple signifique que l'on souhaite appliquer le scalaire à l'ensemble des valeurs contenue dans la colonne.
asset1 <- asset_returns_xts[,1];asset2 <- asset_returns_xts[,2];asset3 <- asset_returns_xts[,3];asset4 <- asset_returns_xts[,4];asset5 <- asset_returns_xts[,5]
portfolio_returns_byhand <- (w_1 * asset1) + (w_2 * asset2) + (w_3 * asset3) +  (w_4 * asset4) + (w_5 * asset5)
names(portfolio_returns_byhand) <- "returns"
head(portfolio_returns_byhand)
## Etape 3.4.2 : Consitution du porteforlio à l'aide de la fonction Return.portfolio avec comme paramètres, un XTS et un vecteur de poids. Il est possible de spécifier la période sur laquelle on souhaite équilibrer nos valeurs (rebalance_on = "months")
portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "months") %>%`colnames<-`("returns")
head(portfolio_returns_xts_rebalanced_monthly)
## Etape 3.4.4 : Equilibrage sur du portefeuille sur l'année (Pour le moment et on peut le vérifier en affichant les head des deux PF, les deux PF sont égaux; La raison est que l'on a codé en dur les coefficients de pondération)
portfolio_returns_xts_rebalanced_yearly <- Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "years") %>%`colnames<-`("returns")
head(portfolio_returns_xts_rebalanced_yearly)
## Etape 3.5 : Conversion du PF asset_returns_long au format tidyquant : fonction tq_portfolio. Elle prend en paramètre un tibble, une colonne d'action que l'on souhaite regroupper ainsi qu'un vecteur de pondération; On peut également affecté un équilibrage par mois ou par an
## Etape 3.5.1 : Equilibrage par mois : 
portfolio_returns_tq_rebalanced_monthly <- asset_returns_long %>% tq_portfolio(assets_col  = asset, returns_col = returns,weights     = w, col_rename  = "returns", rebalance_on = "months")
head(portfolio_returns_tq_rebalanced_monthly)
## Etape 3.5.2 : Equilibrage par année :
portfolio_returns_tq_rebalanced_yearly <- asset_returns_long %>% tq_portfolio(assets_col  = asset, returns_col = returns,weights     = w,rebalance_on = "years")
portfolio_returns_tq_rebalanced_monthly
head(portfolio_returns_tq_rebalanced_yearly)
############################################################
############################################################
########## Reprise du calcul du coef beta pour un PF #######
############################################################
############################################################
## Note :On recommence, on peut donc effacer l'historique avec la fonction 
rm(list=ls())
## Mais penser à recharger les packages
library(tidyquant) #The 'tidyquant' package provides a convenient wrapper to various 'xts', 'zoo', 'quantmod', 'TTR' and 'PerformanceAnalytics' package functions and returns the objects in the tidy 'tibble' format. 
library(tidyverse) # The tidyverse is an opinionated collection of R packages designed for data science.(ggplot2, dplyr,tidyr,readr,purrr,tibble,stringr,forcats)
library(timetk) # Get the time series index, signature, and summary from time series objects and time-based tibbles. 
library(tibbletime) # Built on top of the 'tibble' package, 'tibbletime' is an extension that allows for the creation of time aware tibbles. Some immediate advantages of this include: the ability to perform time-based subsetting on tibbles, quickly summarising and aggregating results by time periods, and creating columns that can be used as 'dplyr' time-based groups.
library(broom) # Convert Statistical Analysis Objects into Tidy Tibbles 
############################################################
## Création du PF 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
prices <- getSymbols(symbols, src = 'yahoo', from = "2013-01-01",to = "2017-12-31", auto.assign = TRUE, warnings = FALSE) %>% map(~Ad(get(.))) %>% reduce(merge) %>% `colnames<-`(symbols)
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
w <- c(0.25, 0.25, 0.20, 0.20, 0.10)
portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "months") %>%`colnames<-`("returns") 
asset_returns_long <-  prices %>% to.monthly(indexAt = "last", OHLC = FALSE) %>% tk_tbl(preserve_index = TRUE, rename_index = "date") %>%gather(asset, returns, -date) %>% group_by(asset) %>%  mutate(returns = (log(returns) - log(lag(returns)))) %>% na.omit()
portfolio_returns_tq_rebalanced_monthly <- asset_returns_long %>%  tq_portfolio(assets_col  = asset,  returns_col = returns, weights     = w, col_rename  = "returns", rebalance_on = "months")
## Etude de rendement de PF et etude de rendement d'actif individuel
#### portfolio_returns_xts_rebalanced_monthly (an xts of monthly returns)
#### portfolio_returns_tq_rebalanced_monthly (a tibble of monthly returns)
#### asset_returns_long (a tidy tibble of monthly returns for those 5 assets above)
############################################################
## MEDAF et Rendement du marché 
### Etape 1 : Choix de l'actif à utiliser comme indicateur de rendement du marché. 
############# Choix de l'actif SPY comme actif indicateur et étude de l'actif SP500
############# Note : On sait déjà que SPY représente 25% de notre PF et la sur la période d'étude (2013-2017), les actifs SPY et SP500 ont été fortement corrélés; Les résultats obtenus seront donc pas très "intéressants". Il est donc recommander de faire les calculs avec un actif à étudier différents 
### Etape 2.1 : Calcul du rendement de marché pour l'actif SPY (Sur 5 années car les données vont du 01/01/2013 au 31/12/2017)
spy_monthly_xts <- getSymbols("SPY",  src = 'yahoo',  from = "2013-01-01", to = "2017-12-31", auto.assign = TRUE,  warnings = FALSE) %>% map(~Ad(get(.))) %>% reduce(merge) %>% `colnames<-`("SPY") %>% to.monthly(indexAt = "last", OHLC = FALSE)
### Etape 2.2 : Calcul du log return pour l'actif SPY
market_returns_xts <- Return.calculate(spy_monthly_xts, method = "log") %>% na.omit()
### Etape 3 : Création d'un dataframe avec les valeurs de rendement du marché récupéré sous forme d'un Df xts avec la fonction tk_tbl(preserve_index = TRUE, rename_index = "date") from the timetk package.
market_returns_tidy <-  market_returns_xts %>%   tk_tbl(preserve_index = TRUE, rename_index = "date") %>% na.omit() %>% select(date, returns = SPY)
head(market_returns_tidy)
### Etape 4 : Vérification : La périodicité de ce DF est en adéquation avec celle de notre portefeuille (ce qui semble être le cas, ouf)
portfolio_returns_tq_rebalanced_monthly %>%   mutate(market_returns = market_returns_tidy$returns) %>%head()
### Note : Si il y avait eu une erreur dans l'alignement des périodicité, la fonction mutate aurait renvoé une erreur.
############################################################
## Calcul du coefficient Beta du marché 
### On rappelle la formule de calcul du coefficient beta : $${\beta}_{portfolio} = cov(R_p, R_m)/\sigma_m $$
### i.e. La covariance entre le rendement d'un actif et celui du marché divisé par le risque au carré (variance) du marché
### On va d'abord le calculer à la main m^ême si il existe des fonctions R le calculant automatiquement.
Nume <- cov(portfolio_returns_xts_rebalanced_monthly, market_returns_tidy$returns);Nume
Deno <- var(market_returns_tidy$returns);Deno
beta_PF <- Nume/Deno;beta_PF
### Cette valeur de Beta est proche de 1 (comme nous nous y attendions), cela signifie que l'actif SPY est une grande partie de notre PF (et dans notre cas, 25%)
### Il est également possible de calculer le coefficient beta total pour un PF à partr du beta de chaque actif présent dans le PF, pondéré par son poids dans le PF 
### Ce calcul de beta est donné par la formule suivante : $${\beta}_{portfolio} ={\sum_{i=1}^n}W _i~{\beta}_i $$
### On rappelle la constitution de notre PF : ("SPY";0.25),("EFA";0.25),("IJS";0.20),("EEM";0.20),("AGG";0.10)
### Pour appliquer la méthode précédemment présentée, nous allons utilser le calcul de Beta par régression linéaire. En effet, nous devons réalisé une regression linéaire entre chaque rendement actif (individuel) présent dans notre PF et le rendement du marché.
### On pourrait faire ça :
###### Pour l'actif 1 : lm(asset_return_1 ~ market_returns_tidy$returns)
###### Pour l'actif 2 : lm(asset_return_2 ~ market_returns_tidy$returns)
###### ...
###### Pour l'actif 5 : lm(asset_return_5 ~ market_returns_tidy$returns)
asset_return_1 <- asset_returns_xts[,1];head(asset_return_1);asset_return_2 <- asset_returns_xts[,2];head(asset_return_2);asset_return_3 <- asset_returns_xts[,3];head(asset_return_3);asset_return_4 <- asset_returns_xts[,4];head(asset_return_4);asset_return_5 <- asset_returns_xts[,5];head(asset_return_5)
### Mais c'est long, très long et surtout si on a 50 actifs ... On peut donc appliquer une fonction map (et pas de reduce) pour réaliser les regressions sur l'ensemble des actifs en un seul passage
### Note : On utilise la fonction nest qui permet, à partir d'un dataframe, de créer des listes de dataframes pour chaque variable imbriquée dans un dataframe
### nest a transformé le Df dans des listes de deux colonnes, une date et une contenant un actif présent dans le df 
beta_assets <- asset_returns_long %>% na.omit() %>% nest(-asset);beta_assets
beta_assets[1,]## Accès à un actif i \in |[1,5]| 
beta_assets[,1]## Accès à la liste des en-têtes de colonne (nom actif)
### Maintenant que l'on sait utiliser la fonction, nest, on peut appliquer une map pour appliquer une fonction de régression sur chaque liste imbriquée
beta_assets <- asset_returns_long %>% na.omit() %>% nest(-asset) %>% mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .)));beta_assets
### On a donc trois colonnes c(l'actif,l'historque de rendement, le résultat du modèle de regression linéaire); le modèle de régression linéaire est obtenu par la fonction suivante : lm(returns ~ market_returns_tidy$returns, data = .)
### Les éléments de la troisième colonnes sont : le beta (coefficient directeur) et l'ordononnée à l'origine (notée Intercept)
beta_assets[1,]$model
### Mais ces résultats ne sont pas au bon format. Nous allons appiquer la fonction tidy() du package broom permettant de transforer un modèle en tidy tibble.
### Nous allons appliquer cette fonction à la colonne contenant les données du modèle (avec mutate et map).
beta_assets <- asset_returns_long %>% na.omit() %>% nest(-asset) %>% mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>%mutate(model = map(model, tidy));beta_assets
### Dès lors, on peut remarquer que la colonne modèle contient encore des dataframe imbriqués - Mais les données sont au bon format
head(beta_assets$model,2)
### On peut finir l'opération avec la fonction unnest() [ liste-colonne, cela fait de chaque élément de la liste sa propre ligne. unnest() peut gérer des listes-colonnes qui peuvent être des vecteurs atomiques, des listes ou des trames de données (mais pas un mélange des différents types).]
beta_assets <- asset_returns_long %>%  na.omit() %>% nest(-asset) %>% mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>%mutate(model = map(model, tidy)) %>% unnest(model)
head(beta_assets)
### Par pur aspect esthétique, on peut supprimer les valeurs d'ordonnées à l'origine (intercept) pour ne conserver que les coefficients directeurs i.e. les coefficients beta
beta_assets <- asset_returns_long %>% na.omit() %>% nest(-asset) %>% mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>% unnest(model %>% map(tidy)) %>% filter(term == "market_returns_tidy$returns") %>% select(-term)
head(beta_assets)
### On peut vérifier que que le coeffcient beta de l'actif SPY est égal à 1 (puisque l'on a constitué notre PF avec 25% d'actif SPY)
beta_assets %>% select(asset, estimate) %>% filter(asset == "SPY")
### Maintenant revenir à la formul sommant les beta de chaque actif du PF pondéré par son poids. 
### On rappelle la valeur de w : 
w
### Et on peut donc effectuer le calcul : 
beta_byhand <-  w[1]*beta_assets$estimate[1] + w[2]*beta_assets$estimate[2] + w[3]*beta_assets$estimate[3] + w[4]*beta_assets$estimate[4] + w[5]*beta_assets$estimate[5]
beta_byhand
### Ce bêta est le même que celui que nous avons calculé ci-dessus en utilisant la méthode covariance/variance, et nous savons maintenant que la covariance des rendements du portefeuille et des rendements du marché divisée par la variance des rendements du marché est égale aux estimations pondérées que nous avons obtenues en régressant le rendement de chaque actif sur les rendements du marché.
############################################################
## Calcul du coefficient beta du MEDAF à partir de séries temporelles
### Il est possible d'effectuer le calcul du coeficient beta du MEDAF à partir de fonction intégrées dans le package PerformanceAnalytics et qui travaille sur des xts CAPM.beta()
### Cette fonction prend deux arguments : le rendement du portefeuille (ou de tout actif) dont nous voulons calculer le bêta, et le rendement du marché. 
library(PerformanceAnalytics)
beta_builtin_xts <- CAPM.beta(portfolio_returns_xts_rebalanced_monthly, market_returns_xts);beta_builtin_xts
### Vérification standard : 
round(beta_byhand,6) == round(beta_builtin_xts,6)
############################################################
## Calcul du coefficient beta du MEDAF à partir des packages dplyr et broom
### Tout d'abord, on utilise le package dplyr pour récupérer le beta de notre PF. On utilise la fonction Do() pour créer le modèle de regression linéaire et récupèrer les coefficients \alpha (ordonnée à l'origine) et \beta (le coefficient directeur)
### Dans un second temps, on utilise la fonction tidy() du package broom pour assurer une meilleure lisibilité (tidy() Turn a model object into a tidy tibble)
head(portfolio_returns_tq_rebalanced_monthly)
beta_dplyr_byhand <-portfolio_returns_tq_rebalanced_monthly %>% do(model = lm(returns ~ market_returns_tidy$returns, data = .)) %>% tidy(model) %>% mutate(term = c("alpha", "beta"));beta_dplyr_byhand
############################################################
## Calcul du coefficient beta du MEDAF à partir des packages tidyquant et PerformanceAnalytics
### On utilise la fonction tq_performance du package tidyquant ce qui ne permettra d'appliquer la fonction CPAM.beta du package PerformanceAnalytics sur le dataframe
### Asset and portfolio performance analysis is a deep field with a wide range of theories and methods for analyzing risk versus reward. The PerformanceAnalytics package consolidates many of the most widely used performance metrics as functions that can be applied to stock or portfolio returns. tq_performance implements these performance analysis functions in a tidy way, enabling scaling analysis using the split, apply, combine framework.
beta_builtin_tq <- portfolio_returns_tq_rebalanced_monthly %>%   mutate(market_return = market_returns_tidy$returns) %>% na.omit() %>% tq_performance(Ra = returns, Rb = market_return,  performance_fun = CAPM.beta) %>% `colnames<-`("beta_tq");beta_builtin_tq
beta_builtin_tq$beta_tq
############################################################
############################################################
## Récapitulons les valeurs des différents beta que l'on a calculé au fil du programme : 
### beta_byhand
### beta_builtin_xts
### beta_dplyr_byhand$estimate[2]
### beta_builtin_tq$beta_tq
resultats <- data.frame(Valeurs = c(beta_byhand,beta_builtin_xts,beta_dplyr_byhand$estimate[2],beta_builtin_tq$beta_tq),row.names = c("Calcul la main","Calcul sur xts","Calcul avec dplyr","Calcul avec tidyquant"));resultats
### Des résultats constants et un bêta proche de 1 comme nous l'attendions, puisque notre portefeuille a une allocation de 25% à l'indice S&P 500. 

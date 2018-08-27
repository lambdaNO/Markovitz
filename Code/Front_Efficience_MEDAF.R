# Introduction à la frontière d'efficience et le MEDAF
setwd("~/Desktop/DIVERS_TEMPLATES/MARKOVITZ/Code")
source('Front_Efficience_MEDAF_Functions.R')
library(ggplot2)
## Source : https://www.r-bloggers.com/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/amp/
## Source : https://github.com/DavZim/Efficient_Frontier/blob/master/
## Objectif : étude du compromis risque-rendement.
## Ex : 2 titres à un même niveau de risque donné mais à niveau de rendement différent. En théorie, on choisirait le placement avec le rendement le plus élevé (un regard plus attentif sur ce que l'on entend par les termes risque et rendement attendu suit plus tard).
## Ex : 2 titres à un même niveau de rendement donné mais à un niveau de risque différent. En théorie, on choisirait l'investissement avec moins de risques. Ce comportement s'appelle l'aversion au risque.
## Problématique : établir ne stratégie e répartition d'un investissement d'une valeur de 100.000 $ entre les 30 sociétés cotées dans l'indice DAX allemand, les 100 sociétés du FTSE britannique, les 500 sociétés du S&P 500, ou si vous avez l'option (la plus réaliste) d'investir dans toutes les actions cotées dans le monde ? Quel serait le choix de répartition des liquidités pour optimiser le rendement du PF.
## Référence : "Portfolio selection" - Markovitz 1952. Considéré comme le modèle fondamental de la finance moderne. Ce modèle est évalué par le MEDAF (CAPM).
# Etude de 3 actifs différents : IMB, GOOG,JOM grâce au package quantmode.
## Extraction des données : 
ticker_sel <- c("IBM", "GOOG", "JPM")
dt <- getData(tickers = ticker_sel);str(dt);head(dt)
######## si l'on souhaite les importer depuis le CSV créé par la fonction getData
### link <- "DATA/fin_data.csv"
### dt <- data.table(read.csv(link))
### dt[, date := as.Date(date)]
### dt[, ticker := as.character(ticker)]
### dt[, idx_price := price/price[1], by = ticker]
### str(dt)
########################################################################################
## Représentation graphique des données récupérées
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() + theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")
## Analyse : Google surclasse les deux autres actifs. Mais on peut quand même voir qu'il est également plus risqué (beaucoup de fluctuations comparé aux deux autres actifs)
## C'est le principe du compromis risque-rendement qui est fondamental pour le MEDAF. Habituellement, le risque est défini comme la volatilité, c'est-à-dire l'écart-type des rendements.
## D'autre part, le rendement est calculé à l'aide de rendements arithmétiques ou logarithmiques (arithmétique dans cet exemple)
## Note : Formule de calcul des rendements
## Rendement arithmétique : R_{t} = \frac{P_{t}-P_{t-1}}{P_{t-1}} = \frac{P_{t}}{P_{t-1}} - 1 
## Rendement logarithmique : R_{t} = \log\left(\frac{P_{t}}{P_{t-1}}\right) = \log(P_{t}) - \log(P_{t-1})
## Une méthode d'analyse du compromis risque/rendement serait de calculer les moyennes et écart-types des rendements et les comparer.
#### Ajout d'une colonne pour le calcul du rendement arithmétique
dt[, ret := price / shift(price, 1) - 1, by = ticker] 
#### Création d'un tableau synthèse - On ne conservera pas les valeurs NA
tab <- dt[!is.na(ret), .(ticker, ret)];head(tab)
#### Ajout d'une colonne pour le calcul du rendement espéré et d'une autre pour la volatiité ;
tab <- tab[, .(er = round(mean(ret), 4), sd = round(sd(ret), 4)),by = "ticker"];head(tab)
## Représentation graphique où (x,y) = (Volatilité, Rendement espéré)
ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = scales::percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = scales::percent, limits = c(0, 0.1))
## Dans ce cas ci, nous devons choisir une seule action et on part de 'hypothèse que les performances indiquent les performances futures (ce qui ne sera pas vrai dans la suite du projet om les performances historiques n'indiqueront presque jamais les performances futures)
## Dans ce cas et sous cette hypothèse, nous choisirions probablement l'actif Google qui semble être le meilleur choix d'investissement.
## Cependant, l'hypothèse du choix d'une unique action est peu plausible. En effet, l'investissement dans plusieurs actifs permet de réduire le risque, c'est ce qu'on appelle le principe de divesification.
## Par exemple, si l'on a le choix d'investif entre plusieurs actifs : 
### - A : esp de rendement 5%, volatilité 4%
### - B : esp de rendement 5%, volatilité 5%
### - C : esp de rendement 6%, volatilité 5%
## On préferera A à B (A est plus efficace que B) car la volatilité de A est inférieure à celle de B
## On préferera C à B (A est plus efficace que B) car l'espérence de rendement de C est supérieure à celui de B
## Cependant, on ne peut pas conclure sur une préférence entre A et C.
## Moralité : Sur un graphique, plus un actif est élevé gauche, plus il a un rendement espéré élvé avec un faible risque de volatilité.
multi_tab <- data.frame(ticker = c("A","B","C") ,er=c(0.05,0.05,0.06), sd = c(0.04,0.05,0.05));head(multi_tab)
ggplot(multi_tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = scales::percent, limits = c(0.025, 0.075)) +
  scale_x_continuous(label = scales::percent, limits = c(0.025, 0.075))
## Dans le cas du PF serait constitué d'une combinaison de deux actifs, ce PF aurait également un compromis rendement/risque espéré. 
## Notre objectif estdonc de calculer pour un ensemble d'actif donné, les valeurs de rendement et de risque afin de trouver le PF optimal.
################################################################################
# Calcul du couple risque/rendemet pour un PF à deux actifs
## Créer un PF composé de deux actifs x et y revient à déterminer la part de titre $\omega_{x}$ et $\omega_{y} = 1 - w_{x}$. On suppose que l'on investit la totalité des fonds, cela implique donc $0 \leq \omega_x \leq 1$, $0 \leq \omega_y \leq 1$ et $\omega_x + \omega_y = 1$
## Exemple : Dans le cas d'un portefeuille contenant 50% de titres $x$ : $\omega_x = 0,5$ (ex Google) et 50% de titres $y$ : $\omega_y = 1 - \omega_x = 0.5$ (ex IBM).
## Les valeurs d'espérance de rendement et de volatilité sont alors fonction de la corrélation $\rho$ entre deux actifs.
## Si les rendements sont parfaitements corrélés (i.e. $\rho = 1$), un PF seraient situés sur une "ligne"; cependant, les rendements ne sont généralement pas parafaitement corrélés
## Pour un PF composé de deux actifs, le graphique montre les valeurs possibles que pourra prendre ce PF pour une corrélation donnée
plot_Cor(nb_Obs = 10000,x_mean = 0.05, x_sd = 0.04, y_mean = 0.03, y_sd = 0.02, corVec = c(0.9999,0.5,0,-0.05,-0.75,-0.9999))
## Plus la corrélation entre deux actions est faibles, plus la diversification peut être intéressante (un rendement espéré plus élevé et une faible volatilité est préférable).
## Dans le cadre d'une corrélation négative parfaite ($\rho = -1$)n la diversfication pourrait être appliquée pour tous les risques. 
## Note : Rappel des notations $\sigma_{a}$ : l'écart type de $a$ et $\sigma_{a,b}$ : la covariance de $a$ et $b$.
## Le calcul du rendement espéré et de la volatilité d'un PF composé de deux actions se fait grâce aux formules suivantes : 
## Le rendement : $\hat{r}_{p} = \omega_{x} \hat{r}_{x}+ \omega_y \hat{r}_{y}$ 
## La volatilité : $\sigma_{p} = \sqrt{\omega_{x}^{2} \sigma_{x}^{2} + \omega_{y}^{2} \sigma_{y}^{2} + 2 \omega_{x} \omega_{y} \sigma_{x,y}}.$
## où : $\omega_{x}$ (resp $\omega_{y}$) est le poids de l'actif $x$ (resp $y$) dans le PF
## Etant donné ces deux séries $x$ et $y$, la corrélation peut se donner par la formule suivante : 
## $\sigma_{x,y} = \rho_{x,y} \sigma_{x} \sigma_{y}$
multi_PF <- get_multiPF(nb_Obs = 10000, x_mean = 0.07,x_sd = 0.05,y_mean = 0.03,y_sd = 0.02, z_mean = 0.04, z_sd = 0.03);head(multi_PF)
## Ou alors : 
## link <- "DATA/mult_assets.csv"
## multi_PF <- data.table(read.csv(link))
### Etape 1 : Calcul des espérances de rendements
er_x <- mean(multi_PF$Actif_x);er_x
er_y <- mean(multi_PF$Actif_y);er_y
### Etape 2 : Calcul de la volatilité - mesure du risque (i.e. l'écart type)
sd_x <- sd(multi_PF$Actif_x);sd_x
sd_y <- sd(multi_PF$Actif_y);sd_y
### Etape 3 : Calcul de la covariance de X et Y
cov_xy <- cov(multi_PF$Actif_x,multi_PF$Actif_y)
### Etape 4 : Création d'un vecteur de poids $\omega$ tq $\omega_{i}\in [0,1]$. (il fait (1-0)/1000 et ajuste les valeurs de $\omega_{i}$ avec $i\in|[1,100]|$) - length.out : non-negative integer. The desired length of the output vector. Other inputs will be coerced to a double vector and the first element taken. Ignored if NA or invalid.
x_weights <- seq(from = 0, to = 1, length.out = 1000)
### Etape 5 : Création d'un PF à 2 actifs sous la contrainte $\omega_{y}[i] = 1 - \omega_{x}[i]$
two_assets <- data.table(wx = x_weights,wy = 1 - x_weights);head(two_assets);tail(two_assets)
### Etape 6 : Calcul de l'espérance de rendement et de la volatilité pour le PF à deux actifs
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,sd_p = sqrt(wx^2 * sd_x^2 + wy^2 * sd_y^2 + 2 * wx * (1 - wx) * cov_xy))];head(two_assets);tail(two_assets)
cat("Nombre de PF possibles : ", dim(three_assets)[1], " combinaisons de deux actifs")
### Etape 7 : Représentation du PF x : "Volatilité", y : "Espérance de rendement".
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)
### Analyse des résultats : 
##### Le point rouge le plus haut correspond au cas extrème $\omega_{x} = 1$ et $\omega_{y} = 0$
##### Le point rouge le plus haut correspond au cas extrème $\omega_{y} = 1$ et $\omega_{x} = 0$
### L'ensemble de toutes les combinaisons possible de deux actifs est représenté par la courbe où les coordonnées des points sont en x : La Volatilité et y : Le rendement espéré
################################################################################
# Calcul du couple risque/rendemet pour un PF à trois actifs actifs
## Avec les mêmes notations de variables que dans le cas d'un PF à deux titres, les formules de calcul sont données par les relations suivantes : 
## Le rendement : $\hat{r}_{p} = \omega_{x} \hat{r}_{x} + \omega_{y} \hat{r}_{y} + \omega_{z} \hat{r}_{z}$
## La volatilité : $\sigma_{p} = \sqrt{\omega_{x}^{2} \sigma_{x}^{2} + \omega_{y}^{2} \sigma_{y}^{2} + \omega_{z}^{2} \sigma_{z}^{2} + 2 \omega_{x} \omega_{y} \sigma_{x,y} + 2 \omega_{x} \omega_{z} \sigma_{x,z} + 2 \omega_{y} \omega_{z} \sigma_{y,z}}$. 
### Etape 1 : Import des données 3
link <- "DATA/mult_assets.csv"
multi_PF3 <- data.table(read.csv(link))
### Etape 2 : Calcul des espérances de rendements
er_x <- mean(multi_PF3$Actif_x);er_x
er_y <- mean(multi_PF3$Actif_y);er_y
er_z <- mean(multi_PF3$Actif_z);er_z
### Etape 3 : Calcul de la volatilité - mesure du risque (i.e. l'écart type)
sd_x <- sd(multi_PF$Actif_x);sd_x
sd_y <- sd(multi_PF$Actif_y);sd_y
sd_z <- sd(multi_PF$Actif_z);sd_z
### Etape 4 : Calcul de la covariance de X et Y, Y et Z, X et Z
cov_xy <- cov(multi_PF3$Actif_x,multi_PF3$Actif_y);cov_xy 
cov_xz <- cov(multi_PF3$Actif_x,multi_PF3$Actif_z);cov_xz
cov_yz <- cov(multi_PF3$Actif_y,multi_PF3$Actif_z);cov_yz
### Etape 5 : Création d'un vecteur de poids $\omega$ 
x_weights <- seq(from = 0, to = 1, length.out = 1000);x_weights
### Etape 5 : Création d'un PF à 3 actifs
###### Etape 5.1 : Création de deux actif x et y - each :  non-negative integer. Each element of x is repeated each times. Other inputs will be coerced to an integer or double vector and the first element taken. Treated as 1 if NA or invalid.
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)), wy = rep(x_weights, length(x_weights)))
###### Etape 5.2 : Ajout du troisème actif z sous la contrainte $\omega_{z}[i] = 1 - (\omega_{x}[i] + \omega_{y}[i])$
three_assets[, wz := 1 - wx - wy];head(three_assets);tail(three_assets)
### Etape 6 : Calcul de l'espérance de rendement et de la volatilité pour le PF à trois actifs
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z, sd_p = sqrt(wx^2 * sd_x^2 + wy^2 * sd_y^2 + wz^2 * sd_z^2 + 2 * wx * wy * cov_xy + 2 * wx * wz * cov_xz + 2 * wy * wz * cov_yz))];head(three_assets);tail(three_assets)
cat("Taille avant suppression des poids neg : ", dim(three_assets)[1], " combinaisons pour un PF à trois actif")
### Etape 7 : Suppression des poids négatif - On suppose dans ce modèle que les poids de chaque actifs dans le PF sont non négatifs (positifs ou nuls)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0];head(three_assets);tail(three_assets)
cat("Taille après suppression des poids neg : ", dim(three_assets)[1], " combinaisons pour un PF à trois actif")
### Etape 8 : Représentation du PF x : "Volatilité", y : "Espérance de rendement".
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)
### Analyse des résultats : 
#### Les couleurs tentent de montrer l'importance des poids $\omega_{x}$ et $\omega_{y}$ :
###### Jaune : Le PF est principalement composé d'actif X
###### Bleu : Le PF est principalement composé d'actif Y
###### Rouge : Le PF est princopalement composé d'actif Z
#### Mise en lumière de la notion de portefeuille efficace : Nous avons de nombreux portefeuilles possibles dont la volatilité est de 2 %, mais un seul d'entre eux est un portefeuille que nous prendrions (celui dont le rendement prévu est le plus élevé), 
#### car il est plus efficace que les autres portefeuilles possibles dont la volatilité est de 2 %, mais dont le rendement prévu est moins élevé. 
#### C'est pourquoi nous appelons le bord (supérieur) de tous les portefeuilles possibles la frontière efficiente.
## Note : Généralisation des formules d'espérance de rendement et de volatilité à des portefeuille de taille n : 
#### Rendement : $\hat{r}_p = \sum_{i=1}^{n} \omega_i \hat{r}_i$
#### Volatilité : $\sigma_{p} = \sum_{i=1}^{n} \omega_{i}^{2} \sigma_{i}^{2} + \sum_{i=1}^{n} \sum_{j=1}^{n} \omega_{i} \omega_{j} \sigma_{i, j} \mbox{? } \forall i \neq j $
################################################################################
# Calcul de la frontière d'efficience (FE)
## On peut effectuer le calcul de la frontière d'effcience sans avoir besoin de simuler la création de PF et déterminer parmis ces derniers ceux qui sont efficaces.
## Dans le cadre du calcul de la FE, on distingue deux cas : 
### Cas d'une FE avec vente à découvert autorisée (i.e. avec pondération négative) 
### Cas d'une FE sans vente à découvert interdite (i.e. avec pondération non négative).
## Note : Vente à découvert = Short Selling 
## Cas 1 : Calcul de la FE avec Short Selling 
### Dans ce cas, la formule de calcul de la FE est donnée par la formule suivante pour un risque $\sigma$ donné et 4 paramètres supplémentaires $\alpha,\beta,\gamma,\delta$ et se base sur des méthodes d'algèbre matriciel : 
### $\hat{r}_{\textrm{ef}}(\sigma) = \frac{\beta}{\alpha} +\sqrt{\left(\frac{\beta}{\alpha}\right)^{2} + \frac{\gamma-\delta\sigma^{2}}{\alpha}}$ qui est la solution à un problme d'optimisation quadratique présentée dans le prises de notes.
#### où $\alpha = \,^t u s^{-1} u$ et $u$, un vecteur unitaire de dimension $n$ ($n$ le nombre de titres dans le PF) et s, la matrice de varCovar des actifs
#### où $\beta = \,^t u s^{-1} \overline{\textrm{ret}}$ avec $\overline{\textrm{ret}}$, le vecteur des rendements moyens pour chaque actif
#### où $\gamma = \,^t \overline{\textrm{ret}} s^{-1} \overline{\textrm{ret}}$
#### où $\delta = \alpha\gamma - \beta^{2}$
### Note : on peut voir que l'on a juste besoin du vecteur des rendements moyens pour effectuer le reste des calculs.
### Note : On suppose que l'on a déja appliqué la fonction get_multiPF pour générer fichier CSV pour un multi_PF
link <- "DATA/mult_assets.csv"
multi_PF2 <- data.table(read.csv(link))
EF_coefs <- calcEFParams(multi_PF2);EF_coefs
## Calcul du couple rendement/risque pour un PF composé de deux titres 
multi_PF2_table <- melt(multi_PF2)[, .(er = mean(value), sd = sd(value)), by = variable];multi_PF2_table
## Représentation graphique de la frontière d'efficience pour le PF composé de deux titres
ggplot(multi_PF2_table, aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = EF_coefs, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = calcEFValues, args = list(abcd = EF_coefs, upper = F), n = 10000,
                color = "blue", size = 1) +
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(multi_PF2_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(multi_PF2_table$sd) * 1.2))
## L'équation de l'EF pour les valeurs des 4 paramèteres calculés et de $\sigma$ est donc : 
## $\hat{r}_{\textrm{ef}}(\sigma) = \frac{147.8}{4037.6} \pm \sqrt{\left(\frac{147.8}{4037.6}\right)^2 - \frac{6.0 - 2339.9 \times \sigma^2}{4037.6}}. $
## Les valeurs pour la partie + de la fonction est la frontière supérieure, efficace, tandis que la partie - représente la frontière inférieure, inefficace.
## La courbe rouge (courbe supérieure) est la frontière efficace réelle, tandis que la courbe inférieure (bleue) représente une frontière inefficace. 
## Cela est dû au fait que nous pouvons créer un mélange des trois actifs qui a la même volatilité mais un rendement attendu plus élevé. Comme nous sommes capables de vendre à découvert (emprunter de l'argent en vendant des actions que nous ne possédons pas et en investissant ces liquidités), la frontière efficiente ne touche pas nécessairement les trois actifs, ni ne s'arrête aux points, mais s'étend vers l'extérieur.
######
## Cas 2 : Calcul de la FE sans Short Selling 
## Restreindre la sélection du portefeuille en n'ayant que des pondérations positives d'actifs limite les montants des portefeuilles possibles et introduit une complexité qui ne peut pas être traitée par les mathématiques sous forme fermée (solution explicite pour un système d'équation)
## Il est donc nécessaire d'étudier ce cas sous forme d'un problème d'optimisation mathématique (Montré dans les modélisation mathématiques)
## Pour ce faire, nous allons utiliser le package tseries qui propose une fonction de calcul de PF optimal
library(tseries)
link <- "DATA/mult_assets.csv"
multi_PF <- data.table(read.csv(link))
multi_PF_table <- melt(multi_PF)[, .(er = mean(value),sd = sd(value)), by = variable];multi_PF_table
### Création d'un vecteur contenant 1000 valeurs reparties entre les valeurs min et max des ecarts types (risques) du PF
er_vals <- seq(from = min(multi_PF_table$er), to = max(multi_PF_table$er), length.out = 1000);head(er_vals);tail(er_vals)
### Recherche du PF optimal pour chaque valeur de rendement attendu possible 
### Note : les valeurs sont explicitement fixées entre le minimum et le maximum des rendements attendus par actif.
sd_vals <- sapply(er_vals, function(er) { 
  op <- portfolio.optim(as.matrix(multi_PF), er) ## Attention, on reprend bien la valeur de la variable multi_PF (et pas la valeur multi_PF_table)
  return(op$ps)
});head(sd_vals);tail(sd_vals)
### Préparation des valeurs pour l'affichage 
plot_dt <- data.table(sd = sd_vals, er = er_vals);head(plot_dt);tail(plot_dt)
### Recherche du risque min - Portefeuile optimal de risque minimal 
minsd <- min(plot_dt$sd);minsd
### Recherche du rendement espéré du PF possédant ce risque min (risque trouvé à la ligne du dessus)
minsd_er <- plot_dt[sd == minsd, er];minsd_er
cat("Risque minimal : ", minsd," %")
cat("Rendement espéré du PF possèdant le risque min ", minsd, " % : ", minsd_er ," %")
### Sélection des PF efficient (i.e. ceux dont le rendement espéré est supérieur à celui du PF possèdant le risque minimal)
plot_dt[, efficient := er >= minsd_er];head(plot_dt);tail(plot_dt)
### Analyse rapide des données
cat("Nombre total de PF", dim(plot_dt)[1]," - Nb de PF efficients ", sum(plot_dt$efficient==TRUE), " Nb de PF non efficients ", dim(plot_dt)[1] - sum(plot_dt$efficient==TRUE) )
### Représentation graphique de l'ensemble des PF calculées (Rouge non-efficient, bleu efficient)
ggplot() +
  geom_point(data = plot_dt[efficient == F], aes(x = sd, y = er), size = 0.5, color = "red") +
  geom_point(data = plot_dt[efficient == T], aes(x = sd, y = er), size = 0.5, color = "blue") +
  geom_point(data = multi_PF_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  theme_bw() + ggtitle("Efficient Frontier without Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(multi_PF_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(multi_PF_table$sd) * 1.2))
##En comparant les deux scénarios, nous constatons qu'en limitant les pondérations (en interdisant les ventes à découvert), nous limitons également le nombre de portefeuilles possibles. Dans certains cas, le fait de limiter les ventes à découvert permet d'obtenir des portefeuilles dont le rendement prévu est moindre ou dont la volatilité prévue est plus élevée.
pdat1 <- plot_dt[, .(sd, er, type = "wo_short", efficient)]
pdat2lower <- data.table(sd = seq(from = 0, to = max(pdat1$sd) * 1.2, length.out = 1000))
pdat2lower[, ':=' (er = calcEFValues(sd, EF_coefs, F), type = "short",efficient = F)]
pdat2upper <- data.table(sd = seq(from = 0, to = max(pdat1$sd) * 1.2, length.out = 1000))
pdat2upper[, ':=' (er = calcEFValues(sd, EF_coefs, T),type = "short",efficient = T)]
pdat <- rbindlist(list(pdat1, pdat2upper, pdat2lower))
# plot the values
ggplot() +
  geom_line(data = pdat, aes(x = sd, y = er, color = type, linetype = efficient), size = 1) +
  geom_point(data = multi_PF_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  theme_bw() + ggtitle("Efficient Frontiers") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(multi_PF_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(multi_PF_table$sd) * 1.2)) +
  scale_color_manual(name = "Short-Sells", values = c("red", "blue"), labels = c("Allowed", "Prohibited")) +
  scale_linetype_manual(name = "Efficient", values = c(2, 1))
##  la frontière efficiente est un outil très utile pour comprendre l'un des fondements de la finance. La question de savoir quel portefeuille de la frontière efficiente est considéré comme le meilleur dans le cadre du modèle

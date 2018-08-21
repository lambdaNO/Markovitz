setwd("~/Desktop/DIVERS_TEMPLATES/MARKOVITZ/Code")
library(Hmisc) # Describe
##### Le fichier suivant contient les excédents de rentabilité pour les firmes Apple (AAPL), Standard and Poor 500 (SP500) et Microsoft (MSFT)
##### CF - Handbook R (lambdaNO/StatDescrUnivFunR/Rapport/Rapport/StatDesRUF.pdf : Régression linéaires)
excRent <- read.table('DATA/table.txt',header=T)
str(excRent)
describe(excRent,num.desc=c("mean","sd","var", "median" ,"min" , "max" , "valid.n"))
attach(excRent) ## Récupération des variables du dataframe pour les manipuler directement (sans excRent$...)
#####################################################
##### Représetation graphique de la sérié temporelle
plot(AAPL,type="l",col="red",ylab="excédent de rendement")
lines(MSFT,type="l",col="blue")
lines(SP500,type="l")
legend("topleft", legend=c("AAPL", "MSFT","SP500"),
       col=c("red", "blue","black"), lty=1:3)
#####################################################
summary.lm(mdl.appl <- lm(AAPL ~ SP500, data = excRent))
anova(mdl.appl)
plot(jitter(AAPL,factor=1),jitter(SP500,factor=4))
abline(lm(AAPL~SP500),lwd=2, col="red")
#####################################################
summary.lm(mdl.msft <- lm(MSFT ~ SP500, data = excRent))
anova(mdl.msft)
plot(jitter(MSFT,factor=1),jitter(SP500,factor=4))
abline(lm(MSFT~SP500),lwd=2, col="red")
detach(excRent)
## Conclusion : Les coefficients des deux titres sont proche de 0 avec des p-value dépassant le niveau de risque de 5%
## AAPL : 0.0916 > 0.05 (Intercept - Pr(>|t|))
## MSFT : 0.64 > 0.05 (Intercept - Pr(>|t|))
## On peut donc accepter l'hypothèse H_{0} concernant la nulité du coefficient \alpha avec un intervalle de confiance à 95%
## Dans le de l'étude du coefficient \beta, on peut voir qu'il est très significatif pour les deux titres au vu de p-value très inférieurs au niveau de risque à 5%
## AAPL : 2.11e-06 < 0.05 (SP500 - Pr(>|t|))
## MSFT : 1.59e-09 < 0.05 (SP500 - Pr(>|t|))
## Valeur de beta :
## AAPL : 0.99477 (Estimate - SP500)
## MSFT : 1.115048 (Estimate - SP500)
## Pourcentage de fiabilité dans l'explication del'évolution du rendement 
## AAPL : 0.2422 (Adjusted R-squared)
## MSFT : 0.3669 (Adjusted R-squared)
## Ouverture : Utiliser un modèle multifactoriel (e.g. APT)



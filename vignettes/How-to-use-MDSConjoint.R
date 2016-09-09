## ------------------------------------------------------------------------

library(MDSConjoint)


## ---- echo=FALSE---------------------------------------------------------
if(!require(support.CEs)){
  install.packages("support.CEs", deps=T, repos="http:cran.us.r-project.org")
}
  library(support.CEs) 

## ------------------------------------------------------------------------
data("MDSConjointData")
names(MDSConjointData)
tire<-MDSConjointData$tire
tire$design

## ------------------------------------------------------------------------
experiment = expand.grid(
  tire$design
)
class(experiment)
head(experiment)
tail(experiment)
length(experiment)

## ------------------------------------------------------------------------
# generate a balanced set of product profiles for survey
tire.survey <- Lma.design(attribute.names = 
                                tire$design, 
nalternatives = 1, nblocks=1, seed=9999)
names(tire.survey)
tire.survey

## ------------------------------------------------------------------------
print(questionnaire(tire.survey))  # print survey design for review
#sink("questions_for_survey.txt")  # send survey to external text file
#questionnaire(tire.survey)
#sink() # send output back to the screen

## ------------------------------------------------------------------------
tires.partWorthsAll<-conjoint.estimation(tire$ratings, tire$bundles, tire$design) 
names(tires.partWorthsAll)
names(tires.partWorthsAll$summary)
names(tires.partWorthsAll$summary$Subj2)


## ------------------------------------------------------------------------
names(tires.partWorthsAll$summary)
knitr::kable(tires.partWorthsAll$summary$Subj2$coefficients, digits=2, caption = 'Coeficientes del modelo estimado para el individuos Subj2' )

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(tires.partWorthsAll$fit), digits=2, caption = 'Estimaciones: Resultados de los 6 primeros individuos' )

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(tires.partWorthsAll$part.worths), digits=2, caption = 'Utilidades parciales: Resultados de los 5 primeros individuos' )

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(t(tires.partWorthsAll$prediction)), digits=2, caption = 'Estimaciones: Resultados de los 6 primeros individuos' )

## ------------------------------------------------------------------------
tires.partWorths1<-conjoint.estimation(tire$ratings[1,], tire$bundles, tire$design)
names(tires.partWorths1)
names(tires.partWorths1$summary$Subj1)

## ------------------------------------------------------------------------
knitr::kable(tires.partWorths1$summary$Subj1$coefficients, digits=2, caption = 'Coeficientes del modelo estimado para el individuos Subj1' )


## ------------------------------------------------------------------------
knitr::kable(head(tires.partWorths1$fit), digits=2, caption = 'Estimaciones: Resultados de *j* individuos' )

## ------------------------------------------------------------------------
knitr::kable(head(tires.partWorths1$part.worths), digits=2, caption = 'Utilidades parciales: Resultados de *j* individuos' )

## ------------------------------------------------------------------------
tires.imp <- importance.of.attributes(tire$ratings, tire$bundles, tire$design)
names(tires.imp)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(tires.imp$part.worths),digits=2, caption = 'Utilidades parciales: Resultados de los 5 primeros individuos' )

## ------------------------------------------------------------------------
knitr::kable(head(tires.imp$imp),digits=2, caption = 'Importancia de los atributos: Resultados de los 5 primeros individuos' )

## ------------------------------------------------------------------------
mean(tires.imp$imp$Brand)
class(tires.imp$imp$Brand)
knitr::kable(apply(tires.imp$imp, 2, mean),digits=2, caption = 'Importancia media de los atributos' )

## ---- fig.cap="Importance of Attributes"---------------------------------
visualize.importance(tires.imp$part.worths,tires.imp$imp, tire$design)

## ------------------------------------------------------------------------
osc<-MDSConjointData$osc
osc$design

## ------------------------------------------------------------------------
osc.partWorthsAll<-conjoint.estimation(osc$ratings, osc$bundles, osc$design) 
class(osc.partWorthsAll)
names(osc.partWorthsAll)
names(osc.partWorthsAll$summary)
names(osc.partWorthsAll$summary$Respondent1)

## ------------------------------------------------------------------------
knitr::kable(osc.partWorthsAll$summary$Respondent1$coefficients, digits=2, caption = 'Estimación del modelo lineal: coeficientes del primer individuo' )

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(t(head(osc.partWorthsAll$fit)), digits=2, caption = 'Estimaciones: Resultados de los 6 primeros individuos' )


## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(t(head(osc.partWorthsAll$part.worths)), digits=2, caption = 'Estimaciones: Resultados de los 6 primeros individuos' )


## ------------------------------------------------------------------------
osc.imp <- importance.of.attributes(osc$ratings, osc$bundles, osc$design)
names(osc.imp)
knitr::kable(head(osc.imp$imp),digits=2, caption = 'Importancia de los atributos: Resultados de los 5 primeros individuos' )

## ------------------------------------------------------------------------
mean(osc.imp$imp$Location)
class(osc.imp$imp$Location)
knitr::kable(apply(osc.imp$imp, 2, mean), digits=2, caption = 'Resumen importancia atributos' )


## ---- fig.cap="Importance of Attributes"---------------------------------
visualize.importance(osc.imp$part.worths, osc.imp$imp, osc$design)

## ------------------------------------------------------------------------
dim(osc$market.profiles)
knitr::kable(osc$market.profiles, digits=2, caption = 'Market profiles')

## ------------------------------------------------------------------------
knitr::kable(osc$bundles, digits=2, caption = 'Bundles of attributes to be rated')

## ------------------------------------------------------------------------
knitr::kable(t(head(osc$ratings)), digits=2, caption = 'Ratings of bundles by individulas')

## ------------------------------------------------------------------------
dim(utilities.of.profiles(osc$market.profiles, osc$ratings, osc$bundles))
knitr::kable(head(utilities.of.profiles(osc$market.profiles, osc$ratings, osc$bundles)), digits=2, caption = 'Ratings of bundles by individulas')

## ------------------------------------------------------------------------
knitr::kable(ms.fe.conjoint(osc$market.profiles, osc$ratings, osc$bundles), digits=2, caption = 'Regla de la maxima utildad')
ms.fe.conjoint(osc$market.profiles, osc$ratings, osc$bundles)

## ------------------------------------------------------------------------
knitr::kable(ms.us.conjoint(osc$market.profiles, osc$ratings, osc$bundles), digits=2, caption = 'Regla de la cuota de preferencia')

## ------------------------------------------------------------------------
knitr::kable(ms.logit.conjoint(osc$market.profiles, osc$ratings, osc$bundles), digits=2, caption = 'Regla logit')

## ------------------------------------------------------------------------
osc.ms.op.1choice<-optim.ms.first.choice(osc$ratings, osc$bundles, osc$market.profiles, osc$design, hpb=1)
knitr::kable(osc.ms.op.1choice, digits=2, caption = 'Regla máxima utilidad')

## ------------------------------------------------------------------------
osc.ms.op.us<-optim.ms.utility.share(osc$ratings, osc$bundles, osc$market.profiles, osc$design, hpb=1)
knitr::kable(osc.ms.op.us, digits=2, caption = 'Regla de la cuota de preferencia')

## ------------------------------------------------------------------------
osc.ms.op.logit<-optim.ms.logit(osc$ratings, osc$bundles, osc$market.profiles, osc$design, hpb=1)
knitr::kable(osc.ms.op.logit, digits=2, caption = 'Regla de la cuota de preferencia')

## ------------------------------------------------------------------------
require(XLConnect)
require(XLConnectJars)

## ------------------------------------------------------------------------
tireData <- loadWorkbook("sas-conjoint.xlsx", create = T) #loading the spreadshit book


## ------------------------------------------------------------------------
# -- Read in conjoint rating data 
tiresBundles <- readWorksheet(tireData, rownames=1, sheet = "bundles", header = TRUE) #load the set of bundles rated by informants
tiresDesign <- readWorksheet(tireData, sheet = "design", header = TRUE) #read conjoint desing
tiresDesign.l <- df2list(tiresDesign)
tiresDesign.l
tiresRatings <- readWorksheet(tireData, rownames=1, sheet = "ratings", header = TRUE)

## ------------------------------------------------------------------------
tirebundles <- read.table("tirebundles.csv", row.names=1, header=T, dec = ".", sep=";")
tiredesign <- read.table("tiredesign.csv", header=T,  sep=";")
tiresDesign.l <- df2list(tiresDesign)
tiresDesign.l
tireratings <- read.table("tireratings.csv", row.names=1, header=T, sep=";")


## ------------------------------------------------------------------------
tiredesign #conjoint design
dim(tirebundles) #check the dimensions of bundles data frame
class(tirebundles) #ckeck the data class
head(tirebundles)
tail(tirebundles)
head(tireratings) #checking data
tail(tireratings)
dim(tireratings)


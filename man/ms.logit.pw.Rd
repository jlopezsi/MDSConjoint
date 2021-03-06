% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms.prediction.pw.R
\name{ms.logit.pw}
\alias{ms.logit.pw}
\title{Computes the market share of the product profiles}
\usage{
ms.logit.pw(mp, pw, design.l)
}
\arguments{
\item{mp}{a matrix with the description of competitors' product profiles}

\item{pw}{a data frame with all clients' part worths}

\item{design.l}{a list with conjoint design}
}
\value{
ms
}
\description{
This function
computes the market share of the product profiles according to the
logit rule. Uses a data frame with part woths to predict utilities
market profiles, mp,  is a matrix of market product profiles (rows)
by attributes (columns); pw  is a data frame of clients (rows) by
attribut levels part worths (colums); design.l is a list with the definition of the conjoint design.
}
\examples{
data(MDSConjointData)
names(MDSConjointData)
osc<- MDSConjointData$osc
names(osc)
osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
names(osc.conjoint)
# [1] "summary"     "fit"         "part.worths"  "prediction"
#head(osc.conjoint$summary)
head(osc.conjoint$fit)
head(osc.conjoint$part.worths)
head(osc.conjoint$prediction)
ms.logit.pw(osc$market.profiles, osc.conjoint$part.worths, osc$design)
class(ms.logit.pw(osc$market.profiles, osc$ratings, osc$bundles))
}


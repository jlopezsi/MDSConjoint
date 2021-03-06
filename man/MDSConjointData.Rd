% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{MDSConjointData}
\alias{MDSConjointData}
\title{Conjoint data sets. It is a list with two lists of data sets needed to test the package}
\format{list}
\source{
\url{http://www.okstate.edu/sas/v7/sashtml/books/stat/chap56/sect45.htm}

\url{http://www.decisionpro.biz/}
}
\usage{
MDSConjointData
}
\description{
MDSConjointData is a list of data needed to illustrate the use of MDSConjoint package.
It conteints a list called MDSConjointData$osc with several data frames and a list.
The list of data called osc is a fictional data set provided by marketing engineering book
(see de reference) about an office department store.

The data file osc is a list contening these files:

design:     a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute.

bundles:    a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis.

ratings:    a data frame with as many rows as individuals have rated the bundles displayed in the columns.

full:       a data frame with a full conjoint design.

market.profiles:  a data frame with as many rows as products are in the market (competitors particularly) by attributes (in columns).

constrains: a data frame with some constrains to be used in the estimation of optimal products.

reveneu:    a data frame with attributes' variation in cost

The data set MDSConjointData$tire is taken from SAS conjoint package (see the reference).
We use it to illustrate the use of conjoint analysis and compare the findings with SAS metric and non
metric conjoing procedure.

The data file tire is a list contening these files:

design:     a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute.

bundles:    a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis.

ratings:    a data frame with as many rows as individuals have rated the bundles displayed in the columns
}
\examples{
data(MDSConjointData)
names(MDSConjointData)
names(MDSConjointData$osc)
names(MDSConjointData$tire)

}
\keyword{datasets}
\keyword{file}


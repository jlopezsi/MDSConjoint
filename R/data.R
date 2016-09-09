#' @description
#' MDSConjointData is a list of data needed to illustrate the use of MDSConjoint package.
#' It conteints a list called MDSConjointData$osc with several data frames and a list.
#' The list of data called osc is a fictional data set provided by marketing engineering book
#' (see de reference) about an office department store.
#'
#' The data file osc is a list contening these files:
#'
#' design:     a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute.
#'
#' bundles:    a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis.
#'
#' ratings:    a data frame with as many rows as individuals have rated the bundles displayed in the columns.
#'
#' full:       a data frame with a full conjoint design.
#'
#' market.profiles:  a data frame with as many rows as products are in the market (competitors particularly) by attributes (in columns).
#'
#' constrains: a data frame with some constrains to be used in the estimation of optimal products.
#'
#' reveneu:    a data frame with attributes' variation in cost
#'
#' The data set MDSConjointData$tire is taken from SAS conjoint package (see the reference).
#' We use it to illustrate the use of conjoint analysis and compare the findings with SAS metric and non
#' metric conjoing procedure.
#'
#' The data file tire is a list contening these files:
#'
#' design:     a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute.
#'
#' bundles:    a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis.
#'
#' ratings:    a data frame with as many rows as individuals have rated the bundles displayed in the columns

#' @title  Conjoint data sets. It is a list with two lists of data sets needed to test the package
#' @name MDSConjointData
#' @aliases MDSConjointData
#' @docType data
#' @examples
#' data(MDSConjointData)
#' names(MDSConjointData)
#' names(MDSConjointData$osc)
#' names(MDSConjointData$tire)
#'
#' @format list
#' @keywords datasets file
#' @format MDSConjointData$osc$full, a data frame with a full conjoint design, with as many rows as profiles, and as many columns as attributes used in the analysis:
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#'
#' @format MDSConjointData$osc$ratings, a data frame with as many rows as individuals have rated the bundles displayed in the columns:
#' \describe{
#'   \item{Bundle1}{numeric}
#'   ...
#'   \item{Bundle16}{numeric}
#' }
#' @format MDSConjointData$osc$design, a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#' @format MDSConjointData$osc$bundles, a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis:
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#' @format MDSConjointData$osc$market.profiles, a data frame with as many rows as products are in the market (competitors particularly) by attributes (in columns):
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#' @format MDSConjointData$osc$constrains, a data frame with some constrains to be used in the estimation of optimal products, in rows we have attributs and in columns their levels:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#' @format MDSConjointData$osc$revenues, a data frame with some variations in cost to be used in the estimation of optimal products, in rows we have attributs and in columns their levels, in cells, variation in cost:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#'
#' @format tire$ratings, a data frame with as many rows as individuals have rated the bundles displayed in the columns:
#' \describe{
#'   \item{Bundle1}{numeric}
#'   ...
#'   \item{Bundle16}{numeric}
#' }
#' @format tire$design, a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#' @format osc$bundles, a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis:
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#'
#'
#' @source \url{http://www.okstate.edu/sas/v7/sashtml/books/stat/chap56/sect45.htm}
#' @source \url{http://www.decisionpro.biz/}
"MDSConjointData"





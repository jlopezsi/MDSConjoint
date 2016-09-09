#' MDSConjoint: An implementation of metric and nonmetric conjoint models for marketing decisions.
#'
#' This package is an implementation of metric and nonmetric conjoint models for
#' marketing analysis and decisions. It estimates the conjoint models por each individual,
#' computes a data frame with all estimations, another data frame with part worts (partial utilities),
#' a data frame with the importance of attributes for all individuals,
#' plots a summary of attributes' importance, computes market shares,
#' and the optim profile given market competitors.
#'
#' @section MDSConjoint functions:
#' The mktgConjoint functions ...
#'
#' @docType package
#' @name MDSConjoint
#' @importFrom graphics axis pie plot
#' @importFrom stats dist lm predict sd
#' @importFrom utils head
#' @importFrom XLConnect loadWorkbook readWorksheet
#' @importFrom XLConnectJars
#' @importFrom support.CEs Lma.design questionnaire
#'


############################################################################ Here I describe the functions needed for analysis, for computing
############################################################################ market shares

utilities.of.profiles <- function(market.profiles, ratings, bundles, rank=0) {
  #  Copyright 2016 Jordi L. Sintas
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  #' @title This function computes the utility of each product profile
  #' @description
  #'
  #' This function computes the utility of each profile (given rivals' profiles and new profiles
  #' ) for each consumer.  The data frame profiles is the
  #' data of existing profiles for which we are going to compute the market shares
  #' is a tale of product profiles in the market (rows) by
  #' attributes (columns), ratings is a data frame of clients (rows) by bundles
  #' rated (colums), bundles is the data frame of bundles (rows) by
  #' attributes (colums) rated by clients.
  #' @param market.profiles                a data frame with the description of competitors' existing product profiles
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @return utilities.profiles     a matrix with the prediction of utilities
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' #this function is called by market share function.
  #' osc.uop <-utilities.of.profiles(osc$market.profiles, osc$ratings, osc$bundles, rank=0)
  #' head(osc.uop)
  #'
################### other variables in the function ##################
  # utilities.profiles      a data frame with the result of product profiles' utility
  # clients                 number of clients that have rated the set of bundles
  # client                  an index for selecting a client
  # y                       a data frame with the ratings of client number i
  # df                      a compposed file of client's ratings plus the bundles description according to bundles matrix
  # n                       number of variables in df data table
  # main.effects.model.fit  an object return by lm() function
  # client.sim              a vector with the prediction of profiles' utility
  # clients.names           a vector with clients' names

  ######################
  options(contrasts = c("contr.treatment", "contr.poly"))  #define the contrasts for the lm model
  utilities.profiles <- data.frame()  #Preparare the data frame
  clients <- nrow(ratings)  #compute the number of respondents
  y <- as.data.frame(t(ratings[1, ]))  #select data for the first respondent
  names(y) <- "y"  #change variable name to y
  #### trasformation of rankings to utilities
  if (rank==1) {
    y<-transform2utilities(y)
  }
  #########
  # dim(y) dim(bundles)
  df <- cbind(y, bundles)  #combine ratings with bundles
  # dim(df) class(df) head(df)
  n <- length(df)  #number of variables in data frame
  main.effects.model.fit <- lm(y ~ ., data = df)  #compute the lm model
  client.sim <- predict(main.effects.model.fit, newdata = market.profiles)  #now we use the model to predict the utility that each product profile offers to the respondent
  # names(main.effects.model.fit)
  utilities.profiles <- as.data.frame(client.sim)  #build the data frame with product profile utilities
  ######## Now we compute the same product profile utilities for the res tof the
  ######## data set

  for (client in 2:clients) {
    # select client ratings
    y <- as.data.frame(t(ratings[client, ]))
    names(y) <- "y"  #change client name to y
    #### trasformation of rankings to utilities
    if (rank==1) {
      y<-transform2utilities(y)
    }
    #########
    df <- cbind(y, bundles)  #column bind client's ratings with bundles design
    n <- length(df)
    main.effects.model.fit <- lm(y ~ ., data = df)  #fit model for client
    client.sim <- predict(main.effects.model.fit, newdata = market.profiles)
    utilities.profiles <- cbind(utilities.profiles, client.sim)
    # conjoint.fit
  }
  clients.names <- row.names(ratings)  #now we get the respondents' names
  names(utilities.profiles) <- clients.names  #we assign respondents' names to the data set
  utilities.profiles <- as.data.frame(t(utilities.profiles))
  return(utilities.profiles)
}

ms.fe.conjoint <- function(market.profiles, ratings, bundles, rank=0) {
  #  Copyright 2016 Jordi L. Sintas
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  #
  #' @title This function computes the market share of  product profiles
  #' @description
  #'
  #' This function computes the market share of  product profiles
  #' according to the first choice rule. Uses treatment effects to estimate part worth utilities of profiles;
  #' the matrix of profiles for which we are going to compute market
  #' profiles is a matrix of existing product profiles in the market (rows)
  #' by attributes (columns); ratings is a matrix of clients (rows) by
  #' bundles rated (colums); bundles is a matrix of bundles (rows)
  #' by attributes (colums)
  #'
  #' @param market.profiles                a data frame with the description of competitors' existing product profiles
  #' @param ratings                 a data frame frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' names(osc)
  #' sapply(osc, class)
  #' sapply(osc$market.proiles, class)
  #' ms.fe.conjoint(osc$market.profiles, osc$ratings, osc$bundles)
  #' class(ms.fe.conjoint(osc$market.profiles, osc$ratings, osc$bundles))

  ###################### other variables in the function ###########
  # up                      the results of the utilities.of.profiles() function
  # individual.elections    cols. identiy respondents for whech the product profiles reports the maximum utility
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names
  ############################
  up <- utilities.of.profiles(market.profiles, ratings, bundles, rank=rank)  #gets the matrix of utilities using the function utilitiesProfiles()
  individual.elections <- apply(up, 1, which.max)  # cols. identiy respondents for whech the product profiles reports the maximum utility
  # individual.elections #print the findings
  ms <- vector()  #initialize the vextor of market shares
  brands <- ncol(up)
  for (brand in 1:brands) {
    ms[brand] <- length(individual.elections[individual.elections ==
                                              brand])/length(individual.elections)  # compute market share for brand j
  }
  ms<-round(ms*100, digits=4)
  brand.names <- row.names(market.profiles)
  names(ms) <- brand.names
  return(as.data.frame(ms))
}

ms.us.conjoint <- function(market.profiles, ratings, bundles, rank=0) {
  #  Copyright 2016 Jordi L. Sintas
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  #
  #' @title This function computes the market share of  product profiles
  #' @description
  #'
  #' This function computes the market share of  product profiles
  #' according to the utility share rule. Uses treatment effects for estimating part worths of profiles;
  #' is the data frame of profiles for which we are going to compute market
  #' profiles is a matrix of product profiles in the market (rows)
  #' by attributes (columns); ratings is a matrix of clients (rows) by
  #' bundles rated (colums); bundles is a matrix of bundles (rows)
  #' by attributes (colums).
  #' @param market.profiles         a matrix with the description of competitors' product profiles
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' ms.us.conjoint(osc$market.profiles, osc$ratings, osc$bundles)
  #' class(ms.us.conjoint(osc$market.profiles, osc$ratings, osc$bundles))

  ############################ other variables in the function #########
  # up                      the results of the utilities.of.profiles() function
  # row.profiles.clients   compute clients' probability of buying each brand
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names
  #############################
  up <- utilities.of.profiles(market.profiles, ratings, bundles)  #gets the matrix of utilities using the function utilitiesProfiles()
  row.profiles.clients <- prop.table(as.matrix(up), 1) #compute de probability of buying each brand
  #sum.util.client <- rowSums(up)  #Sum rows across respondents
  #profiles.prob.client <- up/sum.util.client  #compute de probability of buying each brand
  # head(profiles.prob.client) sum(profiles.prob.client[1,]) check
  # whether sum = 1
  ms <- colMeans(row.profiles.clients)*100  #for each brand compute the mean across respondents
  ms<-round(ms, digits=4)
  brand.names <- row.names(market.profiles)
  names(ms) <- brand.names
  options(digits=3)
  return(as.data.frame(ms))
}

ms.logit.conjoint <- function(market.profiles, ratings, bundles, rank=0) {
  #  Copyright 2016 Jordi L. Sintas
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  #
  #' @title Computes the market share of the product profiles
  #' @description
  #'
  #' This function
  #' computes the market share of the product profiles according to the
  #' BTL, Bradley, Terry, and Luce rule. It uses treatment effects to estimate part worths of profiles;
  #' profiles is a matrix of product profiles in the market (rows)
  #' by attributes (columns); ratings is a matrix of clients (rows) by
  #' bundles rated (colums); bundles is a matrix of bundles (rows)
  #' by attributes (colums)
  #'
  #' @param market.profiles                a matrix with the description of competitors' product profiles
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<- MDSConjointData$osc
  #' names(osc)
  #' ms.logit.conjoint(osc$market.profiles, osc$ratings, osc$bundles)
  #' class(ms.logit.conjoint(osc$market.profiles, osc$ratings, osc$bundles))
  ##################### other variables in the function #############
  # up                      the results of the utilities.of.profiles() function
  # exp.row.profiles.clients  compute clients'  probability of buying each brand
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names

  up <- utilities.of.profiles(market.profiles, ratings, bundles, rank=rank)
  exp.up <- exp(up)
  exp.row.profiles.clients <- prop.table(as.matrix(exp.up), 1) #compute de probability of buying each brand
  #sum.exp.util.client <- rowSums(exp.up)  #Sum rows across respondents
  #profiles.prob.client <- exp.up/sum.exp.util.client  #compute de probability of buying each brand
  # head(profiles.prob.client) sum(profiles.prob.client[1,]) check
  # whether sum = 1
  options(scipen=999)
  ms <- colMeans(exp.row.profiles.clients)*100  #for each brand compute the mean across respondents
  # for each brand compute the mean across respondents
  ms<-round(ms, digits=4)
  brand.names <- row.names(market.profiles)
  names(ms) <- brand.names
  return(as.data.frame(ms))
}

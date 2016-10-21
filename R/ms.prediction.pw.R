fac2char <- function(mpi){
  #' @title This function transform factor variable into character type
  #' @description
  #' This functions takes as data frame with variables of factor type and transform them into
  #' a character type. It is usefull for computing product profile utility from a matrix of part worths
  #' @param  mpi a data frame
  #' @return mpi a data frame transformed
  #' @export
  #'
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' fac2char(osc$market.profiles)
  mpi<-data.frame(lapply(mpi, as.character), stringsAsFactors = FALSE)
  mpi<-as.character(mpi)
  return(mpi)
}


cupi <- function(pw, mp, n.attributes){
  #' @title This function computes for client i the utility of each product profile from a data frame of part worths
  #' @description
  #'
  #' This function computes for client i the utility of each market profile (given rivals' profiles and new profiles
  #' ) for  consumer i.  The data frame mp has the data of existing profiles
  #' for which we are going to compute the market shares.
  #' It is a table of market product profiles (rows) by
  #' attributes (columns), wp is a data frame of client i (rows) by part worths
  #'  (colums), design.l is a list with conjoint desing.
  #' @param mp             a data frame with the description of competitors' existing product profiles
  #' @param pw                a data frame with all clients' partworths
  #' @param n.attributes       the number attributs of conjoint design
  #' @return upi     a matrix with the prediction of utilities for  client i
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' #this function is called by uop().
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' #head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #' #osc.cupi <-cupi(osc$market.profiles, osc.conjoint$part.worths[1,], osc$design)
  #' #head(osc.cupi)
  pwa<-0
  pw.names<-names(pw)
  brand.names<-row.names(mp)
  n.mp<-nrow(mp)
  pw<-as.matrix(pw)
  mp<-as.matrix(mp)
  mpi<-fac2char(mp[1,])
  # compute utilities for market profile number 1
  for (j in 1:n.attributes) {
    index<-which(pw.names==mpi[j])
    pwa<-pwa+as.numeric(pw[index])
  }
  upi <- as.data.frame(pwa)  #build the data frame with product profile utilities
  #### Second and n  market  profiles
  for (i in 2:n.mp) {
    mpi<-fac2char(mp[i,])
    #mpi<-data.frame(lapply(mpi, as.character), stringsAsFactors = FALSE)
    #mpi<-as.character(mpi)
    pwa<-0
    for (j in 1:n.attributes) {
      index<-which(pw.names==mpi[j])
      pwa<-pwa+as.numeric(pw[index])
    }
    upi<-cbind(upi, pwa)

  }
  upi <- as.data.frame(upi)
  names(upi) <- brand.names
  return(upi)
}

uop <- function(mp, pw, design.l) {
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
  #' @title This function computes the utility of each product profile from a data frame of part worths
  #' @description
  #'
  #' This function computes the utility of each profile (given rivals' profiles and new profiles
  #' ) for each consumer.  The data frame mp has the data of existing profiles
  #' for which we are going to compute the market shares.
  #' It is a table of market product profiles (rows) by
  #' attributes (columns), wp is a data frame of clients (rows) by part worths
  #'  (colums), design.l is a list with conjoint desing.
  #' @param mp             a data frame with the description of competitors' existing product profiles
  #' @param pw                a data frame with all clients' partworths
  #' @param design.l                 a list with conjoint design
  #' @return up     a matrix with the prediction of utilities for all clients
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' #this function is called by market share function.
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' #head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #' osc.uop <-uop(osc$market.profiles, osc.conjoint$part.worths, osc$design)
  #' head(osc.uop)
  #'
  ################### other variables in the function ##################
  # n.attributes             number of attributs in conjoint design
  ######################
  n.attributes=length(design.l)
  up <- apply(pw, 1, cupi, mp, n.attributes)
  up<-do.call(rbind.data.frame, up)
  return(up)
  }

########
ms.fe.pw <- function(mp, pw, design.l) {
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
  #' @title This function computes the market share of  product profiles from a data frame of part worths
  #' @description
  #'
  #' This function computes the market share of  market product profiles
  #' according to the first choice rule. Uses a data frame with part woths to predict utilities
  #' market profiles, mp,  is a matrix of market product profiles (rows)
  #' by attributes (columns); pw  is a data frame of clients (rows) by
  #' attribut levels part worths (colums); design.l is a list with the definition of the conjoint design
  #'
  #' @param mp                a data frame with the description of competitors' existing product profiles
  #' @param pw                 a data frame frame with all clients' part worths
  #' @param design.l             a list with conjoint design
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' names(osc)
  #' sapply(osc, class)
  #' sapply(osc$market.proiles, class)
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' #head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #' ms.fe.pw(osc$market.profiles, osc.conjoint$part.worths, osc$design)
  #' class(ms.fe.pw(osc$market.profiles, osc$ratings, osc$bundles))

  ###################### other variables in the function ###########
  # up                      the results of the utilities.of.profiles() function
  # individual.elections    cols. identiy respondents for whech the product profiles reports the maximum utility
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names
  ############################
  up <- uop(mp, pw, design.l)  #gets the matrix of utilities using the function utilitiesProfiles()
  individual.elections <- apply(up, 1, which.max)  # cols. identiy respondents for whech the product profiles reports the maximum utility
  # individual.elections #print the findings
  ms <- vector()  #initialize the vextor of market shares
  brands <- ncol(up)
  for (brand in 1:brands) {
    ms[brand] <- length(individual.elections[individual.elections ==
                                               brand])/length(individual.elections)  # compute market share for brand j
  }
  ms<-round(ms*100, digits=4)
  brand.names <- row.names(mp)
  names(ms) <- brand.names
  return(as.data.frame(ms))
}

################
ms.us.pw <- function(mp, pw, design.l) {
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
  #' according to the utility share rule. Uses a data frame with part woths to predict utilities
  #' market profiles, mp,  is a matrix of market product profiles (rows)
  #' by attributes (columns); pw  is a data frame of clients (rows) by
  #' attribut levels part worths (colums); design.l is a list with the definition of the conjoint design.
  #' @param mp         a matrix with the description of competitors' product profiles
  #' @param pw                a data frame with all clients' part worths
  #' @param design.l        a a list with conjoint design
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' #head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #' ms.us.pw(osc$market.profiles, osc.conjoint$part.worths, osc$design)
  #' class(ms.us.pw(osc$market.profiles, osc$ratings, osc$bundles))

  ############################ other variables in the function #########
  # up                      the results of the utilities.of.profiles() function
  # row.profiles.clients   compute clients' probability of buying each brand
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names
  #############################
  up <- uop(mp, pw, design.l)  #gets the matrix of utilities using the function utilitiesProfiles()
  row.profiles.clients <- prop.table(as.matrix(up), 1) #compute de probability of buying each brand
  #sum.util.client <- rowSums(up)  #Sum rows across respondents
  #profiles.prob.client <- up/sum.util.client  #compute de probability of buying each brand
  # head(profiles.prob.client) sum(profiles.prob.client[1,]) check
  # whether sum = 1
  ms <- colMeans(row.profiles.clients)*100  #for each brand compute the mean across respondents
  ms<-round(ms, digits=4)
  brand.names <- row.names(mp)
  names(ms) <- brand.names
  options(digits=3)
  return(as.data.frame(ms))
}

###############

ms.logit.pw <- function(mp, pw, design.l) {
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
  #' logit rule. Uses a data frame with part woths to predict utilities
  #' market profiles, mp,  is a matrix of market product profiles (rows)
  #' by attributes (columns); pw  is a data frame of clients (rows) by
  #' attribut levels part worths (colums); design.l is a list with the definition of the conjoint design.
  #'
  #' @param mp                a matrix with the description of competitors' product profiles
  #' @param pw                a data frame with all clients' part worths
  #' @param design.l                 a list with conjoint design
  #' @return ms
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<- MDSConjointData$osc
  #' names(osc)
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' #head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #' ms.logit.pw(osc$market.profiles, osc.conjoint$part.worths, osc$design)
  #' class(ms.logit.pw(osc$market.profiles, osc$ratings, osc$bundles))
  ##################### other variables in the function #############
  # up                      the results of the utilities.of.profiles() function
  # exp.row.profiles.clients  compute clients'  probability of buying each brand
  # ms                     for each brand compute the mean across respondents
  # brand.name             collects brands' names

  up <- uop(mp, pw, design.l)
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
  brand.names <- row.names(mp)
  names(ms) <- brand.names
  return(as.data.frame(ms))
}

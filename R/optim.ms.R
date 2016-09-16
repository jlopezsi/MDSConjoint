############ Optim market share

optim.ms.first.choice <- function(ratings, bundles, market.profiles,
                                  design, rank=0, hpb=0) {
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
  #' @title Identifies the product profiles that maximazes  market share, maximum utility rule
  #' @aliases optim.ms.fe
  #' @keywords optim market share
  #' @description
  #' Computes market share for actual product profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use first choice rule
  #' If we combine all three functions into one function,
  #' we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for logit. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param market.profiles           a data frame with competitiors' profiles
  #' @param design          a list with the experiment description (attributes and levels)
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @param hpb                           if hpb==1, then hide the progress bar. This is for rmarkdown
  #' @importFrom utils setTxtProgressBar
  #' @importFrom utils txtProgressBar
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' class(osc)
  #' names(osc)
  #' rat<-osc$ratings
  #' bun<-osc$bundles
  #' mp<-osc$market.profiles
  #' design<-osc$design
  #'
  #' osc.ms.op.1choice<-optim.ms.first.choice(rat, bun, mp, design, hpb=1)
  #' osc.ms.op.1choice
  #' @export
  #######################other variables in the function################
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile

  rivals <- nrow(market.profiles)
  full.experiment = expand.grid(
    design
  )
  n.bundles <- nrow(full.experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations
  if (hpb!=1) {
    pb <- txtProgressBar(min = 0, max = n.bundles, style = 3) # progress bar
  }
  new.market.profiles <- rbind(market.profiles, Optim = full.experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule first choice or election
  ms.full.profile <- t(ms.fe.conjoint(new.market.profiles, ratings, bundles, rank=rank))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    new.market.profiles <- rbind(market.profiles, Optim = full.experiment[i, ])  #combines existing profiles with the i-essim possible combination
    ms.full.profile <- rbind(ms.full.profile, t(ms.fe.conjoint(new.market.profiles,
                                                             ratings, bundles, rank=rank)))  #combines the computations
    ##prepare bar
    if (hpb!=1) {
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }
  }
  if (hpb!=1) {
    Sys.sleep(1)
    close(pb)
  }
  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- full.experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}

optim.ms.utility.share <- function(ratings, bundles, market.profiles,
                                   design, rank=0, hpb=0) {
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
  #' @title Identifies the product profiles that maximazes  market share, share of utility rule
  #' @keywords optim market share
  #' @aliases  optim.ms.us
  #' @description
  #' Computes market share for actual profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use utility share rule
  #' If we combine all three functions into one function, we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for logit. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param market.profiles           a data frame with competitiors' profiles
  #' @param design          a list with the experiment description (attributes and levels) profiles
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @param hpb                     if hpb==1, then hide progress bar. This is for markdowm
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' class(osc)
  #' names(osc)
  #' rat<-osc$ratings
  #' bun<-osc$bundles
  #' mp<-osc$market.profiles
  #' design<-osc$design
  #' osc.ms.op.us<-optim.ms.utility.share(rat, bun, mp, design, hpb=1)
  #' osc.ms.op.us

  ####################### other variabels in the function ##############
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile
  # optim.list              a list with with the optim profile as well as optim market share
  # full.experiment              a data frame with the description of all possible profiles
  #'
  rivals <- nrow(market.profiles)
  full.experiment = expand.grid(
    design
  )
  n.bundles <- nrow(full.experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations
  if (hpb!=1) {
    pb <- txtProgressBar(min = 0, max = n.bundles, style = 3) # progress bar
  }

  new.market.profiles <- rbind(market.profiles, Optim = full.experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule share of utilities
  ms.full.profile <- t(ms.us.conjoint(new.market.profiles, ratings, bundles, rank=rank))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    new.market.profiles <- rbind(market.profiles, Optim = full.experiment[i, ])  #combines existing profiles with the i-essim possible combination
    #new.market.profiles
    ms.full.profile <- rbind(ms.full.profile, t(ms.us.conjoint(new.market.profiles,
                                                             ratings, bundles, rank=rank)))  #combines the computations
    ##prepare bar
    if (hpb!=1) {
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }
  }
  if (hpb!=1) {
    Sys.sleep(1)
    close(pb)
  }
  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- full.experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}

optim.ms.logit <- function(ratings, bundles, market.profiles, design, rank=0, hpb=0) {
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
  #' @title Identifies the product profiles that maximazes  market share, logit rule
  #' @keywords optim market share
  #' @aliases  optim.ms.logit
  #' @description
  #' Computes market share for actual profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use first choice rule
  #' If we combine all three functions into one function, we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for logit. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param market.profiles           a data frame with competitiors' profiles
  #' @param design          a list with the experiment description (attributes and levels)
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @param hpb                     if hpb==1, the hide progress bar, for rmarkdown documents
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @references SAS Institute Inc., SAS*TeclmicalReport R-109, Conjoint Analysis Examples, Gary, NC: SAS Institute Inc., 1993.85 pp.
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' class(osc)
  #' names(osc)
  #' rat<-osc$ratings
  #' bun<-osc$bundles
  #' mp<-osc$market.profiles
  #' design<-osc$design
  #' osc.ms.op.logit<-optim.ms.logit(rat, bun, mp, design, hpb=1)
  #' osc.ms.op.logit
  #' @export
  ####################### other variabels in the function ##############
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile
  # optim.list              a list with with the optim profile as well as optim market share
  # full.experiment              a data frame with the description of all possible profiles
  #########################
  rivals <- nrow(market.profiles)
  full.experiment = expand.grid(
    design
  )
  n.bundles <- nrow(full.experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations
  if (hpb!=1) {
    pb <- txtProgressBar(min = 0, max = n.bundles, style = 3) # progress bar
  }
  new.market.profiles <- rbind(market.profiles, Optim = full.experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule Bradley, Terry and Luce
  ms.full.profile <- t(ms.logit.conjoint(new.market.profiles, ratings, bundles, rank=rank))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    new.market.profiles <- rbind(market.profiles, Optim = full.experiment[i, ])  #combines existing profiles with the i-essim possible combination
    ms.full.profile <- rbind(ms.full.profile, t(ms.logit.conjoint(new.market.profiles,
                                                              ratings, bundles, rank=rank)))  #combines the computations
    if (hpb!=1) {
      ##prepare bar
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }
  }
  if (hpb!=1) {
    Sys.sleep(1)
    close(pb)
  }
  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- full.experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}

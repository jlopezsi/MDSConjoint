conjoint.estimation <- function(ratings, bundles, design.l, rank=0, conj.contrasts="sum") {
  #  part.worths.R
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
  ######
  #' @title conjoint.estimation
  #' @description
  #' This function computes the partworth of attribute levels for each
  #' respondent. Uses treatment effects; ratings is a data frame of clients
  #' (rows) by bundle rated (colums); bundles is a data frame of
  #' bundles (rows) by attributes (colums)
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param design.l                a list with the conjoint design (attributes and levels)
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @param conj.contrasts          if conj.contrasts=="sum", then  options(contrasts = c("contr.sum", "contr.poly")), otherwise, treatment
  #' @return conjoint.results.all    the object returned by the function
  #' @export
  #' @importFrom stats dist lm predict sd
  #' @examples
  #'
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' osc<-MDSConjointData$osc
  #' class(osc)
  #' names(osc)
  #' osc.conjoint <- conjoint.estimation(osc$ratings, osc$bundles, osc$design)
  #' names(osc.conjoint)
  #' # [1] "summary"     "fit"         "part.worths"  "prediction"
  #' head(osc.conjoint$summary)
  #' head(osc.conjoint$fit)
  #' head(osc.conjoint$part.worths)
  #' head(osc.conjoint$prediction)
  #
  ################### other variables in the function ###########
  # conjoint.fit            a data frame with the coefficients of many objects returned by lm()
  # clients                 number of clients that have rated the set of bundles
  # y                       the ratings of client number i
  # df                      a compposed file of client's ratings plus the bundles description according to bundles matrix
  # n                       number of variables in df data table
  # main.effects.model.fit  an object return by lm() function
  # clients.names           a vector with clients' names
  ######### fixing contrasts
  ifelse (conj.contrasts=="sum",
          options(contrasts = c("contr.sum", "contr.poly")),
                  options(contrasts = c("contr.treatment", "contr.poly")
                          )
  )
  ####### inicializing data objects
  conjoint.results.all<-list()
  conjoint.fit.summary<-list() #inicialize data frame
  conjoint.fit<-data.frame() #inicialize data frame
  conjoint.part.worths <- data.frame() #inicialize data frame
  conjoint.prediction<-data.frame() #inicialize data frame

  ###Preparing data for the first client
  clients <- nrow(ratings)  #counting the number of informants
  y <- as.data.frame(t(ratings[1, ]))  #selecting first informant data
  names(y) <- "y"  #changing the variable name to y
  #dim(y)
  #dim(bundles)
  #### trasformation of rankings to utilities
  if (rank==1) {
    y<-transform2utilities(y)
  }
  #########
  df <- cbind(y, bundles)  #combining first informant ratings with profiles
  #dim(df)
  #class(df)
  #head(df)
  n <- length(df)  #the number of variables in df
  ###### estimations of a lm model
  main.effects.model.fit<-lm(y~., data=df) #estimating the main effects conjoint model
  conjoint.results <- main.effects.model.fit[c("contrasts","xlevels","coefficients")]
  #conjoint.results
  #names(main.effects.model.fit)
  ### storing coefficients
  conjoint.fit<-as.data.frame(main.effects.model.fit[c("coefficients")]) #storing coefficients in data frame
  conjoint.prediction<-as.data.frame(predict(main.effects.model.fit, df))
  conjoint.results$attributes <- names(conjoint.results$contrasts)
  # storing complet summary
  conjoint.fit.summary[[row.names(ratings)[1]]] <- append(conjoint.fit.summary, summary(main.effects.model.fit))

  #call compute.importance.for.one function
  conjoint.results2<-compute.importance.for.one(conjoint.results)
  conjoint.part.worths <-as.data.frame(t(conjoint.results2$part.worths.vector))

  ############### computing conjoint for the rest of the data set.
  if (clients >1) {

    for (client in 2:clients) {
      # select client ratings
      y <- as.data.frame(t(ratings[client, ]))
      names(y) <- "y"  #change client name to y
      ####### transform a ranking into utilities
      if (rank==1) {
        y<-transform2utilities(y)
      }
      ##### end transformation #########
      df <- cbind(y, bundles)  #column bind client's ratings with bundles design
      n <- length(df)
      main.effects.model.fit <- lm(y ~ ., data = df)  #fit model for client
      #prepare conjoint.results for computing part worths
      conjoint.results <- main.effects.model.fit[c("contrasts","xlevels","coefficients")]
      conjoint.results$nlevels
      conjoint.results$attributes <- names(conjoint.results$contrasts)

      conjoint.fit.summary[[row.names(ratings)[client]]] <- append(conjoint.fit.summary, summary(main.effects.model.fit))
      #conjoint.fit
      # storing coefficients

      conjoint.fit <- cbind(conjoint.fit, main.effects.model.fit[c("coefficients")])
      # conjoint.fit
      conjoint.prediction<-cbind(conjoint.prediction, predict(main.effects.model.fit, df))

      #call compute.importance.for.one function

      conjoint.results2<-compute.importance.for.one(conjoint.results)
      conjoint.part.worths <-rbind(conjoint.part.worths, conjoint.results2$part.worths.vector)

    }
  }
  # preparing the data file for returning
  clients.names<-row.names(ratings) #selecting clients' names from ratings data frame
  names(conjoint.fit)<-clients.names #attaching clients' names to results
  conjoint.fit<-as.data.frame(t(conjoint.fit)) #transposing data before returning
  names(conjoint.prediction)<-clients.names
  conjoint.prediction <-as.data.frame(t(conjoint.prediction))
  ### prepare part.worths data frame
  level.names<-character() #composing levels of attribtes
  for (j in 1:length(design.l)){
    level.names<-c(level.names, eval(parse(text=paste("design.l$", names(design.l[j]), sep=""))))
  }
  row.names(conjoint.part.worths) <- clients.names
  names(conjoint.part.worths) <- level.names
  ###########
  options(contrasts=c("contr.treatment","contr.poly"))
  conjoint.results.all$summary <- conjoint.fit.summary
  conjoint.results.all$fit <- conjoint.fit
  conjoint.results.all$part.worths <- conjoint.part.worths
  conjoint.results.all$prediction <-conjoint.prediction
  return(conjoint.results.all)
}

############################## importance of attributes

importance.of.attributes<-function(ratings, bundles, design.l, rank=0)
  {
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
  #' @description
  #'
  #' This function computes attributes' importance. First I prepare the
  #' list conjoint.results, after that I compute the sample's mean. It uses
  #' sum zero contrasts to estimate part worths; ratings is a data frame of clients (rows) by bundles
  #' rated (colums); bundles is a data frame of bundles (rows) by attributes
  #' (colums); design.l is a list with the conjoint design (attributes and levels)
  #' Frist we prepare data
  #'
  #' @title Computes the importance of producy attributes
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param design.l                a list with attributs and their levels
  #' @param rank                    if rank==1, then transform a ranking into utilities
  #' @return conjoint.list          a list with three data frames:
  #'                                1) fit, with the model fited for each individual,
  #'                                2) part.worths, with the estimated part worths for all individuals
  #'                                3) imp, wiht the attributes' importance for each individual
  #'
  #' @export
  #' @examples
  #' data(MDSConjointData)
  #' names(MDSConjointData)
  #' names(MDSConjointData$osc)
  #' osc<-MDSConjointData$osc
  #' osc.imp<-importance.of.attributes(osc$ratings, osc$bundles, osc$design)
  #' names(osc.imp)
  #' head(osc.imp$fit)
  #' head(osc.imp$part.worths)
  #' head(osc.imp$imp)
  #' names(MDSConjointData$tire)
  #' tire<-MDSConjointData$tire
  #' tire.imp<-importance.of.attributes(tire$ratings, tire$bundles, tire$design)
  #' names(tire.imp)
  #' head(tire.imp$fit)
  #' head(tire.imp$part.worths)
  #' head(tire.imp$imp)
  #
  ################# other variables in the function ##################
  # clients                 number of clients that have rated the set of bundles
  # y                       the ratings of client number i
  # df                      a compposed file of client's ratings plus the bundles description according to bundles matrix
  # n                       number of variables in df data table
  # main.effects.model.fit  an object return by lm() function
  # clients.names           a vector with clients' names
  # conjoint.results        a list wiht contrasts, xlevels, attributes, and coeficients taken from main.effects.model.fit
  # part.worths             a vector with conjoint.results$xlevels
  # end.index.for.coefficient and index for coefficients
  # part.worths.vector      a vetor for collecting part worths
  # nlevels                 an index with attribute's number of levels
  # begin.index.for.coefficient an index for attribute's level coefficient
  # end.index.for.coefficient   an index for attribute's level coefficient
  # last.part.worth
  # clients.names           a vector with clients' names
  # level.names             a matrix with attributes' level names
  # row.names               a vector with levels' names
  #########################################
  ###### inicialize data objects
  options(contrasts=c("contr.sum","contr.poly"))
  conjoint.fit<-data.frame() #inicialize data frame
  conjoint.part.worths <- data.frame() #inicialize data frame
  conjoint.imp<-data.frame() #inicialize data frame
  conjoint.list<-list()
    ##### prepare data for the first estimation
  clients<-nrow(ratings) #counting the number of informants
  y<-as.data.frame(
    t(ratings[1, ]
      )
    ) #selecting first informant data
  names(y) <-"y" #changing the variable name to y
  #dim(y)
  #dim(bundles)
  #### trasformation of rankings to utilities
  if (rank==1) {
    y<-transform2utilities(y)
  }
  #########
  df<-cbind(y,bundles) #combining first informant ratings with profiles
  #dim(df)
  #class(df)
  #head(df)
  n<-length(df) #the number of variables in df
  main.effects.model.fit<-lm(y~., data=df) #estimating the main effects conjoint model
  #names(main.effects.model.fit)
  conjoint.fit<-as.data.frame(main.effects.model.fit[c("coefficients")]) #storing coefficients in data frame
  #conjoint.fit
  conjoint.results <- main.effects.model.fit[c("contrasts","xlevels","coefficients")]
  #conjoint.results
  conjoint.results$attributes <- names(conjoint.results$contrasts)
  #call compute.importance.for.one function
  conjoint.results2<-compute.importance.for.one(conjoint.results)
  conjoint.part.worths <-as.data.frame(t(conjoint.results2$part.worths.vector))
  conjoint.imp <-as.data.frame(t(conjoint.results2$attribute.importance))
  ############### computing conjoint for the rest of the data set.
  if(clients>1){

    for (client in 2:clients) {
      #select client ratings
      y<-as.data.frame(t(ratings[client,]))
      names(y) <-"y" #change client name to y
      #### trasformation of rankings to utilities
      if (rank==1) {
        y<-transform2utilities(y)
      }
      #########
      df<-cbind(y,bundles) #column bind client's ratings with bundles design
      n<-length(df)
      main.effects.model.fit<-lm(y~., data=df) #fit model for client
      conjoint.fit<- cbind(conjoint.fit, main.effects.model.fit[c("coefficients")])
      conjoint.results <- main.effects.model.fit[c("contrasts","xlevels","coefficients")]
      conjoint.results$nlevels
      conjoint.results$attributes <- names(conjoint.results$contrasts)
      #call compute.importance.for.one function

      #call compute.importance.for.one function
      conjoint.results2<-compute.importance.for.one(conjoint.results)
      conjoint.part.worths <-rbind(conjoint.part.worths, conjoint.results2$part.worths.vector)
      conjoint.imp <-rbind(conjoint.imp, conjoint.results2$attribute.importance)
    }
  }
  #preparing the data file for returning
  clients.names<-row.names(ratings) #selecting clients' names from ratings data frame
  level.names<-character() #composing levels of attribtes
  for (j in 1:length(design.l)){
    level.names<-c(level.names, eval(parse(text=paste("design.l$", names(design.l[j]), sep=""))))
  }
  names(conjoint.fit)<-clients.names #attaching clients' names to results
  conjoint.fit<-as.data.frame(t(conjoint.fit)) #transposing data before returning
  row.names(conjoint.part.worths) <- clients.names
  names(conjoint.part.worths) <- level.names
  row.names(conjoint.imp)<-clients.names
  options(contrasts=c("contr.treatment","contr.poly"))
  conjoint.list$fit<-conjoint.fit
  conjoint.list$part.worths<-conjoint.part.worths
  conjoint.list$imp<-conjoint.imp
  return(conjoint.list)
}

##############################################

compute.importance.for.one <- function(conjoint.results){
  #' @description
  #'
  #' This function computes attributes' importance for one individual. It takes the results of a conjoint analysis,
  #' the list conjoint.results, and then I compute the the importance of every attribute. It uses
  #' sum zero constraints to estimate part worths. I adapt  code from Marketing Data Science book, chapter 1.
  #' @title computes de importance of attributes for one individual
  #' @param conjoint.results        a list with the results of a conjoint analysis
  #' @return conjoint.results       returns the conjoint.results list modified
  #' @export
  #' @examples
  #' # not run This is a function called by importance.of.attributes()
  #' #data(MDSConjointData)
  #' #names(MDSConjointData$osc)
  #' #osc<-MDSConjointData$osc
  #' #osc.imp<-importance.of.attributes(osc$ratings, osc$bundles, osc$design)

  ################# other variables in the function ##################
  # conjoint.results        a list wiht contrasts, xlevels, attributes, and coeficients taken from main.effects.model.fit
  # part.worths             a vector with conjoint.results$xlevels
  # end.index.for.coefficient and index for coefficients
  # part.worths.vector      a vetor for collecting part worths
  # nlevels                 an index with attribute's number of levels
  # begin.index.for.coefficient an index for attribute's level coefficient
  # end.index.for.coefficient   an index for attribute's level coefficient
  # last.part.worth
  # level.names             a matrix with attributes' level names
  part.worths <- conjoint.results$xlevels
  # list of same structure as xlevels
  end.index.for.coefficient <- 1  # intitialize skipping the intercept
  part.worth.vector <- NULL # used for accumulation of part worths
  for(index.for.attribute in seq(along=conjoint.results$contrasts)) {
    nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
    #nlevels
    begin.index.for.coefficient <- end.index.for.coefficient + 1
    end.index.for.coefficient <- begin.index.for.coefficient + nlevels -2
    last.part.worth <- -sum(conjoint.results$coefficients[
      begin.index.for.coefficient:end.index.for.coefficient])
    part.worths[index.for.attribute] <-
      list(as.numeric(c(conjoint.results$coefficients[
        begin.index.for.coefficient:end.index.for.coefficient],
        last.part.worth)))
    part.worth.vector <- c(part.worth.vector,unlist(part.worths[index.for.attribute]))
  }
  conjoint.results$part.worths <- part.worths
  conjoint.results$part.worths.vector<-part.worth.vector
  ###### This function computes attributes' importance
  ###### conjoint results is a list with the data need to compute importance and print them
  ##########################################################################
  #I use the code from Marketing Data Science book, chapter 1.
  part.worth.ranges <- conjoint.results$contrasts
  for(index.for.attribute in seq(along=conjoint.results$contrasts))
    part.worth.ranges[index.for.attribute] <-
    dist(range(conjoint.results$part.worths[index.for.attribute]))
  conjoint.results$part.worth.ranges <- part.worth.ranges
  # compute and store importance values in % for each attribute
  sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))
  attribute.importance <- conjoint.results$contrasts
  for(index.for.attribute in seq(along=conjoint.results$contrasts))
    attribute.importance[index.for.attribute] <-
    (dist(range(conjoint.results$part.worths[index.for.attribute]))/
       sum.part.worth.ranges) * 100
  conjoint.results$attribute.importance <- unlist(attribute.importance)
  return(conjoint.results)
}

########### plot importances #################
visualize.importance <- function(part.worths, importance, design.l) {
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
  #' @description
  #' This functions print attributes' importance using R standard plotting system
  #' @title Plots the mean importance of attributes
  #' @param part.worths       a  data frame with as many rows as clients and as many columns as atributes' levels
  #' @param importance        a  data frame with as many rows as clinetes and as mnay columns as attributes' importance
  #' @param  design.l         a list with attributs and their levels
  #' @export
  #' @importFrom graphics axis pie plot
  #' @examples
  #' #data(MDSConjointData)
  #' #names(MDSConjointData)
  #' #osc<-MDSConjointData
  #' #osc.imp<-importance.of.attributes(osc$ratings, osc$bundles, osc$design)
  #' #names(osc.imp)
  #' #visualize.importance(osc.imp$part.worths, osc.imp$imp, osc$design)
  #' #tire <- MDSConjointData$tire
  #' #tire.imp<-importance.of.attributes(tire$ratings, tire$bundles, tire$design)
  #' #names(tire.imp)
  #' #visualize.importance(tires.imp$part.worths,tires.imp$imp, tire$design)
   #################other variables in the function ###############
  # n.attributes           a variable with the number of attributes
  # r                      a variable that defines the number of lines of a composed plot
  # proportions            defines the data percentages for the pie plot
  # lbls                   labels for attributes
  # pw.means               a vector with part worths' means
  # imp.means              a vector with importnace' means
  # data.to.plot           a numeric vector with levels' importnace for each attribute
  # ln                     labels of attributes' importance
  ########################
  pw.means <-apply(part.worths, 2, mean) #get part worths means
  imp.means <- apply(importance, 2, mean) # get attributes' importance
  n.attributes<-length(design.l) #get the number of attributes
  r<-abs(1+(n.attributes%/%2)) #define the number of lines of a composed plot. Only works with Rñ gui
  r # check the number of rows in the composed plot
  mfrow=c(r, 2) # define the grid of the composed plot
  proportions <- as.numeric(imp.means) #define the data percentages for the pie plot
  class(proportions)
  lbls <-names(imp.means) # get the labels of data
  proportions
  lbls <- paste(lbls, round(proportions), "%") #Attach % t the labels
  pie(proportions, lbls, main="Importance of attributes") #plot the pie chart
  data.to.plot<-numeric()
  index<-0
  for (j in 1:n.attributes) {
    data.to.plot<-c(data.to.plot, as.numeric(pw.means[(index+1):(
      (index+(length(
        design.l[[j]])))
    )]))
    index<-index+length(design.l[[j]])
    ln<-as.character(eval(parse(text = paste("design.l$", names(design.l[j]), sep="")))) #compose the the levels' labels
    plot(data.to.plot, type="b", xaxt="n", main=names(design.l[j]), xlab= "Levels", ylab="Utility")
    #axis(1, at=1:length(eval(parse(text = paste("design.l$", names(design.l[j]), sep="")))), labels=ln, las = 1) #plot the axis with the levels' names
    axis(1, at=1:length(ln), labels=ln, las = 1)
    data.to.plot<-numeric()
  }
  mfrow=c(1,1) #return to the standard plot
}

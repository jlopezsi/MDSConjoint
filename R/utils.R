transform2factor<-function(df){
  #' @title transform2factor
  #' @description
  #' This function transforms a data frame's variables into factors
  #' This function is a convenient way to  quickly prepare data from other packages to
  #' test with this package
  #' @param df  A data frame with variables in numeric format
  #' @return df a data frame with all variables in factor format
  #' @export
  #' @examples
  #' n = c(2, 3, 5)
  #' s = c("aa", "bb", "cc")
  #' b = c(TRUE, FALSE, TRUE)
  #' df = data.frame(n, s, b)
  #' df
  #' class(df$n)
  #' class(df$s)
  #' class(df$b)
  #' sapply(df, class)
  #'
  #' df.factor <- transform2factor(df)
  #' class(df$n)
  #' class(df$s)
  #' class(df$b)
  for (i in 1:length(df)) {
    df[,i]<-as.factor(df[,i])
  }
  return(df)
}

transform2utilities <- function(y){
  #' @title transform2utilities
  #' @description
  #' This function transforms a vector of ranked profiles into a vector of utilities
  #' It uses SAS monotonic transformation of preference judgements as described in SAS technical Report R-109, p. 2.
  #' @param y  A vector of bundles ranked by clients
  #' @return y A vector of bundles transformed into utilities
  #' @export
  #' @examples
  #' ranking<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
  #' ranking
  #' ratings <- transform2utilities(ranking)
  #' ratings
  #' @references
  #' SAS Institute Inc., SAS Technical Report R-109, Conjoint Analysis Examples, Cary, NC: SAS Institute Inc., 1993.85 pp.
  #'
  y.mean <-mean(y)
  for (i in 1:length(y)){
    y[i]<- -(y[i]-y.mean)+y.mean
  }
  return(y)
}

df2list<-function(df){
  #' @title df2list
  #' @description
  #' This function transforms a data frame's into a list according to columns
  #' @param df  A data frame
  #' @return df.list a list
  #' @export
  #' @examples
  #' xy.df <- data.frame(x = runif(10),  y = runif(10))
  #' df2list(xy.df)
  df.list <- list()
  for (i in 1:ncol(df)) {
    df.list[[names(df[i])]] <- as.character(df[,i])
  }
  names(df.list)<-names(df)
  df.list <-lapply(df.list, function(x) x[!is.na(x)])
  return(df.list)
}

#' Workflow with Random Forest learning algorithm
#'
#' @param form A model formula
#' @param train Train data
#' @param test Test data
#' @param ntrees Number of trees (algorithm parameter)
#' @param rstrategy Name of Resampling Strategy
#' @param rs.pars Parameters of Resampling Strategy
#' @param return.model If TRUE, this function will return the model and not its predictive performance. Default FALSE.
#' @param ... Other parameters for the learning algorithm
#'
#' @import UBL
#'
#' @return Evaluation Vector
#'
wf.RandomForest <- function(form,train,test,ntrees=500,rstrategy=NULL,rs.pars=list(),return.model=FALSE,...) {

  ind.y <- which(colnames(train)==as.character(form[[2]]))
  eval <- NA

  if(return.model) {

    if(is.logical(train[,ind.y])) {
      train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    }

    if(!is.null(rstrategy) && rstrategy!="") {
      train <- do.call(what = rstrategy, args = c(list(form=form, train=train), rs.pars))
    }

    m <- ranger::ranger(formula = form, data = train, num.trees = ntrees, ...)

    eval <- m

  } else {

    if(is.logical(train[,ind.y])) {
      train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
      test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
    }

    if(!is.null(rstrategy) && rstrategy!="") {
      train <- do.call(what = rstrategy, args = c(list(form=form, train=train), rs.pars))
    }

    m <- ranger::ranger(formula = form, data = train, num.trees = ntrees, ...)

    p <- stats::predict(m, test)$predictions

    eval <- eval.func(form,train,test,p)

  }

  eval

}

#' Decision Tree (Landmarker)
#'
#' @param form A model formula
#' @param train Train data
#' @param test Test data
#' @param maxdepth Depth parameter for tree-based learning
#' @param ... Additional parameters for rpart.control
#'
#' @return Evaluation vector
#'
wf.Tree <- function(form,train,test,maxdepth=4,...) {

  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }

  m <- rpart::rpart(formula = form, data = train, control = rpart::rpart.control(maxdepth = maxdepth, ...))

  p <- stats::predict(m, test, type="class")

  eval <- eval.func(form,train,test,p)

  eval

}

#' Naive Bayes (Landmarker)
#'
#' @param form A model formula
#' @param train Train data
#' @param test Test data
#' @param ... Additional parameters for rpart.control
#'
#' @return Evaluation Vector
#'
wf.NaiveBayes <- function(form,train,test,...) {

  ind.y <- which(colnames(train)==as.character(form[[2]]))
  if(is.logical(train[,ind.y])) {
    train[,ind.y] <- as.factor(as.numeric(train[,ind.y]))
    test[,ind.y] <- as.factor(as.numeric(test[,ind.y]))
  }

  m <- e1071::naiveBayes(formula = form, data = train)

  p <- stats::predict(m, test, type="class")

  eval <- eval.func(form,train,test,p)

  eval

}

#######################################################

#' Evaluation function to assess predictive ability of models
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param test A data.frame object with the test set
#' @param preds A vector with the predictions of a given model
#'
#' @return Evaluation Vector
#'
eval.func <- function(form, train, test, preds) {

  pos <- classNames(form,train)[2]

  y_test <- test[,which(colnames(test)==form[[2]])]

  eval.res <- c()

  acc <- Metrics::accuracy(actual = y_test, predicted = preds)
  f1.score <- fscore(y_test,preds,positive = pos); if(length(f1.score)==0) f1.score <- 0
  auc <- tryCatch({
    AUC::auc(AUC::roc(preds,y_test))
  }, error = function(e) { NA }
  )

  c(Accuracy=acc, F1=f1.score, AUC=auc)

}

#######################################################

#' Calculates the F1-Score for a pair set of true and predicted values
#'
#' @param y_true Vector of size n with true values
#' @param y_pred Vector of size n with predicted values
#' @param positive String with the name of the positive class. If none is attributed it will select the least frequent class. Note: the distribution of classes in the population and the sample may be significantly different.
#'
#' @return F-Score value
#'
fscore <- function(y_true,y_pred,positive = NULL) {

  # THIS IS WRONG. FORM IS NOT A VARIABLE IN THIS ENVIRONMENT
  # if(is.null(positive)) positive <- classNames(form,y_true)[2]

  conf <- MLmetrics::ConfusionDF(y_pred,y_true)

  tp <- conf[conf$y_true==positive & conf$y_pred==positive,]$Freq
  fp <- conf[conf$y_true!=positive & conf$y_pred==positive,]$Freq
  fn <- conf[conf$y_true==positive & conf$y_pred!=positive,]$Freq

  prec <- tp/(tp+fp)
  rec <- tp/(tp+fn)

  f1.score <- (2*(prec*rec)/(prec+rec))

  f1.score

}


#######################################################


#' Returns the names of classes
#'
#' @description This function returns the names of classes in a nominal and binary target variable.
#' The vector is ordered: the majority class name is in the first position, and the minority class in the second.
#'
#' @param form A model formula
#' @param ds A data.frame object with the training data
#'
#' @return A vector
#'
classNames <- function(form, ds) {

  tgt <- ds[,which(colnames(ds)==form[[2]])]
  tbl <- table(tgt)

  ind.y <- as.numeric(which(table(tgt)==max(table(tgt))))

  maj.y <- names(tbl)[ind.y]
  min.y <- names(tbl)[-ind.y]

  c(maj.y, min.y)

}

#######################################################


#' Percentual Difference
#'
#' @description This function calculates the percentual difference between two values (a - b)/b * 100
#'
#' @param a Numerical value
#' @param b Numerical value
#'
#' @return Numerical value
#'
percDiff <- function(a, b) {

  (abs(a - b)/mean(c(abs(a),abs(b))))*100

}

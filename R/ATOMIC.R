#' Automated Imbalanced Classification
#'
#' A meta-learning-based AutoML approach to solving imbalanced classification tasks with resampling strategies.
#'
#' @param form A model formula
#' @param train Training data
#' @param nmodels Number of models to consider. Default is 20. Only such number of top-k models (based on internal validation performance with cross-validation methodology) will be tested.
#' @param metric Evaluation metric used for assessing the optimisation of predictive performance. Default is F1-Score
#' @param numCores number of cores for parallel computing
#' @param ... Other parameters
#'
#' @return A predictive model containing the workflow (algorithm+resampling strategy) that are estimated to optimise the generalisation error.
#' @export
#'
#' @importFrom foreach %do%
#'
#' @examples
#' \dontrun{
#'
#' library(mlbench)
#'
#' data(PimaIndiansDiabetes)
#'
#' form <- diabetes ~ .
#'
#' atomic.m <- ATOMIC(form,PimaIndiansDiabetes)
#'
#' }
ATOMIC <- function(form, train, nmodels=20, metric="F1", numCores=1, ...) {

  if(!(metric %in% c("F1"))) return(cat("Metric not implemented yet. Let me know at https://github.com/nunompmoniz/autoresampling"))

  metafeats <- getMetaFeatures(train, form,numCores=numCores)
  wfconf <- sysdata$wf.config.class

  metadb <- cbind(wfconf,metafeats)

  metadb <- merge(metadb,sysdata$agg.class,by=c("ntrees","RStrategy","k","und.perc","ove.perc"),all.x=TRUE)

  xgbTest <- xgboost::xgb.DMatrix(data=data.matrix(metadb))

  preds <- stats::predict(sysdata$metamodel.class, xgbTest)

  ranks <- rank(-preds,ties.method="random")

  select.wfs <- metadb[order(ranks),1:5]; select.wfs$RStrategy <- as.character(select.wfs$RStrategy)

  #create parellel computing structure
  doParallel::registerDoParallel(numCores)

  nf <- NULL
  cv.eval <- foreach::foreach(nf=1:nmodels, .combine=cbind) %do% {

    cat("Estimating the validation error of model ",nf, "/", nmodels, " ...\n")

    res <- NULL
    m.select <- select.wfs[nf,]

    if(m.select$ntrees==-1) {

      if(m.select$RStrategy=="None") {
        res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                       rstrategy=NULL, rs.pars=NULL, percTest=NULL, ...)[,metric]
      } else {
        res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                       rstrategy=m.select$RStrategy, rs.pars=NULL, percTest=NULL, ...)[,metric]
      }

    } else if(m.select$RStrategy=="rs.ENN") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=m.select[,c("k")], percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="rs.RandUnder") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=m.select[,c("und.perc")], percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="rs.RandOver") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=m.select[,c("ove.perc")], percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="rs.ImpSamp") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=m.select[,c("und.perc","ove.perc")], percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="rs.SMOTE") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=m.select[,c("und.perc","ove.perc")], percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="rs.TomekUnder") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=m.select$RStrategy, rs.pars=NULL, percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else if(m.select$RStrategy=="None") {

      res <- kf_xval(train, form, 10, wf.RandomForest, average_results=FALSE, seedlock=TRUE,
                     rstrategy=NULL, rs.pars=NULL, percTest=NULL, ntrees=m.select$ntrees, ...)[,metric]

    } else {

      warning("Unknown resampling strategy... Ignoring it.")

      res <- NA

    }

    res

  }

  cv.res <- colMeans(cv.eval); if(any(is.na(cv.res))) cv.res[is.na(cv.res)] <- 0
  best <- select.wfs[which(cv.res==max(cv.res))[1],]

  best.model <- NA

  if(best$ntrees==-1) {

    if(best$RStrategy=="None") {
      best.model <- wf.RandomForest(form, train, NULL,
                              rstrategy = NULL, rs.pars = NULL, return.model = TRUE, ...)
    } else {
      best.model <- wf.RandomForest(form, train, NULL,
                             rstrategy = best$RStrategy, rs.pars = NULL, return.model = TRUE, ...)
    }

  } else if(best$RStrategy=="rs.ENN") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = best[c("k")], ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="rs.RandUnder") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = best[c("und.perc")], ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="rs.RandOver") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = best[c("ove.perc")], ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="rs.ImpSamp") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = best[c("und.perc","ove.perc")], ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="rs.SMOTE") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = best[c("und.perc","ove.perc")], ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="rs.TomekUnder") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = best$RStrategy, rs.pars = NULL, ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  } else if(best$RStrategy=="None") {

    best.model <- wf.RandomForest(form, train, NULL,
                                  rstrategy = NULL, rs.pars = NULL, ntrees = best$ntrees,
                                  return.model = TRUE, ...)

  }

  structure(list(best.model=best.model,
       cv.results=cbind(select.wfs[1:nmodels,],MeanPerformance=cv.res)), class="atomic")

}

#' Prediction method for the ATOMIC method
#'
#' Standard prediction method applied to an instance of the ATOMIC class
#'
#' @param object A prediction model
#' @param test Test data
#' @param ... Additional parameters
#'
#' @return A vector of predicted values
#' @method predict atomic
#' @export
#'
#' @examples
#' \dontrun{
#' library(mlbench)
#'
#' data(PimaIndiansDiabetes)
#'
#' ind <- sample(1:nrow(PimaIndiansDiabetes), 0.7*nrow(PimaIndiansDiabetes))
#'
#' train <- PimaIndiansDiabetes[ind,]
#' test <- PimaIndiansDiabetes[-ind,]
#'
#' form <- diabetes ~ .
#'
#' atomic.m <- ATOMIC(form,train)
#'
#' preds <- predict(atomic.m, test)
#' }
#'
predict.atomic <- function(object, test, ...) {

  preds <- stats::predict(object$best.model, test, ...)$predictions

  preds

}

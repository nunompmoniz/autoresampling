#' Edited Nearest Neighbour
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param k Number of nearest neighbours to consider (default is 3)
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.ENN <- function(form, train, k=3, ...) {

  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")

  new.ds <- UBL::ENNClassif(form = form, dat = train, k = k, dist = distance, ...)[[1]]

  new.ds

}

########################################################

#' Undersampling with Tomek links
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param rem Indicates if both parts of the Tomek link should be removed ("both") or solely the case from the majority class ("maj")
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.TomekUnder <- function(form, train, rem = "maj", ...) {

  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")

  new.ds <- UBL::TomekClassif(form = form, dat = train, dist = distance, rem = rem, ...)[[1]]

  new.ds

}

########################################################

#' Random Undersampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.RandUnder <- function(form, train, und.perc=0.5, ...) {

  nms <- classNames(form, train)

  lst <- list(und.perc,0); names(lst) <- nms

  new.ds <- UBL::RandUnderClassif(form = form, dat = train, C.perc = lst[1], ...)

  new.ds

}

########################################################

#' Random Oversampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param ove.perc Oversampling percentage for the minority class (e.g. 20\% oversampling corresponds to 0.2)
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.RandOver <- function(form, train, ove.perc=0.5, ...) {

  nms <- classNames(form, train)

  lst <- list(1,(1+ove.perc)); names(lst) <- nms

  new.ds <- UBL::RandOverClassif(form = form, dat = train, C.perc = lst[2], ...)

  new.ds

}

########################################################

#' ADASYN
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param beta Balance level after synthetic case generation - 1 corresponds to fully balanced classes
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.Adasyn <- function(form, train, beta=0.5, ...) {

  nms <- classNames(form,train)

  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")

  new.ds <- UBL::AdasynClassif(form = form, dat = train, beta = beta, dist = distance, baseClass = nms[1], ...)

  new.ds

}

########################################################

#' Importance Sampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.ImpSamp <- function(form, train, und.perc = 1, ove.perc = 0, ...) {

  nms <- classNames(form, train)

  lst <- list(und.perc, (1+ove.perc)); names(lst) <- nms

  new.ds <- UBL::ImpSampClassif(form = form, dat = train, C.perc = lst, ...)

  new.ds

}

########################################################

#' Synthetic Minority Over-sampling Technique (SMOTE)
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... Additional parameters for the resampling strategy
#'
#' @return New data set
#'
rs.SMOTE <- function(form, train, und.perc = 1, ove.perc = 0, ...) {

  nms <- classNames(form, train)

  lst <- list(und.perc, (1+ove.perc)); names(lst) <- nms

  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")

  new.ds <- UBL::SmoteClassif(form = form, dat = train, C.perc = lst, dist = distance, ...)

  new.ds

}


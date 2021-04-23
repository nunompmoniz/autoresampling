########################################################
#'
#' 1. Attributes with outliers
#'
########################################################

#' Attributes with outliers
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A numeric value with the number of attributes where outliers are present
#'
#' @importFrom grDevices boxplot.stats
#'
attrWithOutliers <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  attrs <- 0

  if(length(numattrs)>0) {
    for(i in 1:length(numattrs)) {

      if(length(boxplot.stats(ds[,numattrs[i]])$out)>0) attrs <- attrs + 1

    }
  }

  attrs

}

########################################################
#'
#' 2. Correlation between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on the Correlation between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of values concerning correlation between numerical attributes,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsCor_NumAttrs <- function(ds, tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  # create 
  tmp <- stats::cor(ds[,numattrs])
  v_cor <- tmp[upper.tri(tmp, diag = FALSE)]
 # v_cor <- c()
#
 # if(length(numattrs)>1) {
#
 #   for(i in 1:(length(numattrs)-1)) {
#
 #     for(j in 2:length(numattrs)) {
#
 #       v_cor <- c(v_cor,stats::cor(ds[,numattrs[i]],ds[,numattrs[j]]))
#
 #     }
#
 #   }
#
 # }

  v_cor <- v_cor[!is.na(v_cor)]

  mincor <- ifelse(length(v_cor)==0,NA,base::min(v_cor,na.rm=TRUE))
  maxcor <- ifelse(length(v_cor)==0,NA,base::max(v_cor,na.rm=TRUE))
  avgcor <- ifelse(length(v_cor)==0,NA,mean(v_cor,na.rm=TRUE))
  sdcor <- ifelse(length(v_cor)==0,NA,stats::sd(v_cor,na.rm=TRUE))
  varcor <- ifelse(length(v_cor)==0,NA,stats::var(v_cor,na.rm=TRUE))

  histcor <- c()
  if(length(v_cor)>1 && !all(v_cor == v_cor[1])) {
    histcor <- as.vector(graphics::hist(v_cor,breaks=seq(from=base::min(v_cor), to=base::max(v_cor), by=(base::max(v_cor)-base::min(v_cor))/10), plot = FALSE)$counts)
  } else { histcor <- c(rep(NA,10))}

  names(histcor) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=mincor, max=maxcor, avg=avgcor, sd=sdcor, var=varcor, t(histcor))

}

########################################################
#'
#' 3. Interquartile Range (IQR)
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Interquartile Range of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' IQR values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsIQR_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_iqr <- c()

  if(length(numattrs)>0) {

    for(i in 1:length(numattrs)) {
      v_iqr <- c(v_iqr,stats::IQR(ds[,numattrs[i]]))
    }

  }

  v_iqr <- v_iqr[!is.na(v_iqr)]
  miniqr <- ifelse(length(v_iqr)==0,NA,base::min(v_iqr,na.rm=TRUE))
  maxiqr <- ifelse(length(v_iqr)==0,NA,base::max(v_iqr,na.rm=TRUE))
  avgiqr <- ifelse(length(v_iqr)==0,NA,mean(v_iqr,na.rm=TRUE))
  sdiqr <- ifelse(length(v_iqr)==0,NA,stats::sd(v_iqr,na.rm=TRUE))
  variqr <- ifelse(length(v_iqr)==0,NA,stats::var(v_iqr,na.rm=TRUE))

  histiqr <- c()
  if(length(v_iqr)>1 && !all(v_iqr == v_iqr[1])) {
    histiqr <- as.vector(graphics::hist(v_iqr,breaks=seq(from=base::min(v_iqr), to=base::max(v_iqr), by=(base::max(v_iqr)-base::min(v_iqr))/10), plot = FALSE)$counts)
  } else { histiqr <- c(rep(NA,10))}

  names(histiqr) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=miniqr, max=maxiqr, avg=avgiqr, sd=sdiqr, var=variqr, t(histiqr))

}

########################################################
#'
#' 4. Geary's Kurtosis of Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Geary's Kurtosis of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' (Geary) kurtosis values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsGKur_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_gkur <- c()

  if(length(numattrs)>0) {

    for(i in 1:length(numattrs)) {
      v_gkur <- c(v_gkur,moments::geary(ds[,numattrs[i]]))
    }

  }

  v_gkur <- v_gkur[!is.na(v_gkur)]
  mingkur <- ifelse(length(v_gkur)==0,NA,base::min(v_gkur,na.rm=TRUE))
  maxgkur <- ifelse(length(v_gkur)==0,NA,base::max(v_gkur,na.rm=TRUE))
  avggkur <- ifelse(length(v_gkur)==0,NA,mean(v_gkur,na.rm=TRUE))
  sdgkur <- ifelse(length(v_gkur)==0,NA,stats::sd(v_gkur,na.rm=TRUE))
  vargkur <- ifelse(length(v_gkur)==0,NA,stats::var(v_gkur,na.rm=TRUE))

  histgkur <- c()
  if(length(v_gkur)>1 && !all(v_gkur == v_gkur[1])) {
    histgkur <- as.vector(graphics::hist(v_gkur,breaks=seq(from=base::min(v_gkur), to=base::max(v_gkur), by=(base::max(v_gkur)-base::min(v_gkur))/10), plot = FALSE)$counts)
  } else { histgkur <- c(rep(NA,10))}

  names(histgkur) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=mingkur, max=maxgkur, avg=avggkur, sd=sdgkur, var=vargkur, t(histgkur))

}

########################################################
#'
#' 5. Pearson's Kurtosis of Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Pearson's Kurtosis of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' (Pearson) kurtosis values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsPKur_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_pkur <- c()

  if(length(numattrs)>0) {

    for(i in 1:length(numattrs)) {
      v_pkur <- c(v_pkur,moments::kurtosis(ds[,numattrs[i]]))
    }

  }

  v_pkur <- v_pkur[!is.na(v_pkur)]
  minpkur <- ifelse(length(v_pkur)==0,NA,base::min(v_pkur,na.rm=TRUE))
  maxpkur <- ifelse(length(v_pkur)==0,NA,base::max(v_pkur,na.rm=TRUE))
  avgpkur <- ifelse(length(v_pkur)==0,NA,mean(v_pkur,na.rm=TRUE))
  sdpkur <- ifelse(length(v_pkur)==0,NA,stats::sd(v_pkur,na.rm=TRUE))
  varpkur <- ifelse(length(v_pkur)==0,NA,stats::var(v_pkur,na.rm=TRUE))

  histpkur <- c()
  if(length(v_pkur)>1 && !all(v_pkur == v_pkur[1])) {
    histpkur <- as.vector(graphics::hist(v_pkur,breaks=seq(from=base::min(v_pkur), to=base::max(v_pkur), by=(base::max(v_pkur)-base::min(v_pkur))/10), plot = FALSE)$counts)
  } else { histpkur <- c(rep(NA,10))}

  names(histpkur) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minpkur, max=maxpkur, avg=avgpkur, sd=sdpkur, var=varpkur, t(histpkur))

}

########################################################
#'
#' 6. Skewness of Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Skewness of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' (Pearson) skewness values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsSkew_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_skew <- c()

  if(length(numattrs)>0) {

    for(i in 1:length(numattrs)) {
      v_skew <- c(v_skew,moments::skewness(ds[,numattrs[i]]))
    }

  }

  v_skew <- v_skew[!is.na(v_skew)]
  minskew <- ifelse(length(v_skew)==0,NA,base::min(v_skew,na.rm=TRUE))
  maxskew <- ifelse(length(v_skew)==0,NA,base::max(v_skew,na.rm=TRUE))
  avgskew <- ifelse(length(v_skew)==0,NA,mean(v_skew,na.rm=TRUE))
  sdskew <- ifelse(length(v_skew)==0,NA,stats::sd(v_skew,na.rm=TRUE))
  varskew <- ifelse(length(v_skew)==0,NA,stats::var(v_skew,na.rm=TRUE))

  histskew <- c()
  if(length(v_skew)>1 && !all(v_skew == v_skew[1])) {
    histskew <- as.vector(graphics::hist(v_skew,breaks=seq(from=base::min(v_skew), to=base::max(v_skew), by=(base::max(v_skew)-base::min(v_skew))/10), plot = FALSE)$counts)
  } else { histskew <- c(rep(NA,10))}

  names(histskew) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minskew, max=maxskew, avg=avgskew, sd=sdskew, var=varskew, t(histskew))

}

########################################################
#'
#' 7. Coefficient of Variation
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Coefficient of Variation of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' coefficient of variation values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsCoV_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_cov <- c()

  if(length(numattrs)>0) {

    for(i in 1:length(numattrs)) {
      v_cov <- c(v_cov, (stats::sd(ds[,numattrs[i]]) / mean(ds[,numattrs[i]])) )
    }

  }

  v_cov <- v_cov[!is.na(v_cov)]
  mincov <- ifelse(length(v_cov)==0,NA,base::min(v_cov,na.rm=TRUE))
  maxcov <- ifelse(length(v_cov)==0,NA,base::max(v_cov,na.rm=TRUE))
  avgcov <- ifelse(length(v_cov)==0,NA,mean(v_cov,na.rm=TRUE))
  sdcov <- ifelse(length(v_cov)==0,NA,stats::sd(v_cov,na.rm=TRUE))
  varcov <- ifelse(length(v_cov)==0,NA,stats::var(v_cov,na.rm=TRUE))

  histcov <- c()
  if(length(v_cov)>1 && !all(v_cov == v_cov[1])) {
    histcov <- as.vector(graphics::hist(v_cov,breaks=seq(from=base::min(v_cov), to=base::max(v_cov), by=(base::max(v_cov)-base::min(v_cov))/10), plot = FALSE)$counts)
  } else { histcov <- c(rep(NA,10))}

  names(histcov) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=mincov, max=maxcov, avg=avgcov, sd=sdcov, var=varcov, t(histcov))

}

########################################################
#'
#' 8-12. Eval all the following metrics on Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MIC score,
#' and a vector with the cardinality of values after binning (10 bins)
#'

statsALL_NumAttrs <- function(ds,tgt,numCores=1) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)
  
  tmp <- minerva::mine(x=as.matrix(ds[,numattrs]),n.cores=numCores)
  
  #MIC
  v_mic <- tmp$MIC[upper.tri(tmp$MIC, diag = FALSE)]
  v_mic <- v_mic[!is.na(v_mic)]
  minmic <- ifelse(length(v_mic)==0,NA,base::min(v_mic,na.rm=TRUE))
  maxmic <- ifelse(length(v_mic)==0,NA,base::max(v_mic,na.rm=TRUE))
  avgmic <- ifelse(length(v_mic)==0,NA,mean(v_mic,na.rm=TRUE))
  sdmic <- ifelse(length(v_mic)==0,NA,stats::sd(v_mic,na.rm=TRUE))
  varmic <- ifelse(length(v_mic)==0,NA,stats::var(v_mic,na.rm=TRUE))
  histmic <- c()
  if(length(v_mic)>1 && !all(v_mic == v_mic[1])) {
    histmic <- as.vector(graphics::hist(v_mic,breaks=seq(from=base::min(v_mic), to=base::max(v_mic), by=(base::max(v_mic)-base::min(v_mic))/10), plot = FALSE)$counts)
  } else { histmic <- c(rep(NA,10))}
  names(histmic) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")
  out.mic = cbind(min=minmic, max=maxmic, avg=avgmic, sd=sdmic, var=varmic, t(histmic))

  #MAS
  v_mas <- tmp$MAS[upper.tri(tmp$MAS, diag = FALSE)]
  v_mas <- v_mas[!is.na(v_mas)]
  minmas <- ifelse(length(v_mas)==0,NA,base::min(v_mas,na.rm=TRUE))
  maxmas <- ifelse(length(v_mas)==0,NA,base::max(v_mas,na.rm=TRUE))
  avgmas <- ifelse(length(v_mas)==0,NA,mean(v_mas,na.rm=TRUE))
  sdmas <- ifelse(length(v_mas)==0,NA,stats::sd(v_mas,na.rm=TRUE))
  varmas <- ifelse(length(v_mas)==0,NA,stats::var(v_mas,na.rm=TRUE))
  histmas <- c()
  if(length(v_mas)>1 && !all(v_mas == v_mas[1])) {
    histmas <- as.vector(graphics::hist(v_mas,breaks=seq(from=base::min(v_mas), to=base::max(v_mas), by=(base::max(v_mas)-base::min(v_mas))/10), plot = FALSE)$counts)
  } else { histmas <- c(rep(NA,10))}
  names(histmas) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")
  out.mas = cbind(min=minmas, max=maxmas, avg=avgmas, sd=sdmas, var=varmas, t(histmas))

  #MEV
  v_mev <- tmp$MEV[upper.tri(tmp$MEV, diag = FALSE)]
  v_mev <- v_mev[!is.na(v_mev)]
  minmev <- ifelse(length(v_mev)==0,NA,base::min(v_mev,na.rm=TRUE))
  maxmev <- ifelse(length(v_mev)==0,NA,base::max(v_mev,na.rm=TRUE))
  avgmev <- ifelse(length(v_mev)==0,NA,mean(v_mev,na.rm=TRUE))
  sdmev <- ifelse(length(v_mev)==0,NA,stats::sd(v_mev,na.rm=TRUE))
  varmev <- ifelse(length(v_mev)==0,NA,stats::var(v_mev,na.rm=TRUE))
  histmev <- c()
  if(length(v_mev)>1 && !all(v_mev == v_mev[1])) {
    histmev <- as.vector(graphics::hist(v_mev,breaks=seq(from=base::min(v_mev), to=base::max(v_mev), by=(base::max(v_mev)-base::min(v_mev))/10), plot = FALSE)$counts)
  } else { histmev <- c(rep(NA,10))}
  names(histmev) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")
  out.mev = cbind(min=minmev, max=maxmev, avg=avgmev, sd=sdmev, var=varmev, t(histmev))
 
  #MCN
  v_mcn <- tmp$MCN[upper.tri(tmp$MCN, diag = FALSE)]
  v_mcn <- v_mcn[!is.na(v_mcn)]

  minmcn <- ifelse(length(v_mcn)==0,NA,base::min(v_mcn,na.rm=TRUE))
  maxmcn <- ifelse(length(v_mcn)==0,NA,base::max(v_mcn,na.rm=TRUE))
  avgmcn <- ifelse(length(v_mcn)==0,NA,mean(v_mcn,na.rm=TRUE))
  sdmcn <- ifelse(length(v_mcn)==0,NA,stats::sd(v_mcn,na.rm=TRUE))
  varmcn <- ifelse(length(v_mcn)==0,NA,stats::var(v_mcn,na.rm=TRUE))
  histmcn <- c()
  if(length(v_mcn)>1 && !all(v_mcn == v_mcn[1])) {
    histmcn <- as.vector(graphics::hist(v_mcn,breaks=seq(from=base::min(v_mcn), to=base::max(v_mcn), by=(base::max(v_mcn)-base::min(v_mcn))/10), plot = FALSE)$counts)
  } else { histmcn <- c(rep(NA,10))}
  names(histmcn) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")
  out.mcn = cbind(min=minmcn, max=maxmcn, avg=avgmcn, sd=sdmcn, var=varmcn, t(histmcn))

  #TIC
  v_tic <- tmp$TIC[upper.tri(tmp$TIC, diag = FALSE)]
  v_tic <- v_tic[!is.na(v_tic)]

  mintic <- ifelse(length(v_tic)==0,NA,base::min(v_tic,na.rm=TRUE))
  maxtic <- ifelse(length(v_tic)==0,NA,base::max(v_tic,na.rm=TRUE))
  avgtic <- ifelse(length(v_tic)==0,NA,mean(v_tic,na.rm=TRUE))
  sdtic <- ifelse(length(v_tic)==0,NA,stats::sd(v_tic,na.rm=TRUE))
  vartic <- ifelse(length(v_tic)==0,NA,stats::var(v_tic,na.rm=TRUE))

  histtic <- c()
  if(length(v_tic)>1 && !all(v_tic == v_tic[1])) {
    histtic <- as.vector(graphics::hist(v_tic,breaks=seq(from=base::min(v_tic), to=base::max(v_tic), by=(base::max(v_tic)-base::min(v_tic))/10), plot = FALSE)$counts)
  } else { histtic <- c(rep(NA,10))}

  names(histtic) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  out.tic = cbind(min=mintic, max=maxtic, avg=avgtic, sd=sdtic, var=vartic, t(histtic))

  return(list('MIC' = out.mic, 'MAS' = out.mas, 'MEV' = out.mev, 'MCN' = out.mcn, 'TIC' = out.tic))
}

########################################################
#'
#' 8. Maximal Information Coefficient (MIC) between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Maximal Information Coefficient (MIC) between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MIC score,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsMIC_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)
  
  v_mic <- c()

  if(length(numattrs)>1) {

    for(i in 1:(length(numattrs)-1)) {

      for(j in 2:length(numattrs)) {

        v_mic <- c(v_mic,minerva::mine_stat(ds[,numattrs[i]],ds[,numattrs[j]],measure="mic"))

      }

    }

  }

  v_mic <- v_mic[!is.na(v_mic)]

  minmic <- ifelse(length(v_mic)==0,NA,base::min(v_mic,na.rm=TRUE))
  maxmic <- ifelse(length(v_mic)==0,NA,base::max(v_mic,na.rm=TRUE))
  avgmic <- ifelse(length(v_mic)==0,NA,mean(v_mic,na.rm=TRUE))
  sdmic <- ifelse(length(v_mic)==0,NA,stats::sd(v_mic,na.rm=TRUE))
  varmic <- ifelse(length(v_mic)==0,NA,stats::var(v_mic,na.rm=TRUE))

  histmic <- c()
  if(length(v_mic)>1 && !all(v_mic == v_mic[1])) {
    histmic <- as.vector(graphics::hist(v_mic,breaks=seq(from=base::min(v_mic), to=base::max(v_mic), by=(base::max(v_mic)-base::min(v_mic))/10), plot = FALSE)$counts)
  } else { histmic <- c(rep(NA,10))}

  names(histmic) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minmic, max=maxmic, avg=avgmic, sd=sdmic, var=varmic, t(histmic))

}

########################################################
#'
#' 9. Maximum Asymmetry Score (MAS) between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Maximum Asymmetry Score (MAS) between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MAS score,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsMAS_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_mas <- c()

  if(length(numattrs)>1) {

    for(i in 1:(length(numattrs)-1)) {

      for(j in 2:length(numattrs)) {

        v_mas <- c(v_mas,minerva::mine_stat(ds[,numattrs[i]],ds[,numattrs[j]],measure="mas"))

      }

    }

  }

  v_mas <- v_mas[!is.na(v_mas)]

  minmas <- ifelse(length(v_mas)==0,NA,base::min(v_mas,na.rm=TRUE))
  maxmas <- ifelse(length(v_mas)==0,NA,base::max(v_mas,na.rm=TRUE))
  avgmas <- ifelse(length(v_mas)==0,NA,mean(v_mas,na.rm=TRUE))
  sdmas <- ifelse(length(v_mas)==0,NA,stats::sd(v_mas,na.rm=TRUE))
  varmas <- ifelse(length(v_mas)==0,NA,stats::var(v_mas,na.rm=TRUE))

  histmas <- c()
  if(length(v_mas)>1 && !all(v_mas == v_mas[1])) {
    histmas <- as.vector(graphics::hist(v_mas,breaks=seq(from=base::min(v_mas), to=base::max(v_mas), by=(base::max(v_mas)-base::min(v_mas))/10), plot = FALSE)$counts)
  } else { histmas <- c(rep(NA,10))}

  names(histmas) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minmas, max=maxmas, avg=avgmas, sd=sdmas, var=varmas, t(histmas))

}

########################################################
#'
#' 10. Maximum Edge Value (MEV) between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Maximum Edge Value (MEV) between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MEV score,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsMEV_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_mev <- c()

  if(length(numattrs)>1) {

    for(i in 1:(length(numattrs)-1)) {

      for(j in 2:length(numattrs)) {

        v_mev <- c(v_mev,minerva::mine_stat(ds[,numattrs[i]],ds[,numattrs[j]],measure="mev"))

      }

    }

  }

  v_mev <- v_mev[!is.na(v_mev)]

  minmev <- ifelse(length(v_mev)==0,NA,base::min(v_mev,na.rm=TRUE))
  maxmev <- ifelse(length(v_mev)==0,NA,base::max(v_mev,na.rm=TRUE))
  avgmev <- ifelse(length(v_mev)==0,NA,mean(v_mev,na.rm=TRUE))
  sdmev <- ifelse(length(v_mev)==0,NA,stats::sd(v_mev,na.rm=TRUE))
  varmev <- ifelse(length(v_mev)==0,NA,stats::var(v_mev,na.rm=TRUE))

  histmev <- c()
  if(length(v_mev)>1 && !all(v_mev == v_mev[1])) {
    histmev <- as.vector(graphics::hist(v_mev,breaks=seq(from=base::min(v_mev), to=base::max(v_mev), by=(base::max(v_mev)-base::min(v_mev))/10), plot = FALSE)$counts)
  } else { histmev <- c(rep(NA,10))}

  names(histmev) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minmev, max=maxmev, avg=avgmev, sd=sdmev, var=varmev, t(histmev))

}

########################################################
#'
#' 11. Minimum Cell Number (MCN) between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Minimum Cell Number (MCN) between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MCN score,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsMCN_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_mcn <- c()

  if(length(numattrs)>1) {

    for(i in 1:(length(numattrs)-1)) {

      for(j in 2:length(numattrs)) {

        v_mcn <- c(v_mcn,minerva::mine_stat(ds[,numattrs[i]],ds[,numattrs[j]],measure="mcn"))

      }

    }

  }

  v_mcn <- v_mcn[!is.na(v_mcn)]

  minmcn <- ifelse(length(v_mcn)==0,NA,base::min(v_mcn,na.rm=TRUE))
  maxmcn <- ifelse(length(v_mcn)==0,NA,base::max(v_mcn,na.rm=TRUE))
  avgmcn <- ifelse(length(v_mcn)==0,NA,mean(v_mcn,na.rm=TRUE))
  sdmcn <- ifelse(length(v_mcn)==0,NA,stats::sd(v_mcn,na.rm=TRUE))
  varmcn <- ifelse(length(v_mcn)==0,NA,stats::var(v_mcn,na.rm=TRUE))

  histmcn <- c()
  if(length(v_mcn)>1 && !all(v_mcn == v_mcn[1])) {
    histmcn <- as.vector(graphics::hist(v_mcn,breaks=seq(from=base::min(v_mcn), to=base::max(v_mcn), by=(base::max(v_mcn)-base::min(v_mcn))/10), plot = FALSE)$counts)
  } else { histmcn <- c(rep(NA,10))}

  names(histmcn) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minmcn, max=maxmcn, avg=avgmcn, sd=sdmcn, var=varmcn, t(histmcn))

}

########################################################
#'
#' 12. Total Information Coefficient (TIC) between Numeric Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Total Information Coefficient (TIC) between Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' TIC score,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsTIC_NumAttrs <- function(ds,tgt) {

  numattrs <- as.numeric(which(sapply(ds,is.numeric)))
  numattrs <- setdiff(numattrs,tgt)

  v_tic <- c()

  if(length(numattrs)>1) {

    for(i in 1:(length(numattrs)-1)) {

      for(j in 2:length(numattrs)) {

        v_tic <- c(v_tic,minerva::mine_stat(ds[,numattrs[i]],ds[,numattrs[j]],measure="tic"))

      }

    }

  }

  v_tic <- v_tic[!is.na(v_tic)]

  mintic <- ifelse(length(v_tic)==0,NA,base::min(v_tic,na.rm=TRUE))
  maxtic <- ifelse(length(v_tic)==0,NA,base::max(v_tic,na.rm=TRUE))
  avgtic <- ifelse(length(v_tic)==0,NA,mean(v_tic,na.rm=TRUE))
  sdtic <- ifelse(length(v_tic)==0,NA,stats::sd(v_tic,na.rm=TRUE))
  vartic <- ifelse(length(v_tic)==0,NA,stats::var(v_tic,na.rm=TRUE))

  histtic <- c()
  if(length(v_tic)>1 && !all(v_tic == v_tic[1])) {
    histtic <- as.vector(graphics::hist(v_tic,breaks=seq(from=base::min(v_tic), to=base::max(v_tic), by=(base::max(v_tic)-base::min(v_tic))/10), plot = FALSE)$counts)
  } else { histtic <- c(rep(NA,10))}

  names(histtic) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=mintic, max=maxtic, avg=avgtic, sd=sdtic, var=vartic, t(histtic))

}

########################################################
#'
#' 13. Entropy (Empirical) of Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Entropy of Numeric Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of attributes' entropy values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsEnt <- function(ds,tgt) {

  attrs <- 1:ncol(ds)
  attrs <- setdiff(attrs,tgt)

  v_ent <- c()

  if(length(attrs)>0) {

    for(i in 1:length(attrs)) {
      if(is.numeric(ds[,attrs[i]])) {
        v_ent <- c(v_ent, suppressWarnings(entropy::entropy(entropy::freqs(ds[,attrs[i]]))))
      } else {
        v_ent <- c(v_ent, suppressWarnings(entropy::entropy(table(ds[,attrs[i]]))))
      }
    }

  }

  v_ent <- v_ent[!is.na(v_ent)]
  minent <- ifelse(length(v_ent)==0,NA,base::min(v_ent,na.rm=TRUE))
  maxent <- ifelse(length(v_ent)==0,NA,base::max(v_ent,na.rm=TRUE))
  avgent <- ifelse(length(v_ent)==0,NA,mean(v_ent,na.rm=TRUE))
  sdent <- ifelse(length(v_ent)==0,NA,stats::sd(v_ent,na.rm=TRUE))
  varent <- ifelse(length(v_ent)==0,NA,stats::var(v_ent,na.rm=TRUE))

  histent <- c()
  if(length(v_ent)>1 && !all(v_ent == v_ent[1])) {
    histent <- as.vector(graphics::hist(v_ent,breaks=seq(from=base::min(v_ent), to=base::max(v_ent), by=(base::max(v_ent)-base::min(v_ent))/10), plot = FALSE)$counts)
  } else { histent <- c(rep(NA,10))}

  names(histent) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minent, max=maxent, avg=avgent, sd=sdent, var=varent, t(histent))

}

########################################################
#'
#' 14. Mutual Information of Attributes
#' min, max, mean, sd, var, hist
#'
########################################################

#' Statistics on Mutual Information of Attributes
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector with the minimum, maximum, mean, standard deviation and variance of attributes' mutual information values,
#' and a vector with the cardinality of values after binning (10 bins)
#'
statsMuI <- function(ds,tgt) {

  attrs <- 1:ncol(ds)
  attrs <- setdiff(attrs,tgt)

  #create a discretize version of ds
  tmpM <- infotheo::discretize(ds[,attrs],nbins=10)
  tmp.res <-  infotheo::mutinformation(tmpM)
  
  v_mui <- tmp.res[upper.tri(tmp.res, diag = FALSE)]

  #if(length(attrs)>0) {
#
  #  for(i in 1:(length(attrs)-1)) {
#
 #     for(j in 2:length(attrs)) {
#
 #       v_mui <- c(v_mui,infotheo::mutinformation(infotheo::discretize(ds[,i:j],nbins=10),infotheo::discretize(ds[,j],nbins=10)))
#
 #     }
#
 #   }
#
 # }

  v_mui <- v_mui[!is.na(v_mui)]
  minmui <- ifelse(length(v_mui)==0,NA,base::min(v_mui,na.rm=TRUE))
  maxmui <- ifelse(length(v_mui)==0,NA,base::max(v_mui,na.rm=TRUE))
  avgmui <- ifelse(length(v_mui)==0,NA,mean(v_mui,na.rm=TRUE))
  sdmui <- ifelse(length(v_mui)==0,NA,stats::sd(v_mui,na.rm=TRUE))
  varmui <- ifelse(length(v_mui)==0,NA,stats::var(v_mui,na.rm=TRUE))

  histmui <- c()
  if(length(v_mui)>1 && !all(v_mui == v_mui[1])) {
    histmui <- as.vector(graphics::hist(v_mui,breaks=seq(from=base::min(v_mui), to=base::max(v_mui), by=(base::max(v_mui)-base::min(v_mui))/10), plot = FALSE)$counts)
  } else { histmui <- c(rep(NA,10))}

  names(histmui) <- c("bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10")

  cbind(min=minmui, max=maxmui, avg=avgmui, sd=sdmui, var=varmui, t(histmui))

}

########################################################
#'
#' 15. Measures of Overlapping
#' F1, F2 and F3
#'
########################################################

#' Measures of Overlapping
#' Includes Maximum Fishers' Discriminant Ratio (F1), the Volume of the Overlapping Region (F2), and the Maximum Individual Feature Efficiency (F3)
#'
#' @param ds A data set
#' @param tgt The index of the target variable
#'
#' @return A vector of numeric values concerning Maximum Fisher's Discriminant Ratio (F1), the Volume of the Overlapping Region (F2), and the Maximum Individual Feature Efficiency (F3)
#'
measOverlap <- function(ds,tgt) {

  t(ECoL::overlapping(ds[,-tgt], ds[,tgt], measures=c("F1","F2","F3"), summary="mean"))

}

########################################################
#'
#' 16. Percentual Difference by Class (PDC)
#' F1, F2 and F3
#'
########################################################

#' Class-Based Percentual Differences
#' Applies all of the stats meta functions and calculates the percentual difference between values calculated based on the majority class and the minority class.
#' This is calculated as 100 * ( abs( f(X|Min) - f(X|Maj) ) / ( [ abs(f(X|Min)) + abs(f(X|Maj)) ] ) / 2 )
#'
#' @param ds A data set
#' @param form A model formula
#'
#' @return A vector of numerical values
#'
ClassPD <- function(ds, form) {

  nms <- classNames(form = form, ds = ds)
  tgt <- which(colnames(ds)==as.character(form[[2]]))

  # IQR
  diffIQR <- percDiff(statsIQR_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsIQR_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Coefficient of Variation
  diffCoefVar <- percDiff(statsCoV_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsCoV_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Correlation between Numerical Attributes
  diffCorNumAttrs <- percDiff(statsCor_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsCor_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Geary's Kurtosis of Numerical Attributes
  diffGKurNumAttrs <- percDiff(statsGKur_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsGKur_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Pearson's Kurtosis of Numerical Attributes
  diffPKurNumAttrs <- percDiff(statsPKur_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsPKur_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Skewness of Numerical Attributes
  diffSkewNumAttrs <- percDiff(statsSkew_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsSkew_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Maximal Information Coefficient (MIC)
  diffMICNumAttrs <- percDiff(statsMIC_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsMIC_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Maximum Asymmetry Score (MAS)
  diffMASNumAttrs <- percDiff(statsMAS_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsMAS_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Maximum Edge Value (MEV)
  diffMEVNumAttrs <- percDiff(statsMEV_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsMEV_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Minimum Cell Number (MCN)
  diffMCNNumAttrs <- percDiff(statsMCN_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsMCN_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Total Information Coefficient (TIC)
  diffTICNumAttrs <- percDiff(statsTIC_NumAttrs(ds[ds[,tgt]==nms[2],],tgt), statsTIC_NumAttrs(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Entropy
  diffEntropy <- percDiff(statsEnt(ds[ds[,tgt]==nms[2],],tgt), statsEnt(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  # Mutual Information of Attributes
  diffMIN <- percDiff(statsMuI(ds[ds[,tgt]==nms[2],],tgt), statsMuI(ds[ds[,tgt]==nms[1],],tgt))[,1:5]

  data.frame(diffIQR=t(diffIQR),diffCoefVar=t(diffCoefVar),diffCorNumAttrs=t(diffCorNumAttrs),diffGKurNumAttrs=t(diffGKurNumAttrs),
             diffPKurNumAttrs=t(diffPKurNumAttrs),diffSkewNumAttrs=t(diffSkewNumAttrs),diffMICNumAttrs=t(diffMICNumAttrs),
             diffMASNumAttrs=t(diffMASNumAttrs),diffMEVNumAttrs=t(diffMEVNumAttrs),diffMCNNumAttrs=t(diffMCNNumAttrs),
             diffTICNumAttrs=t(diffTICNumAttrs),diffEntropy=t(diffEntropy), diffMIN=t(diffMIN))

}


########################################################
#'
#' 17. Landmarkers
#' Trees, NB
#'
########################################################

#' Decision-Tree Landmarkers
#'
#' @param ds A data set
#' @param form A model formula
#' @param maxdepth The maximum depth for the tree
#'
#' @return A vector of numerical values
#'
landmarker.tree <- function(ds, form, maxdepth=1) {

  res <- kf_xval(ds, form, 10, wf.Tree, maxdepth=maxdepth)
  res[is.na(res)] <- 0
  res <- colMeans(res)
  as.data.frame(t(res))

}

#' Naive Bayes Landmarkers
#'
#' @param ds A data set
#' @param form A model formula
#'
#' @return A vector of numerical values
#'
landmarker.nb <- function(ds, form) {

  res <- kf_xval(ds, form, 10, wf.NaiveBayes)
  res[is.na(res)] <- 0
  res <- colMeans(res)
  as.data.frame(t(res))

}


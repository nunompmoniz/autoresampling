
### sysdata.rda
#' - workflow configurations for ATOMIC
#' - workflow configurations for ATOMIR
#' - workflow rankings for ATOMIC
#' - workflow ranking for ATOMIR
#' - meta model for imbalanced classification tasks
#' - meta model for imbalanced regression tasks

# - workflow configurations for ATOMIC

#' Workflow configurations for ATOMIC
#'
#' This script creates a data.frame containing all of the workflow configurations available for testing within the ATOMIC method
#'

library(tidyverse)
library(foreach)

wf.config.class <- c()

i <- r <- NA

gs.RF <- as.data.frame(list(ntrees=c(100,250,500)))
gs.ENN <- as.data.frame(list(Strategy="rs.ENN",k=c(1,3,5)) %>% purrr::cross_df())
gs.RU <- as.data.frame(list(Strategy="rs.RandUnder",und.perc=seq(0.1,0.9,by=0.1)) %>% purrr::cross_df())
gs.RO <- as.data.frame(list(Strategy="rs.RandOver",ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% purrr::cross_df())
gs.IS <- as.data.frame(list(Strategy="rs.ImpSamp",und.perc=seq(0.1,1,by=0.1),ove.perc=c(seq(0,1,by=0.25),2,3,4)) %>% purrr::cross_df()); gs.IS <- gs.IS[-10,]
gs.SM <- as.data.frame(list(Strategy="rs.SMOTE",und.perc=seq(0.1,1,by=0.1),ove.perc=c(seq(0,1,by=0.25),2,3,4)) %>% purrr::cross_df()); gs.SM <- gs.SM[-10,]

str.vec <- c("","rs.ENN","rs.RandUnder","rs.RandOver","rs.ImpSamp","rs.SMOTE","rs.TomekUnder")

gridlist <- list(gs.ENN,gs.RU,gs.RO,gs.IS,gs.SM)

# WORKFLOWS
for(xi in 1:length(gridlist)) {

  x <- gridlist[[xi]]

  v <- 1

  foreach::foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {

    foreach::foreach(i=1:nrow(x), .combine = rbind) %do% {

      parms.row <- data.frame(Model=paste0(x[i,1],paste0(".v",v)), ntrees=gs.RF[r,], RStrategy=x[i,1], k=-1, und.perc=-1, ove.perc=-1)

      parm.names <- colnames(x)[-1]
      if(length(parm.names)>0) {
        for(p in 1:length(parm.names)) {
          parms.row[,which(colnames(parms.row)==parm.names[p])] <- x[i,parm.names[p]]
        }
      }

      wf.config.class <- rbind(wf.config.class,parms.row[,-1])

      v<-v+1

    }

  }

}


# WORKFLOWS CONCERNING THE APPLICATION OF THE UNDERSAMPLING STRATEGY BASED ON TOMEK LINKS
res.tl <- foreach::foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {

  parms.row <- data.frame(Model=paste0("rs.TomekUnder",paste0(".v",r)), ntrees=gs.RF[r,], RStrategy="rs.TomekUnder", k=-1, und.perc=-1, ove.perc=-1)

  wf.config.class <- rbind(wf.config.class,parms.row[,-1])

}

# WORKFLOWS WITHOUT APPLICATION OF RESAMPLING STRATEGIES
res.none <- foreach::foreach(r=1:nrow(gs.RF), .combine = rbind) %do% {

  parms.row <- data.frame(Model=paste0("RF",paste0(".v",r)), ntrees=gs.RF[r,], RStrategy="None", k=-1, und.perc=-1, ove.perc=-1)

  wf.config.class <- rbind(wf.config.class,parms.row[,-1])

}

# RUN NAIVE SOLUTIONS
res.naive <- foreach::foreach(r=1:length(str.vec), .combine = rbind) %do% {

  parms.row <- data.frame(Model=paste0(ifelse(str.vec[r]=="","RF",str.vec[r]),".Naive"), ntrees=-1, RStrategy=ifelse(str.vec[r]=="","None",str.vec[r]), k=-1, und.perc=-1, ove.perc=-1)

  wf.config.class <- rbind(wf.config.class,parms.row[,-1])
}

######################################################
# - workflow configurations for ATOMIC

#' Workflow configurations for ATOMIR
#'
#' This script creates a data.frame containing all of the workflow configurations available for testing within the ATOMIR method
#'

# NOT AVAILABLE YET (wf.config.reg)
wf.config.reg <- NULL

######################################################
# - workflow ranking for ATOMIC

#' Workflow rankings for ATOMIC
#'
#' This script creates a data.frame containing the rankings of workflow configurations available for testing within the ATOMIC method
#'

# (agg.class)
agg.class <- readRDS("~/Desktop/Academia/Manuscripts/2018 - AutoResampling Classification (PR)/AutoResampling/Results/aggclass.rds")

######################################################
# - workflow ranking for ATOMIR

#' Workflow rankings for ATOMIR
#'
#' This script creates a data.frame containing the rankings of workflow configurations available for testing within the ATOMIR method
#'

# NOT AVAILABLE YET (agg.reg)
agg.reg <- NULL


######################################################
# - meta model for imbalanced classification tasks

#' Meta-model for ATOMIC
#'
#' This script creates the xgboost model for ATOMIC
#'

# External. Consult Github for the respective paper.
# metamodel.class
metamodel.class <- xgboost::xgb.load("metamodelATOMIC.model")

######################################################
# - meta model for imbalanced regression tasks

#' Meta-model for ATOMIR
#'
#' This script creates the xgboost model for ATOMIR
#'

# External. Consult Github for the respective paper.
# NOT AVAILABLE YET (metamodel.reg)
metamodel.reg <- NULL

######################################################

sysdata <- list(wf.config.class=wf.config.class,
                wf.config.reg=wf.config.reg,
                agg.class=agg.class,
                agg.reg=agg.reg,
                metamodel.class=metamodel.class,
                metamodel.reg=metamodel.reg)

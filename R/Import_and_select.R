## Functions to import PSP data into R and select plots

## These functions all work pretty well

## Import sample data into R
#' Title Import PSP Sample Data
#'
#'This function imports PSP sample data
#'
#' @param data_path Path to PSP sample data
#' @param data_type
#' @param tsas TSA(s) to be imported
#'
#' @return
#' @export
#'
#' @examples
importPSP <- function(data_path, data_type, tsas){
  read.list <- list()
  dat.list <- list()
  for(i in 1:length(data_type)){
    #setwd(paste0(r.path,dat.type[i]))
    for(j in 1:length(tsas)){
      read.list[[j]]<- fread(paste0(data_path,"/", data_type[i],"/","TSA",tsas[j],".csv"))
    }
    dat.list[[i]] <- rbindlist(read.list)
  }
  return(dat.list)
}





## Select sample plots to use in model based on criteria (site series, remeasurements, treatment)
#' Title Select and Clean PSP Sample Data
#'
#' This function selects PSP sample plots based on criteria and cleans data
#'
#' @param samples.dt Imported sample data output by importPSP function
#' @param tree.dat
#' @param BECzone BEC zone(s) to be selected
#' @param BEClabel BEC label(s) to be selected
#' @param SiteSeriesOfInterest Site series of interest to be selected
#' @param MinRemeasInterval Minimum remeasurement interval for selected plots
#'
#' @return
#' @export
#'
#' @examples
sel.psp <- function(samples.dt,tree.dat,BECzone,BEClabel,SiteSeriesOfInterest,MinRemeasInterval){
  # Remove repeats (which I think represent sub-plots)
  uni.samples.dt<-unique(samples.dt, by="SAMP_ID")
  #create the list of criteria needed to determine whether a plot should be included. This assumes that coding is consistent
  if(!is.null(BECzone)){criteria.samples <- uni.samples.dt[[1]][bgc_zone == BECzone & bgc_ss_grd>0] #02
  } else{criteria.samples <- uni.samples.dt[[1]][beclabel_grd == BEClabel & bgc_ss_grd>0]} #05/06
  remeas.samples <- criteria.samples[(criteria.samples[,meas_yr_first]!=criteria.samples[,meas_yr_last])]
  remeas.samples <- remeas.samples[tot_period>=MinRemeasInterval & treatment != "THINNED" & stnd_org!="P"]
  plot.SORTIE <- unique(remeas.samples[bgc_ss_grd==SiteSeriesOfInterest]$SAMP_ID)

  #remove plots based on composition: actually just need to remove from plotID
  # c("XC","CW")
  #plot.SORTIE <- tree.dat[samp_id %in% ]
  #setkey(tree.dat)
  #remove plots based on composition: actually just need to remove from plotID
  #rm.plot <- unique(tree.dat[which(tree.dat[,species==.("XC","CW")]),.(samp_id)])
  #tree.dat <- tree.dat[!rm.plot]

  return(plot.SORTIE)
}







## Import trees data for selected sample plots
#' Title Import PSP Tree Data
#'
#' This function imports PSP tree data corresponding to selected sample plots
#'
#' @param tree_data_path Path to tree data
#' @param tsas TSAs to select tree data for
#' @param selected_plots Plot names selected in sel.psp function
#'
#' @return
#' @export
#'
#' @examples
importTrees <- function(tree_data_path, tsas, selected_plots){
  read.list <- list()
  #  dat.list <- list()
  # for(i in 1:length(selected_plots)){
  for(j in 1:length(tsas)){
    read.list[[j]]<- fread(paste0(tree_data_path,"/","/","TSA",tsas[j],".csv"))
  }
  dat.list <- rbindlist(read.list)

  selected_plots = selected_plots %>%
    as.data.frame() %>%
    rename("samp_id" = ".")

  # not sure if this is right, don't know how big it should be
  dat.list = dat.list[dat.list$samp_id %in% selected_plots$samp_id]

  return(dat.list)
}



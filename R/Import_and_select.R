## Functions to import PSP data into R and select plots

## These functions all work pretty well

## Import sample data into R
#' Title Import PSP Sample Data
#'
#'This function imports PSP sample data
#'
#' @param data_path Path to PSP sample data
#' @param data_type is it trees or samples. default is trees
#' @param tsas TSA(s) to be imported (vector of characters). default is import all
#'
#' @details
#' Just pass the TSA number if want to overide importing all the data
#'
#' @return
#' @export
#'
#' @examples
import_psps <- function(data_path, data_type = "Trees", tsas = TRUE){
  if(tsas){
    tsa_r <- list.files(paste0(datpath,data_type), full.names = TRUE)
  }else{
    tsa_r <- paste0(datpath,data_type,"/TSA",tsas,".csv")
  }

  read_tsa <- lapply(tsa_r, data.table::fread)
  tsa_dt <- do.call(rbind, read_tsa)

  return(tsa_dt)
}





## Select sample plots to use in model based on criteria (site series, remeasurements, treatment)
#' Title Select and Clean PSP Sample Data
#'
#' This function selects PSP sample plots based on criteria and cleans data
#'
#' @param samples_data Imported sample data output by importPSP function
#' @param BECzone BEC zone(s) to be selected
#' @param BEClabel BEC label(s) to be selected
#' @param site_series Site series of interest to be selected
#' @param min_remeasure Minimum remeasurement interval for selected plots
#'
#' @return
#' @export
#'
#' @examples
select_psps <- function(samples_data, BECzone, BEClabel, site_series,
                    min_remeasure){
  # Remove repeats (which I think represent sub-plots)
  uni.samples.dt<-unique(samples_data, by="SAMP_ID")
  # create the list of criteria needed to determine whether a plot should be included.
  # This assumes that coding is consistent
  if(!is.null(BECzone)){
    criteria.samples <- uni.samples.dt[[1]][bgc_zone == BECzone & bgc_ss_grd>0] #02

  } else {
    criteria.samples <- uni.samples.dt[[1]][beclabel_grd == BEClabel & bgc_ss_grd>0] #05/06
  }

  remeas.samples <- criteria.samples[(criteria.samples[,meas_yr_first]!=
                                        criteria.samples[,meas_yr_last])]
  remeas.samples <- remeas.samples[tot_period >= min_remeasure & treatment !=
                                     "THINNED" & stnd_org!="P"]
  plot.SORTIE <- unique(remeas.samples[bgc_ss_grd == site_series]$SAMP_ID)

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
#' @param data_path Path to tree data
#' @param tsas TSAs to select tree data for
#' @param selected_plots Plot names selected in sel.psp function
#'
#' @return
#' @export
#'
#' @examples
import_trees <- function(data_path, tsas, selected_plots){
  read.list <- list()
  #  dat.list <- list()
  # for(i in 1:length(selected_plots)){
  for(j in 1:length(tsas)){
    read.list[[j]]<- fread(paste0(data_path,"/","/","TSA",tsas[j],".csv"))
  }
  dat.list <- rbindlist(read.list)

  selected_plots = selected_plots %>%
    as.data.frame() %>%
    rename("samp_id" = ".")

  # not sure if this is right, don't know how big it should be
  dat.list = dat.list[dat.list$samp_id %in% selected_plots$samp_id]

  return(dat.list)
}




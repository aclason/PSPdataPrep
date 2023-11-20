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
    tsa_r <- paste0(datpath, data_type, "/TSA", tsas,".csv")
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
#' @param BECzone BEC zone(s) to be selected. Required. can be a vector of multiple zones
#' @param BEClabel BEC label(s) to be selected. Required. can be a vector of multiple subzones
#' @param site_series Site series of interest to be selected
#' @param min_remeasure Minimum remeasurement interval for selected plots
#' @param treatments Options = "THINNED" or "none" for untreated (default)
#' @param stand_origin Options = "P" for plantations or "untreated" for untreated (default)
#' @returns description
#' @export
#'
#' @details
#' bgc_ss_grd is the site series call - right now, this can't be NA (only true for edaphic paper)
#' beclabel	Concatanation of (BGC_zone + BGC_sbzn + BGC_var), based on provincial BEC coverage if ground sample coordinates available, otherwise ground sample based
#' beclabel_grd	concatanation of (BGC_zone_grd + BGC_sbzn_grd + BGC_var_grd), ground sample based classification
#'
#' @examples
select_psps <- function(samples_data, BECzone, BECsubzone, site_series,
                    min_remeasure, treatments = "none",
                    stand_origin = "untreated"){

  # Remove repeats (which I think represent sub-plots)
  uni.samples.dt <- unique(samples_data, by="SAMP_ID")

  # bec zone
  bl <- uni.samples.dt[like(beclabel, BECzone, ignore.case = TRUE)]
  blg <- uni.samples.dt[like(beclabel_grd, BECzone, ignore.case = TRUE)]
  bl_blg <- unique(rbind(bl,blg))

  # becsubzone
  combined_condition <- paste(BECsubzone,collapse = "|")
  bl_s <- bl_blg[like(beclabel, combined_condition, ignore.case = TRUE)]
  blg_s <- bl_blg[like(beclabel_grd, combined_condition, ignore.case = TRUE)]
  bl_blg_s <- unique(rbind(bl_s,blg_s))

  #end up with some random calls, so clean again
  combined_condition <- paste0(BECzone, BECsubzone, collapse = "|")
  zs <- bl_blg_s[like(beclabel, combined_condition, ignore.case = TRUE)]


  #if site series passed
  if(!is.null(site_series)){
    combined_condition <- paste("bgc_ss_grd","==",site_series, collapse = "|")
    zs_ss <-  zs[eval(parse(text = combined_condition))]

    #has there been more than one measurement?
    remeas.samples <- zs_ss[(zs_ss[,meas_yr_first]!= zs_ss[,meas_yr_last])]
  }else{
    remeas.samples <- zs[(zs[,meas_yr_first]!= zs[,meas_yr_last])]
  }

  #has it been measured for longer than the minimum amount?
  rs_min <- remeas.samples[tot_period >= min_remeasure]

  if(stand_origin == "untreated"){
    rs_min_so <- rs_min[stnd_org != "P"]
  }else{
    rs_min_so <- rs_min[stnd_org == stand_origin]
  }

  if(treatments == "none"){
    rs_min_so_t <- rs_min_so[treatment == "UNTREATED"]
  }else{
    rs_min_so_t <- rs_min_sp[treatment == "THINNED"]
  }

  selected_psp_plots <- unique(rs_min$SAMP_ID)

  #remove plots based on composition: actually just need to remove from plotID
  # c("XC","CW")
  #plot.SORTIE <- tree.dat[samp_id %in% ]
  #setkey(tree.dat)
  #remove plots based on composition: actually just need to remove from plotID
  #rm.plot <- unique(tree.dat[which(tree.dat[,species==.("XC","CW")]),.(samp_id)])
  #tree.dat <- tree.dat[!rm.plot]

  return(selected_psp_plots)
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






#
#' Import PSP data
#'
#' @param data_path pathway to data
#' @param data_type samples or trees?
#' @param tsas which tsas to include (vector of characters I think)
#'
#' @details Import tree and samples data
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
      read.list[[j]]<- fread(paste0(data_path,data_type[i],"/","TSA",tsas[j],".csv"))
    }
    dat.list[[i]] <- rbindlist(read.list)
  }
  return(dat.list)
}


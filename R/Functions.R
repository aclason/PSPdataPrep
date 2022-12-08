

#
#' Import PSP data
#'
#' @param r.path pathway to data
#' @param dat.type samples or trees?
#' @param tsas which tsas to include (vector of characters I think)
#'
#' @details Import tree and samples data
#' @return
#' @export
#'
#' @examples
import.psp <- function(r.path, dat.type, tsas){
  read.list <- list()
  dat.list <- list()
  for(i in 1:length(dat.type)){
    #setwd(paste0(r.path,dat.type[i]))
    for(j in 1:length(tsas)){
      read.list[[j]]<- fread(paste0(r.path,dat.type[i],"/","TSA",tsas[j],".csv"))
    }
    dat.list[[i]] <- rbindlist(read.list)
  }
  return(dat.list)
}


## Functions to prepare and process PSP data in R

## These functions need some more clean up and testing

## Prepare tree data for import into SORTIE.tree.create
#' Title Prepare Tree Data for Import
#'
#' This function prepares selected tree data for import into SORTIE.tree.create
#'
#' @param tree.dat Tree data imported for selected sample plots
#' @param study.plots Sample plots selected in sel.psp function
#'
#' @return
#' @export
#'
#' @examples
psp.meas <- function(tree.dat,study.plots){
  live.Plot.ReMeas.list <- list()
  for(k in 1:length(study.plots)){
    num.meas <- length(unique(tree.dat[samp_id==study.plots[k],meas_no]))
    Plot.ReMeas.list <- list()
    for(i in 1:num.meas){
      study.plots.meas <- tree.dat[samp_id==study.plots[k] & meas_no==(i-1)]
      study.plots.meas[,LD_Group:=ifelse(ld=="L",1,ifelse(ld=="I",1,ifelse(ld=="V",1,2)))]
      #in below line, changes sp_PSP to species
      red.study.plots.meas <- study.plots.meas[,.(samp_id,tree_no,meas_yr,meas_no,phf_tree,species,dbh,ld,
                                                  LD_Group,age_tot,height,batree,baha,volwsv,volcu10m,
                                                  volcu15m,wsvha,gmv10ha,gmv15ha,nmv10ha,nmv15ha)]
      #just main plot trees
      main.plot.phf <- min(na.omit(tree.dat[samp_id==study.plots[k] & meas_no==(i-1)]$phf_tree))
      Plot.ReMeas.list[[i]] <- red.study.plots.meas[round(phf_tree) == round(main.plot.phf)]
    }
    Plot.ReMeas.tab <- rbindlist(Plot.ReMeas.list)
    #return(Plot.ReMeas.tab)
    live.Plot.ReMeas <- Plot.ReMeas.tab[LD_Group==1]
    for(j in 1:nrow(live.Plot.ReMeas)){
      if(live.Plot.ReMeas[j,species] == "SX"){
        live.Plot.ReMeas[j, Type :=  ifelse(live.Plot.ReMeas[j,height >=1.35 & dbh < 3.0],"Sapling",
                                            ifelse(live.Plot.ReMeas[j,height >=1.35 & dbh >= 3.0],"Adult",
                                                   "Seedling"))]
      } else {
        live.Plot.ReMeas[j, Type :=  ifelse(live.Plot.ReMeas[j,height >=1.35 & dbh < 5.0],"Sapling",
                                            ifelse(live.Plot.ReMeas[j,height >=1.35 & dbh >= 5.0],"Adult",
                                                   "Seedling"))]
      }
    }
    live.Plot.ReMeas.list[[k]] <- live.Plot.ReMeas
  }
  #make a single data table for PSP plot remeasurements:
  cleaned.psp.remeas <- rbindlist(live.Plot.ReMeas.list)
  #return(cleaned.psp.remeas)
}




## Create SORTIE starting conditions for each plot

#' Title Create initial tree population table for SORTIE model
#'
#' @param sizeClasses Vector of desired size classes
#' @param SORTIE.species
#' @param PSP.measurements
#'
#' @return
#' @export
#'
#' @examples
SORTIE.tree.create <- function(sizeClasses,SORTIE.species, PSP.measurements){

  #extract min adult DBH
  Min.Adult.DBH = PSP.measurements %>%
    filter(., Type == "Adult") %>%
    group_by(., species, min(dbh)) %>%
    summarize(.)
  Min.Adult.DBH = as.vector(Min.Adult.DBH$`min(dbh)`)

  # extract max seedling height
  #assuming with this seedling = sapling??
  Max.Seedling.Hgt.m = PSP.measurements %>%
    filter(., Type == "Sapling") %>%
    group_by(., species, max(height)) %>%
    summarize(.)
  Max.Seedling.Hgt.m = as.vector(Max.Seedling.Hgt.m$`max(height)`)

  Init.Dens.Seedling.Hgt.Class.1 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling.Hgt.Class.2 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling.Hgt.Class.3 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling <- rep(0, length(SORTIE.species))

  inits <- vector()
  init.values <- matrix(nrow=length(sizeClasses),ncol=length(SORTIE.species))
  for(i in 1:length(sizeClasses)){
    inits[i] <- paste0("Init.Dens.",sizeClasses[i])
  }
  row.names(init.values) <- inits
  for(j in 1:length(SORTIE.species)){
    init.values[,j] <- rep(0, length(inits))
  }

  # Now create the data.table of parameter values that vary
  SORTIE.tree.dat <- data.table()
  SORTIE.tree.dat <- rbind(Min.Adult.DBH,Max.Seedling.Hgt.m,Init.Dens.Seedling.Hgt.Class.1,
                           Init.Dens.Seedling.Hgt.Class.2,Init.Dens.Seedling.Hgt.Class.3,Init.Dens.Seedling,init.values)
  colnames(SORTIE.tree.dat)<-SORTIE.species
  return(SORTIE.tree.dat)
}




## Export prepared csvs
#' Title Export initial SORTIE plot
#'
#' This function exports initial SORTIE starting conditions as a CSV
#'
#' @param SORTIE.plot Initial SORTIE starting conditions plot exported from SORTIE.tree.create
#'
#' @return
#' @export
#'
#' @examples
write.psp <- function(SORTIE.plot){

  write.csv(SORTIE.plot, "psp_output_for_SORTIE.csv")

}



## Determine number of years for SORTIE run based on data
#' Title Calculate number of years and age for PSP SORTIE run
#'
#' @param plot.SORTIE Selected plots to be included
#' @param tree.dat Tree data imported for selected plots
#' @param samples.dt Imported sample data
#' @param age.crit
#'
#' @return
#' @export
#'
#' @examples
psp.years.age <- function(plot.SORTIE,tree.dat,samples.dt,age.crit){
  #make the right output for print functions
  psp.dets <- list()
  sp_comp_list <-list()
  #num.meas <- vector()
  main.plot.phf <- vector()
  #run_years <- vector()
  for(i in 1:length(plot.SORTIE)){
    num.meas <- length(unique(tree.dat[samp_id==plot.SORTIE[i],meas_no]))
    for(j in 1:num.meas){
      main.plot.phf[j] <- min(na.omit(tree.dat[samp_id==plot.SORTIE[i] & meas_no==(j-1)]$phf_tree))
    }
    #plots.each.meas[[i]] <- main.plot.phf
    #main.plot.phf <- min(na.omit(tree.dat[samp_id==plot.SORTIE[i]]$phf_tree))
    sp_comp <- table(tree.dat[samp_id==plot.SORTIE[i],species])
    #unique(samples.dt[SAMP_ID==plot.SORTIE[i]]$beclabel_grd)
    age <- max(na.omit(tree.dat[samp_id==plot.SORTIE[i]&meas_no==0]$age_tot))
    run_years <- max(samples.dt[[1]][SAMP_ID==plot.SORTIE[i],meas_yr])-min(samples.dt[[1]][SAMP_ID==plot.SORTIE[i],meas_yr])
    psp.dets$plotid[i] <- plot.SORTIE[i]
    #psp.dets$main.plot.phf[i] <- plots.each.meas[[i]]
    psp.dets$num.meas[i] <- num.meas
    psp.dets$run.years[i] <- run_years
    psp.dets$age[i] <- age
    sp_comp_list[[i]] <- sp_comp
  }
  psp.dets[[6]] <- sp_comp_list
  #psp.dets[[7]] <- plots.each.meas
  # return(psp.dets)
  #return(num.meas)
}




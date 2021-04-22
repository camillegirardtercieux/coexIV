#' Get Genus
#'
#' @param Sp 
#'
#' @return a character string Gen for the name of the genus
Get_Gen <- function (Sp) {
  Sp <- strsplit(Sp, "_")
  Gen <- Sp [[1]][1]
  Gen
}

#' Get Species
#'
#' @param Sp 
#'
#' @return a character string Spe for the name of the species
Get_Spe <- function (Sp) {
  Sp <- strsplit(Sp, "_")
  Spe <- Sp [[1]][2]
  Spe
}

#' Transforms the name of the undertermined species into "Indet_sp"
#'
#' @param Binomial_name 
#'
#' @return a character string for the new name of the species
transform_Indet_species <- function (Binomial_name) {
  Genus <- Get_Gen(Binomial_name)
  Species <- Get_Spe(Binomial_name)
  if (grepl("Indet", Species, fixed = TRUE) == TRUE) {
    Genus <- "Indet"
    Species <- "sp"
  }
  paste0(Genus, "_", Species)
}

#' Read the original paracou data
#'
#' @return a dataframe containing Paracou's data with plots merged
read_data_paracou <- function(){
  Plots <- list()
  for (i in 1:15) {
    Plots[[i]] <- readr::read_csv(here::here("data", "Paracou", glue::glue("Paracou20200429-plot{i}.csv")))
  }
  Paracou <- dplyr::bind_rows(Plots)
  return(Paracou)
}

#' Compute the annual growth of each tree of the paracou dataset
#'
#' @param Plots 
#'
#' @return a dataframe containing the annual growth in mm/y
compute_growth_paracou <- function(Paracou){
  Paracou_no_mean <- Paracou %>%
    dplyr::mutate(D_tp1=Circ/pi, D_tp1_corr=CircCorr/pi) %>%
    dplyr::group_by(idTree) %>%
    dplyr::mutate(D_t = c(NA, D_tp1[1 : NROW(D_tp1)-1]),
                  D_t_corr = c(NA, D_tp1_corr[1 : NROW(D_tp1)-1]),
                  Date_tp1 = CensusDate,
                  Date_t = lubridate::as_date(c(NA, CensusDate[1 : NROW(CensusDate)-1]))) %>%
    dplyr::mutate(DiffD = D_tp1 - D_t,
                  DiffD_corr = D_tp1_corr - D_t_corr,
                  DiffDate = Date_tp1 - Date_t) %>%
    dplyr::mutate(G_tp1 = DiffD*10/(as.numeric(DiffDate/365)),
                  G_tp1_corr = DiffD_corr*10/(as.numeric(DiffDate/365))) %>%
    dplyr::mutate(Tree = paste0("P", Plot, "T", idTree)) %>%
    dplyr::mutate(Sp = paste0(Genus, "_",Species))%>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
  
  return(Paracou_no_mean)
}

#' Rename the indeterminate species in paracou
#'
#' @param Paracou_no_mean 
#'
#' @return the dataframe with a modified Sp column
rename_indet_paracou <- function(Paracou_no_mean){
  Paracou_no_mean <- Paracou_no_mean %>%
    dplyr::group_by(Sp)%>%
    dplyr::mutate(Sp_inter = transform_Indet_species(Sp))%>%
    dplyr::ungroup()%>%
    dplyr::select(-Sp)%>%
    dplyr::rename(Sp=Sp_inter)%>%
    
    return(Paracou_no_mean)
}

#' Add IdentSpecies column
#'
#' @param data 
#'
#' @return the dataframe with a new column
create_identspecies <- function(data){
  data <- data %>%
    dplyr::mutate(Sp = as.factor(Sp), Tree=as.factor(Tree), IdentSpecies=factor(Sp))
  levels(data$IdentSpecies) <-1:length(levels(data$IdentSpecies))
  
  return(data)
}

#' Compute the mean growth for paracou
#'
#' @param Data_no_mean 
#'
#' @return a dataframe with the mean growth of each tree
compute_mean_growth_paracou <- function(Data_no_mean){
  Data_mean_g <- Data_no_mean %>%
    dplyr::group_by(Tree) %>%
    dplyr::mutate(
      Mean_G_Tree = dplyr::case_when(
        dplyr::n()>2~(
          ((dplyr::last(na.omit(D_t_corr)) - dplyr::first(na.omit(D_t_corr)))*10)/
            (as.numeric(dplyr::last(na.omit(Date_t)) - dplyr::first(na.omit(Date_t)))/365)
        )
      )
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(Sp = as.factor(Sp), Tree=as.factor(Tree))
  return(Data_mean_g)
}

#' Growth computation for Uppangala
#'
#' @param data 
#'
#' @return data but with new colums
compute_growth_uppangala <- function(data){
  data <- data%>%
    dplyr::arrange(TreeID, CensusDate)%>%
    dplyr::group_by(TreeID) %>%
    dplyr::rename(D_tp1=dbh)%>%
    dplyr::mutate(Date_tp1 = lubridate::as_date(lubridate::ymd(paste0(CensusDate, "-01"))),
                  D_t = c(NA, D_tp1[1 : NROW(D_tp1)-1]), 
                  Date_t =  lubridate::as_date(c(NA, Date_tp1[1 : NROW(Date_tp1)-1]))) %>%
    dplyr::mutate(DiffD = dplyr::case_when(Date_tp1 != Date_t ~ D_tp1 - D_t) ,
                  DiffDate = Date_tp1 - Date_t) %>%
    dplyr::mutate(G_tp1 = DiffD*10/(as.numeric(DiffDate/365))) %>%
    dplyr::ungroup()
  return(data)
}

#' Compute mean growth for Uppangala
#'
#' @param data 
#'
#' @return data but with a new colums Mean_G_Tree
compute_mean_growth_uppangala <- function(data){
  data <- data %>%
    dplyr::group_by(TreeID) %>%
    dplyr::mutate(Mean_G_Tree = ((dplyr::last(na.omit(D_t)) - dplyr::first(na.omit(D_t)))*10)/(as.numeric((dplyr::last(na.omit(Date_t)) - dplyr::first(na.omit(Date_t))))/365)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(Sp = as.factor(SpCode), Tree=as.factor(TreeID))
  
  data <- data[which(is.na(data$Mean_G_Tree)==F),]
  
  return(data)
}


#' Load BCI data and gather all censuses in the same file
#'
#' @return the BCI dataset used in the analysis
read_data_bci <- function(){
  BCI <- list()
  for (i in 1:8) {
    BCI[[i]] <- lapply(here::here("data", "BCI", glue::glue("bci.tree{i}.rdata")), function(x) get(load(x)))
  }
  BCI <- dplyr::bind_rows(BCI)
  return(BCI)
}

#' Compute growth for BCI
#'
#' @param data 
#'
#' @return the same dataframe with some new columns, including annual growth
compute_growth_BCI <- function(data){
  data <- data%>%
    #convert diameter from mm to cm
    dplyr::mutate(dbh = dbh/10)%>%
    dplyr::arrange(treeID, ExactDate)%>%
    dplyr::group_by(treeID, stemID, hom) %>%
    dplyr::rename(D_tp1=dbh)%>%
    dplyr::mutate(Date_tp1 = lubridate::as_date(lubridate::ymd(ExactDate)),
                  D_t = c(NA, D_tp1[1 : NROW(D_tp1)-1]), 
                  Date_t =  lubridate::as_date(c(NA, Date_tp1[1 : NROW(Date_tp1)-1]))) %>%
    dplyr::mutate(DiffD = dplyr::case_when(Date_tp1 != Date_t ~ D_tp1 - D_t) ,
                  DiffDate = Date_tp1 - Date_t) %>%
    dplyr::mutate(G_tp1 = DiffD/(as.numeric(DiffDate/365))) %>%
    dplyr::ungroup()
  return(data)
}

#' Compute mean growth for BCI
#'
#' @param data 
#'
#' @return a dataframe with mean individual growth
compute_mean_growth_BCI <- function(data){
  data <- data %>%
    dplyr::group_by(treeID) %>%
    dplyr::mutate(Mean_G_Tree = ((dplyr::last(na.omit(D_t)) - dplyr::first(na.omit(D_t))))/(as.numeric((dplyr::last(na.omit(Date_t)) - dplyr::first(na.omit(Date_t))))/365)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(Sp = as.factor(sp), Tree=as.factor(treeID))
  
  data <- data[which(is.na(data$Mean_G_Tree)==F),]
  
  return(data)
}


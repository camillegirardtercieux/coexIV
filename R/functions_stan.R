#' Prepare data for Stan model
#'
#' @param data 
#'
#' @return data
prep_stan <- function(data){
  
  data<- data[which(is.na(data$Growth)==F&is.na(data$Diameter)==F),]
  
  #To ensure no identifiability problem for random effects :
  # Erase single observations !
  data <- data%>%
    dplyr::group_by(idTree)%>%
    dplyr::mutate(nobs_tree=dplyr::n())%>%
    dplyr::filter(nobs_tree>1)%>%
    dplyr::ungroup()%>%
    dplyr::group_by(IdentSpecies)%>%
    dplyr::mutate(nobs_sp=dplyr::n())%>%
    dplyr::filter(nobs_sp>1)%>%
    dplyr::ungroup()
  
  data$IdentSpecies <- as.factor(data$Sp)
  data$IdentSpecies <- factor(data$IdentSpecies)
  levels(data$IdentSpecies) <- 1:length(levels(data$IdentSpecies))
  
  data$IdentTree <- data$idTree
  data$IdentTree <- factor(data$IdentTree)
  levels(data$IdentTree) <- 1:length(levels(droplevels(data$IdentTree)))
  
  #constant to avoid NaNs when logging
  # 2mm for all (minimum, see data manipulation earlier)
  
  data$logG <- data$Growth + 2
  
  data$logD <- log(data$Diameter)
  data$logG <- log(data$logG)
  
  #scale
  data$logG<-scale(data$logG)
  data$logD<-scale(data$logD)
  
  return(data)
}
#' Print two abudance diagrams
#'
#' @param data a dataframe
#' @param site a character chain
#'
#' @return two barplots
abundance_diagram <- function(data, site){
  Table <- as.data.frame(sort(table(data$Sp)))
  Table[,1] <- as.character(Table[,1])
  
  Table$Prop <- Table$Freq/sum(Table$Freq)
  colnames(Table) <- c("Species", "Inds_nb", "Prop")
  Table <- Table[order(Table$Inds_nb, decreasing = TRUE),]
  Table$CumProp <- cumsum(Table$Prop)
  Table$Species_ID <- c(1:nrow(Table))
  
  g <- ggplot2::ggplot(Table, ggplot2::aes(x=Species_ID, y=CumProp))+
    ggplot2::geom_point()+
    ggplot2::xlab("Species number")+
    ggplot2::ylab("Cumulative proportion of individuals")+
    ggplot2::labs(title=paste("All species", site, sep='\n'))
  
  ggplot2::ggsave(filename = glue::glue("{site}_abundance.png"), path=here::here("outputs", "tropical_analysis", "figures"), device = "png", width = 7, height = 5)
  
  if (site=="Paracou") {
    Table_determined_species <- Table[which(as.character(Table$Species)!="Indet_sp"),]
    Table_determined_species$CumProp <- cumsum(Table_determined_species$Prop)
    Table_determined_species$Species_ID <- c(1:nrow(Table_determined_species))
    
    g <- ggplot2::ggplot(Table_determined_species, ggplot2::aes(x=Species_ID, y=CumProp))+
      ggplot2::geom_point()+
      ggplot2::ylim(0,1)+
      ggplot2::xlab("Species number")+
      ggplot2::ylab("Cumulative proportion of individuals")+
      ggplot2::labs(title=paste("Only determined species", site, sep='\n'))
    
    ggplot2::ggsave(filename = "Paracou_abundance_determined.png", path=here::here("outputs", "tropical_analysis", "figures"), device = "png", width = 7, height = 5)
    
  }
}
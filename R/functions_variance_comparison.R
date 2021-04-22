#new sampling strategy : take 10 individuals of each species (or less if the species has less than 10 individuals)
# as we compute the distances within species, we can sample (with d <=100) at the same time.
# then the comparison inter intra must be done only after the computation of all species matrices
# if it is a problem for RAM, the distance datasets for each species must be stored in a file.

#Test
# data <- data.frame(Sp=sample(1:3, 25000, replace = T), Mean_G_Tree = runif(25000, min = 0, max = 1))
# n_ind <- 10
# write.table(data.frame(
#   i= c(rep(0, 9), rep(1, 8)),
#   j = c(1:9, 2:9),
#   dist=runif(17, min = 1, max = 100)),
#   "file0.txt")
# write.table(data.frame(
#   i = c(rep(2, 7), rep(3, 6), rep(4, 5), rep(5, 4), rep(6, 3), rep(7, 2), 8),
#   j = c(3:9, 4:9, 5:9, 6:9, 7:9, 8:9, 9),
#   dist=runif(28, min = 1, max = 100)),
#   "file1.txt")

#distlist(cbind(Data_BCI$X, Data_BCI$Y))
#file.move("C:/path/to/file/some_file.txt", "C:/some/other/path")

#' Variance comparison
#'
#' @param data the whole dataset as a tibble or dataframe
#' @param nfiles number of files created for each plot by distlist
#' @param dataset_name string of characters - each dataset must begin with a different letter
#'
#' @return a dataframe with the results of the comparison for each species
variance_comp <- function(data, nfiles, dataset_name) {
  
  List_species <- as.data.frame(sort(table(data$Sp)))
  List_species <- List_species[which((as.character(List_species$Var1))!="Indet_sp"),]
  rownames(List_species) <- c(1:nrow(List_species))
  Abundance_species <- List_species
  List_species <- as.vector(List_species[,1])
  
  Df_comp_var_inter_intra <- data.frame(Species=character(length(List_species)),
                                        Var_intra=numeric(length(List_species)),
                                        Var_inter=numeric(length(List_species)),
                                        Var_inter_sample=numeric(length(List_species)),
                                        Var_intra_inf=numeric(length(List_species)),
                                        Var_intra_inf_sample=numeric(length(List_species)),
                                        P_val=numeric(length(List_species)),
                                        P_val_sample=numeric(length(List_species)),
                                        Signif=numeric(length(List_species)),
                                        Signif_sample=numeric(length(List_species)),
                                        Nb_ind=numeric(length(List_species)),
                                        Nb_pairs_consp=numeric(length(List_species)),
                                        Nb_pairs_het=numeric(length(List_species)),
                                        Nb_pairs_het_sample=numeric(length(List_species)))
  
  #Compute intraspecific and interspecific semivariance for each species
  for (k in 1:length(List_species)){
    sp <- List_species[k]
    print(sp)
    n_consp <- 0
    n_het <- 0
    n_het_sample <- 0
    Vec_squared_diff_consp <- c(0)
    Vec_squared_diff_het <- c(0)
    Vec_squared_diff_het_sample <- c(0)
    Sum_squared_diff_consp <- 0
    Sum_squared_diff_het_sample <- c(0)
    Sum_squared_diff_het <- 0
    
    Df_comp_var_inter_intra$Species[k] <- sp
    Df_comp_var_inter_intra$Nb_ind[k] <- Abundance_species$Freq[k]
    Df_comp_var_inter_intra$Var_intra[k] <- NA
    Df_comp_var_inter_intra$Var_inter[k] <- NA
    Df_comp_var_inter_intra$Var_inter_sample[k] <- NA
    Df_comp_var_inter_intra$Var_intra_inf[k] <- NA
    Df_comp_var_inter_intra$Var_intra_inf_sample[k] <- NA
    Df_comp_var_inter_intra$P_val[k] <- NA
    Df_comp_var_inter_intra$P_val_sample[k] <- NA
    Df_comp_var_inter_intra$Signif[k] <- NA
    Df_comp_var_inter_intra$Signif_sample[k] <- NA
    Df_comp_var_inter_intra$Nb_pairs_consp[k] <- NA
    Df_comp_var_inter_intra$Nb_pairs_het[k] <- NA
    Df_comp_var_inter_intra$Nb_pairs_het_sample[k] <- NA
    
    if(Abundance_species$Freq[k]>5){
      
      for(plot in sort(unique(data$Plot))) {
        
        data_plot <- data[which(data$Plot==plot),]
        
        for (l in 0:(nfiles-1)) { #charge separately each sub-dataset with distances
          Dists <- read.table(here::here("outputs", glue::glue("Dists_{dataset_name}"), glue::glue("file{l}_{plot}_{substr(dataset_name, 1, 1)}.txt"))) #beware, indices were computed with C++ and therefore start with 0
          colnames(Dists) <- c("i", "j", "dist")
          Dists <- Dists %>%
            dplyr::mutate(Sp_1=data_plot[i+1,]$Sp,
                          Sp_2=data_plot[j+1,]$Sp,
                          Growth_1=data_plot[i+1,]$Mean_G_Tree,
                          Growth_2=data_plot[j+1,]$Mean_G_Tree,
                          Squared_diff=(Growth_1-Growth_2)^2,
                          Couple = paste0(as.character(sort(c(Sp_1, Sp_2))[1]), "_", as.character(sort(c(Sp_1, Sp_2))[2])))
          #remove NA
          Dists <- Dists[which(is.na(Dists$Squared_diff)==F),]
          gc()
          
          #keep only couples containing focal species
          Dists <- Dists[which(Dists$Sp_1==sp | Dists$Sp_2==sp),]
          gc()
          
          #compute sum of squared diff for heterospecific couples
          Dists_het <- Dists[which(Dists$Sp_1 !=Dists$Sp_2),]
          Sum_squared_diff_het <- Sum_squared_diff_het + sum(Dists_het$Squared_diff)
          n_het <- n_het + nrow(Dists_het)
          Vec_squared_diff_het <- c(Vec_squared_diff_het, Dists_het$Squared_diff)#this vector will be used to perform the test
          
          #compute sum of squared diff for conspecific couples
          Dists_consp <- Dists[which(Dists$Sp_1==sp & Dists$Sp_2==sp),]
          Sum_squared_diff_consp <- Sum_squared_diff_consp + sum(Dists_consp$Squared_diff)
          n_consp <- n_consp + nrow(Dists_consp)
          Vec_squared_diff_consp <- c(Vec_squared_diff_consp, Dists_consp$Squared_diff)#this vector will be used to perform the test
          
          #compute sum of squared diff for heterospecific couples, but with at most 10 individuals per species
          Dists_het_sample <- Dists_het%>%
            dplyr::group_by(Couple)%>%
            dplyr::mutate(n_couple = dplyr::n())%>%
            dplyr::slice_sample(n=10)
          Sum_squared_diff_het_sample <- Sum_squared_diff_het_sample + sum(Dists_het_sample$Squared_diff)
          n_het_sample <- n_het_sample + nrow(Dists_het_sample)
          Vec_squared_diff_het_sample <- c(Vec_squared_diff_het_sample, Dists_het_sample$Squared_diff)
          
          rm(Dists)
          gc()
          
        }#end of loop on fractioned distance datasets
        
      }#end of loop on plots
      
      semivar_consp <- (1/(2*n_consp))*Sum_squared_diff_consp
      Df_comp_var_inter_intra$Var_intra[k] <- semivar_consp
      Df_comp_var_inter_intra$Nb_pairs_consp[k] <- n_consp
      
      semivar_het <- (1/(2*n_het))*Sum_squared_diff_het
      Df_comp_var_inter_intra$Var_inter[k] <- semivar_het
      Df_comp_var_inter_intra$Nb_pairs_het[k] <- n_het
      
      semivar_het_sample <- (1/(2*n_het_sample))*Sum_squared_diff_het_sample
      Df_comp_var_inter_intra$Var_inter_sample[k] <- semivar_het_sample
      Df_comp_var_inter_intra$Nb_pairs_het_sample[k] <- n_het_sample
      
      if(is.na(semivar_consp)==F && is.na(semivar_het)==F && n_consp > 5 && n_het > 5) {
        
        Data_Test <- data.frame(
          Squared_diff = c(Vec_squared_diff_het, Vec_squared_diff_consp),
          Group = c(rep("Het", length(Vec_squared_diff_het)), rep("Consp", length(Vec_squared_diff_consp))))
        
        U <- wilcox.test(Squared_diff~Group, data=Data_Test)
        
        Df_comp_var_inter_intra$P_val[k] <- U$p.value
        
        if(U$p.value<0.05&&semivar_consp<semivar_het){
          Df_comp_var_inter_intra$Var_intra_inf[k] <- 1
          Df_comp_var_inter_intra$Signif[k] <- 1
        }
        
        if(U$p.value>=0.05&&semivar_consp<semivar_het){
          Df_comp_var_inter_intra$Var_intra_inf[k] <- 1
          Df_comp_var_inter_intra$Signif[k] <- 0
        }
        
        if(U$p.value<0.05&&semivar_consp>=semivar_het){
          Df_comp_var_inter_intra$Var_intra_inf[k] <- 0
          Df_comp_var_inter_intra$Signif[k] <- 1
        }
        
        if(U$p.value>=0.05&&semivar_consp>=semivar_het){
          Df_comp_var_inter_intra$Var_intra_inf[k] <- 0
          Df_comp_var_inter_intra$Signif[k] <- 0
        }
        
      }#end of condition on number of pairs (conspecific and heterospecific)
      
      if(is.na(semivar_consp)==F && is.na(semivar_het_sample)==F && n_consp > 5 && n_het_sample > 5) {
        
        Data_Test <- data.frame(
          Squared_diff = c(Vec_squared_diff_het_sample, Vec_squared_diff_consp),
          Group = c(rep("Het_sample", length(Vec_squared_diff_het_sample)), rep("Consp", length(Vec_squared_diff_consp))))
        
        U <- wilcox.test(Squared_diff~Group, data=Data_Test)
        
        Df_comp_var_inter_intra$P_val_sample[k] <- U$p.value
        
        if(U$p.value<0.05&&semivar_consp<semivar_het_sample){
          Df_comp_var_inter_intra$Var_intra_inf_sample[k] <- 1
          Df_comp_var_inter_intra$Signif_sample[k] <- 1
        }
        
        if(U$p.value>=0.05&&semivar_consp<semivar_het_sample){
          Df_comp_var_inter_intra$Var_intra_inf_sample[k] <- 1
          Df_comp_var_inter_intra$Signif_sample[k] <- 0
        }
        
        if(U$p.value<0.05&&semivar_consp>=semivar_het_sample){
          Df_comp_var_inter_intra$Var_intra_inf_sample[k] <- 0
          Df_comp_var_inter_intra$Signif_sample[k] <- 1
        }
        
        if(U$p.value>=0.05&&semivar_consp>=semivar_het_sample){
          Df_comp_var_inter_intra$Var_intra_inf_sample[k] <- 0
          Df_comp_var_inter_intra$Signif_sample[k] <- 0
        }
        
      }#end of condition on number of pairs (conspecific and heterospecific with sample)
      
    }#end of condition on species abundance
    
  }#end of loop on species
  
  return(Df_comp_var_inter_intra)
  
}#end of function

#' Calculate the proportion of individuals in each category
#'
#' @param data 
#'
#' @return a vector containing the 4 proportions
props_ind_comp_var <- function(data){
  
  Prop_ind_var_sp_inf_all_signif <- sum(
    data[which(
      data$Var_intra_inf==1&data$Signif==1),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_inf_all_unsignif <- sum(
    data[which(
      data$Var_intra_inf==1&data$Signif==0),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_sup_all_signif <- sum(
    data[which(
      data$Var_intra_inf==0&data$Signif==1),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_sup_all_unsignif <- sum(
    data[which(
      data$Var_intra_inf==0&data$Signif==0),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_inf_all_signif_sample <- sum(
    data[which(
      data$Var_intra_inf_sample==1&data$Signif_sample==1),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_inf_all_unsignif_sample <- sum(
    data[which(
      data$Var_intra_inf_sample==1&data$Signif_sample==0),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_sup_all_signif_sample <- sum(
    data[which(
      data$Var_intra_inf_sample==0&data$Signif_sample==1),]$Nb_ind
  )/sum(data$Nb_ind)
  
  Prop_ind_var_sp_sup_all_unsignif_sample <- sum(
    data[which(
      data$Var_intra_inf_sample==0&data$Signif_sample==0),]$Nb_ind
  )/sum(data$Nb_ind)
  
  return(c(Prop_ind_var_sp_inf_all_signif,
           Prop_ind_var_sp_inf_all_unsignif,
           Prop_ind_var_sp_sup_all_signif,
           Prop_ind_var_sp_sup_all_unsignif,
           Prop_ind_var_sp_inf_all_signif_sample,
           Prop_ind_var_sp_inf_all_unsignif_sample,
           Prop_ind_var_sp_sup_all_signif_sample,
           Prop_ind_var_sp_sup_all_unsignif_sample))
}

#' compute a table with the proportion of individuals and species with var inter < var intra for each dataset
#'
#' @param data2 
#' @param data3 
#' @param site1 
#' @param site2 
#' @param site3 
#' @param data1 
#'
#' @return a dataframe
results_var_comp <- function(data1, data2, data3, site1, site2, site3){
  #Taking only species for which we performed the test into account
  data1 <- data1%>%
    dplyr::filter(is.na(Signif)==FALSE)
  data2 <- data2%>%
    dplyr::filter(is.na(Signif)==FALSE)
  data3 <- data3%>%
    dplyr::filter(is.na(Signif)==FALSE)
  
  props_ind_data1 <- props_ind_comp_var(data1)
  props_ind_data2 <- props_ind_comp_var(data2)
  props_ind_data3 <- props_ind_comp_var(data3)
  
  results <-  data.frame(
    Proportion = rep(c("Proportion of species", "Proportion of individuals"), 3),
    Intra_inf_inter = c(
      format((length(which(data1$Var_intra_inf==1&data1$Signif==1))/nrow(data1))*100, digits=3),
      format(props_ind_data1[1]*100, digits=3),
      format((length(which(data2$Var_intra_inf==1&data2$Signif==1))/nrow(data2))*100, digits=3),
      format(props_ind_data2[1]*100, digits=3),
      format((length(which(data3$Var_intra_inf==1&data3$Signif==1))/nrow(data3))*100, digits=3),
      format(props_ind_data3[1]*100, digits=3)
    ),
    Unsignif = c(
      format(((length(which(data1$Var_intra_inf==0&data1$Signif==0))+length(which(data1$Var_intra_inf==1&data1$Signif==0)))/nrow(data1))*100, digits=3),
      format((props_ind_data1[2]+props_ind_data1[4])*100, digits=3),
      format(((length(which(data2$Var_intra_inf==0&data2$Signif==0))+length(which(data2$Var_intra_inf==1&data2$Signif==0)))/nrow(data2))*100, digits=3),
      format((props_ind_data2[2]+props_ind_data2[4])*100, digits=3),
      format(((length(which(data3$Var_intra_inf==0&data3$Signif==0))+length(which(data3$Var_intra_inf==1&data3$Signif==0)))/nrow(data3))*100, digits=3),
      format((props_ind_data3[2]+props_ind_data3[4])*100, digits=3)
    ),
    Intra_sup_inter =  c(
      format((length(which(data1$Var_intra_inf==0&data1$Signif==1))/nrow(data1))*100, digits=3),
      format(props_ind_data1[3]*100, digits=3),
      format((length(which(data2$Var_intra_inf==0&data2$Signif==1))/nrow(data2))*100, digits=3),
      format(props_ind_data2[3]*100, digits=3),
      format((length(which(data3$Var_intra_inf==0&data3$Signif==1))/nrow(data3))*100, digits=3),
      format(props_ind_data3[3]*100, digits=3)
    )
  )
  
  colnames(results) <- c(
    " ",
    "Intraspecific variability < interspecific variability (i)",
    "Intraspecific variability ~ interspecific variability (ii)",
    "Intraspecific variability > interspecific variability (iii)"
  )
  
  return(results)
}
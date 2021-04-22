#' Moran's I computation
#'
#' @param data 
#'
#' @return A vector of length 4 corresponding to Moran's index, the p-value, the expected value under H0 and the standard deviation of the normal distribution
moran_I <- function(data) {
  
  Mean_growth <- mean(data$G_Tree_1)
  N <- length(unique(c(data$Tree_1, data$Tree_2)))
  
  if(N >= 5){#we consider that less than 5 individuals does not allow to have a relevant result
    
    data <- data %>%
      dplyr::mutate(
        centered_G_1 = G_Tree_1-Mean_growth,
        centered_G_2 = G_Tree_2-Mean_growth,
        weight = dplyr::case_when(Dist!=0 ~ 1/Dist,
                                  Dist == 0 ~ Dist),
      )%>%
      dplyr::group_by(Tree_1)%>%
      dplyr::mutate(
        weight_j = sum(weight),
        normalised_weight = dplyr::case_when(weight_j!=0 ~ weight/weight_j,
                                             weight_j == 0 ~ weight),
        numerator_moran=sum(normalised_weight*(centered_G_1)*(centered_G_2)),
      )%>%
      dplyr::ungroup()
    
    W <- sum(data$normalised_weight)
    
    data <- within(
      data,
      weight_ji <- normalised_weight[order(Row)][order(Col, Row)])
    
    # a little longer
    #data <- merge(data, data[, c("Col", "Row", "normalised_weight")], by.x = c('Row', 'Col'), by.y = c('Col', 'Row'))%>%
    #dplyr::rename(normalised_weight=normalised_weight.x,  weight_ji=normalised_weight.y)
    
    data$intermediate_s1 <- numeric(nrow(data))
    data$intermediate_s1 <- (data$normalised_weight + data$weight_ji)^2
    
    Data_2 <- data %>%
      dplyr::group_by(Tree_1)%>%
      dplyr::mutate(intermediate_s2=(sum(weight_ji)+sum(normalised_weight))^2)%>%
      dplyr::slice(1)%>%
      dplyr::ungroup()%>%
      dplyr::mutate(
        denominator_moran = (centered_G_1)^2
      )
    
    observed <-(sum(Data_2$numerator_moran)/sum(Data_2$denominator_moran))*(N/W)
    
    expected <- -1/(N-1)
    
    s1 <- (1/2)*(sum(data$intermediate_s1))
    
    s2 <- sum(Data_2$intermediate_s2)
    
    s3 <- ((1/N) * sum((Data_2$centered_G_1)^4)) / (((1/N) * sum(Data_2$centered_G_1^2))^2)
    
    s4 <- (N^2 - 3*N +3)*s1 - N*s2 + 3*W^2
    s5 <- (N^2-N)*s1 - 2*N*s2 + 6*W^2
    sigma <- sqrt((N*s4-s3*s5)/((N-1)*(N-2)*(N-3)*W^2) - expected^2)
    
    #alternative == "greater"
    p_val <- 1-pnorm(observed, mean = expected, sd = sigma)
  }
  else observed <- p_val <- expected <- sigma <- NaN
  
  #To compare with ape package : do not remove dists < 100 and individuals which are not in the same plot
  #weight_ape <- 1/Data_Moran_dists
  #diag(weight_ape) <- 0
  #Moran_ape <- Moran.I(Data_Moran$Mean_G_Tree, weight_ape, alternative = "greater")
  
  return(c(observed, p_val, expected, sigma))
}

#' Do the Moran analysis of the data
#'
#' @param data 
#'
#' @return a dataframe containing the results of the Moran analysis
moran_analysis <- function(data) {
  Data_identified <- data[which((as.character(data$Sp))!="Indet_sp"),]
  
  res <- data.frame(Sp=levels(droplevels(Data_identified$Sp)), Moran.I=numeric(length(levels(droplevels(Data_identified$Sp)))),                      p_value=numeric(length(levels(droplevels(Data_identified$Sp)))), signif=numeric(length(levels(droplevels(Data_identified$Sp)))),
                    nb_ind=numeric(length(levels(droplevels(Data_identified$Sp)))))
  
  for(k in 1:nrow(res)) {
    sp = as.character(res$Sp[k])
    print(sp)
    Data_sp <- Data_identified[which(Data_identified$Sp==sp),]
    Data_sp <- Data_sp[which(is.na(Data_sp$Mean_G_Tree)==F),]
    res$nb_ind[k] <- nrow(Data_sp)
    #The number of individuals within the species must be higher than 2, and we consider that below 5 individuals the index is not relevant
    if (res$nb_ind[k]>5){
      
      if(res$nb_ind[k]>3000){#sample for very abundant species (memory issues otherwise)
        sample <- sample(1:res$nb_ind[k], 3000)
        Data_sp <- Data_sp[sample,]
      }
      
      Data_Moran<-data.frame(Tree=Data_sp$Tree, X=Data_sp$X, Y=Data_sp$Y, Mean_G_Tree=Data_sp$Mean_G_Tree, Plot=Data_sp$Plot)
      
      rm(Data_sp)
      gc()
      
      #At least one value must differ from 0 in order to compute Moran's I
      if(length(which(Data_Moran$Mean_G_Tree!=0))!=0){
        Data_Moran_dists <- as.matrix(arma_distmat(cbind(Data_Moran$X, Data_Moran$Y)))
        
        xy <- cbind(rep(1:nrow(Data_Moran), each=nrow(Data_Moran)), rep(1:nrow(Data_Moran), nrow(Data_Moran)))
        
        colnames(xy) <- c("Row", "Col")
        Growth_dists <- data.frame(xy, Dist=Data_Moran_dists[xy])
        
        rm(Data_Moran_dists)
        gc()
        
        Growth_dists <- Growth_dists %>%
          dplyr::mutate(
            Sp_1=Data_Moran[Row,]$Sp,
            Sp_2=Data_Moran[Col,]$Sp,
            Plot_1=Data_Moran[Row,]$Plot,
            Plot_2=Data_Moran[Col,]$Plot,
            Tree_1=Data_Moran[Row,]$Tree,
            Tree_2=Data_Moran[Col,]$Tree,
            G_Tree_1 = Data_Moran[Row,]$Mean_G_Tree,
            G_Tree_2 =Data_Moran[Col,]$Mean_G_Tree)
        
        rm(Data_Moran)
        gc()
        
        #only small distances
        Growth_dists <- Growth_dists[which(Growth_dists$Dist<=100),]
        
        #only distances within plots
        Growth_dists <- Growth_dists[which(Growth_dists$Plot_1==Growth_dists$Plot_2),]
        rownames(Growth_dists) <- c(1:nrow(Growth_dists))
        
        Mor <- Paracou.Uppangala.BCI::moran_I(Growth_dists)
        rm(Growth_dists)
        gc()
        res$Moran.I[k]<-Mor[1]
        res$p_value[k]<-Mor[2]
        
        if (is.na(res$p_value[k])==TRUE) {res$signif[k] <- NaN}
        else {
          if (res$p_value[k]<0.05) {res$signif[k] <- 1}
          else {res$signif[k] <- 0}
        }
      }
      else{
        res$Moran.I[k] <- NaN 
        res$p_value[k] <- NaN
        res$signif[k] <- NaN
      }
    }
    else{
      res$Moran.I[k] <- NaN 
      res$p_value[k] <- NaN
      res$signif[k] <- NaN
    }
    
  }
  
  res$Prop_ind <- res$nb_ind/sum(res$nb_ind)
  
  return(res)
}

#' Compute the proportion of species and individuals in the different categories of results of the Moran test
#'
#' @param data1 a dataframe
#' @param data2 a dataframe
#' @param data3 a dataframe
#'
#' @return Results_moran, a dataframe
results_moran <- function(data1, data2, data3){
  
  #Number of species with significant spatial autocorrelation
  nb_sp_signif_site1 <- sum(na.omit(data1$signif))
  nb_sp_signif_site2 <- sum(na.omit(data2$signif))
  nb_sp_signif_site3 <- sum(na.omit(data3$signif))
  #Mean number of individuals represented by those species
  mean_nb_ind_signif_site1 <- mean(data1[which(data1$signif==1),]$nb_ind)
  mean_nb_ind_signif_site2 <- mean(data2[which(data2$signif==1),]$nb_ind)
  mean_nb_ind_signif_site3 <- mean(data3[which(data3$signif==1),]$nb_ind)
  #Proportion of individuals represented by those species
  prop_ind_signif_site1 <- sum(data1[which(data1$signif==1),]$Prop_ind)*100
  prop_ind_signif_site2 <- sum(data2[which(data2$signif==1),]$Prop_ind)*100
  prop_ind_signif_site3 <- sum(data3[which(data3$signif==1),]$Prop_ind)*100
  #Proportion of individuals represented by those species among tested species
  prop_ind_signif_site1_test <- (sum(data1[which(data1$signif==1),]$nb_ind)/sum(data1[which(is.na(data1$signif)==FALSE),]$nb_ind))*100
  prop_ind_signif_site2_test <- (sum(data2[which(data2$signif==1),]$nb_ind)/sum(data2[which(is.na(data2$signif)==FALSE),]$nb_ind))*100
  prop_ind_signif_site3_test <- (sum(data3[which(data3$signif==1),]$nb_ind)/sum(data3[which(is.na(data3$signif)==FALSE),]$nb_ind))*100
  
  #Number of species with no significant spatial autocorrelation
  nb_sp_unsignif_site1 <- length(which(data1$signif == 0))
  nb_sp_unsignif_site2 <- length(which(data2$signif == 0))
  nb_sp_unsignif_site3 <- length(which(data3$signif == 0))
  #Mean number of individuals represented by those species
  mean_nb_ind_unsignif_site1 <- mean(data1[which(data1$signif==0),]$nb_ind)
  mean_nb_ind_unsignif_site2 <- mean(data2[which(data2$signif==0),]$nb_ind)
  mean_nb_ind_unsignif_site3 <- mean(data3[which(data3$signif==0),]$nb_ind)
  #Proportion of individuals represented by species with no significant spatial autocorrelation
  prop_ind_unsignif_site1 <- sum(data1[which(data1$signif==0),]$Prop_ind)*100
  prop_ind_unsignif_site2 <- sum(data2[which(data2$signif==0),]$Prop_ind)*100
  prop_ind_unsignif_site3 <- sum(data3[which(data3$signif==0),]$Prop_ind)*100
  #Proportion of individuals represented by those species among tested species
  prop_ind_unsignif_site1_test <- (sum(data1[which(data1$signif==0),]$nb_ind)/sum(data1[which(is.na(data1$signif)==FALSE),]$nb_ind))*100
  prop_ind_unsignif_site2_test <- (sum(data2[which(data2$signif==0),]$nb_ind)/sum(data2[which(is.na(data2$signif)==FALSE),]$nb_ind))*100
  prop_ind_unsignif_site3_test <- (sum(data3[which(data3$signif==0),]$nb_ind)/sum(data3[which(is.na(data3$signif)==FALSE),]$nb_ind))*100
  
  #Number of species which do not have enough data or non-null data to compute Moran's I
  nb_sp_NA_site1 <- sum(is.na(data1$signif))
  nb_sp_NA_site2 <- sum(is.na(data2$signif))
  nb_sp_NA_site3 <- sum(is.na(data3$signif))
  #Mean number of individuals represented by those species
  mean_nb_ind_NA_site1 <- mean(data1[which(is.na(data1$signif)),]$nb_ind)
  mean_nb_ind_NA_site2 <- mean(data2[which(is.na(data2$signif)),]$nb_ind)
  mean_nb_ind_NA_site3 <- mean(data3[which(is.na(data3$signif)),]$nb_ind)
  #Proportion of individuals represented by species with no significant spatial autocorrelation
  prop_ind_NA_site1 <- sum(data1[which(is.na(data1$signif)),]$Prop_ind)*100
  prop_ind_NA_site2 <- sum(data2[which(is.na(data2$signif)),]$Prop_ind)*100
  prop_ind_NA_site3 <- sum(data3[which(is.na(data3$signif)),]$Prop_ind)*100
  
  #total number of species
  nb_sp_site1 <- nb_sp_signif_site1+nb_sp_unsignif_site1+nb_sp_NA_site1
  nb_sp_site2 <- nb_sp_signif_site2+nb_sp_unsignif_site2+nb_sp_NA_site2
  nb_sp_site3 <- nb_sp_signif_site3+nb_sp_unsignif_site3+nb_sp_NA_site3
  
  #total number of tested species
  nb_sp_site1_test <- length(which(is.na(data1$signif)==FALSE))
  nb_sp_site2_test <- length(which(is.na(data2$signif)==FALSE))
  nb_sp_site3_test <- length(which(is.na(data3$signif)==FALSE))
  
  Results_moran <- data.frame(
    Proportion = rep(c("Proportion of species", "Proportion of individuals"), 3),
    Signif = c(
      format((nb_sp_signif_site1/nb_sp_site1_test)*100, digits=3),
      format(prop_ind_signif_site1_test, digits=3),
      format((nb_sp_signif_site2/nb_sp_site2_test)*100, digits=3),
      format(prop_ind_signif_site2_test, digits=3),
      format((nb_sp_signif_site3/nb_sp_site3_test)*100, digits=3),
      format(prop_ind_signif_site3_test, digits=3)
    ),
    Unsignif = c(
      format(nb_sp_unsignif_site1/nb_sp_site1_test*100, digits=3),
      format(prop_ind_unsignif_site1_test, digits=3),
      format(nb_sp_unsignif_site2/nb_sp_site2_test*100, digits=3),
      format(prop_ind_unsignif_site2_test, digits=3),
      format(nb_sp_unsignif_site3/nb_sp_site3_test*100, digits=3),
      format(prop_ind_unsignif_site3_test, digits=3)
    )
    # No_resut = c(
    #   format(nb_sp_NA_site1/nb_sp_site1*100, digits=3),
    #   format(prop_ind_NA_site1, digits=3),
    #   format(nb_sp_NA_site2/nb_sp_site2*100, digits=3),
    #   format(prop_ind_NA_site2, digits=3),
    #   format(nb_sp_NA_site3/nb_sp_site3*100, digits=3),
    #   format(prop_ind_NA_site3, digits=3)
    #   )
  )
  
  colnames (Results_moran) <- c(" ", "Significant (i)", "Not significant (ii)")                      
  return(Results_moran)
  
}

results_moran_undisturbed <- function(data){
  
  #Number of species with significant spatial autocorrelation
  nb_sp_signif <- sum(na.omit(data$signif))
  #Mean number of individuals represented by those species
  mean_nb_ind_signif <- mean(data[which(data$signif==1),]$nb_ind)
  #Proportion of individuals represented by those species
  prop_ind_signif <- sum(data[which(data$signif==1),]$Prop_ind)*100
  #Proportion of individuals represented by those species among tested species
  prop_ind_signif_test <- (sum(data[which(data$signif==1),]$nb_ind)/sum(data[which(is.na(data$signif)==FALSE),]$nb_ind))*100
  #Number of species with no significant spatial autocorrelation
  nb_sp_unsignif <- length(which(data$signif == 0))
  #Mean number of individuals represented by those species
  mean_nb_ind_unsignif <- mean(data[which(data$signif==0),]$nb_ind)
  #Proportion of individuals represented by species with no significant spatial autocorrelation
  prop_ind_unsignif <- sum(data[which(data$signif==0),]$Prop_ind)*100
  #Proportion of individuals represented by those species among tested species
  prop_ind_unsignif_test <- (sum(data[which(data$signif==0),]$nb_ind)/sum(data[which(is.na(data$signif)==FALSE),]$nb_ind))*100
  #Number of species which do not have enough data or non-null data to compute Moran's I
  nb_sp_NA <- sum(is.na(data$signif))
  #Mean number of individuals represented by those species
  mean_nb_ind_NA <- mean(data[which(is.na(data$signif)),]$nb_ind)
  #Proportion of individuals represented by species with no significant spatial autocorrelation
  prop_ind_NA <- sum(data[which(is.na(data$signif)),]$Prop_ind)*100
  #total number of species
  nb_sp <- nb_sp_signif+nb_sp_unsignif+nb_sp_NA
  #total number of tested species
  nb_sp_test <- length(which(is.na(data$signif)==FALSE))
  
  Results_moran <- data.frame(
    Proportion = c("Proportion of species", "Proportion of individuals"),
    Signif = c(
      format((nb_sp_signif/nb_sp_test)*100, digits=3),
      format(prop_ind_signif_test, digits=3)
    ),
    Unsignif = c(
      format(nb_sp_unsignif/nb_sp_test*100, digits=3),
      format(prop_ind_unsignif_test, digits=3)
    )
  )
  
  colnames (Results_moran) <- c(" ", "Significant (i)", "Not significant (ii)")                      
  return(Results_moran)
  
}

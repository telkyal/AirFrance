# Import tables    #
setwd("C:/Users/Paul/Desktop/PIE")
data_AF <- read.csv("Données/new_dataset_supaero.csv",header = T, sep=",", dec=".")
data_AF=na.omit(data_AF)


# Variables creation #
data1=data_AF
data1$CHOICE=TRUE
data2=data_AF
data2$CHOICE=FALSE
data_AF=rbind(data1,data2)

data_AF$WEIGHT=0
data_AF$WEIGHT[data_AF$CHOICE==TRUE]=data_AF$NBR_BKG_1[data_AF$CHOICE==TRUE]
data_AF$WEIGHT[data_AF$CHOICE==FALSE]=data_AF$NBR_BKG_2[data_AF$CHOICE==FALSE]

data_AF$NBR_SEGTS=data_AF$NBR_SEGTS_1-data_AF$NBR_SEGTS_2
data_AF$FREQUENCY=data_AF$FREQUENCY_1-data_AF$FREQUENCY_2
data_AF$ELAPSE_TM=data_AF$ELAPSE_TM_1-data_AF$ELAPSE_TM_2
data_AF$CNX_TM=data_AF$CNX_TM_1-data_AF$CNX_TM_2
data_AF$NBR_CODE_SHARE=data_AF$NBR_CODE_SHARE_1-data_AF$NBR_CODE_SHARE_2
data_AF$PCT_CPY_ORI=data_AF$PCT_CPY_ORI_1-data_AF$PCT_CPY_ORI_2  
data_AF$PCT_CPY_DST=data_AF$PCT_CPY_DST_1-data_AF$PCT_CPY_DST_2  
data_AF$PREF_DEP_TM=data_AF$PREF_DEP_TM_1-data_AF$PREF_DEP_TM_2 
data_AF$NBR_BKG_BC=data_AF$NBR_BKG_BC_1-data_AF$NBR_BKG_BC_2 
data_AF$NBR_BKG_HC=data_AF$NBR_BKG_HC_1-data_AF$NBR_BKG_HC_2 



# k-means
  ## List of itineraries data_AF
temp=which(colnames(data_AF)%in% c("DAY","COD_CTY_ORI","CONTINENT_ORI","COD_CTY_DST","CONTINENT_DST"))
end1=which(grepl("1",names(data_AF)))
end2=which(grepl("2",names(data_AF)))
itin1=data_AF[,c(temp,end1)]
itin2=data_AF[,c(temp,end2)]
names(itin1)=gsub("_1", "", names(itin1))
colnames(itin2)=colnames(itin1)
data=unique(rbind(itin1,itin2))

  ## k-means variables
ONDS_names=unique(data$COD_CTY_DST)
list_ONDS = as.vector(ONDS_names)
featuring <- function(data,ONDS_names){
  features=matrix(, nrow = 0, ncol = 7)
  
  for(dst in ONDS_names){
    data_dst=subset(data,data$COD_CTY_DST==dst)
    data_dst=data_dst[complete.cases(data_dst[,c(9,10)]), ]
    col=c(mean(data_dst$NBR_SEGTS),mean(data_dst$FREQUENCY),
          mean(data_dst$ELAPSE_TM),mean(data_dst$CNX_TM),nrow(data_dst),
          mean(data_dst$NBR_BKG_BC/(data_dst$NBR_BKG_BC+data_dst$NBR_BKG_HC)),
          mean(data_dst$NBR_BKG))
    features=rbind(features,col)
  }
  features=data.frame(features,row.names=ONDS_names)
  colnames(features)=c("mean_nbr_segts","mean_frequency","mean_elapse_tm","mean_cnx_time",
                       "size","economy_ratio","mean_nbr_bkg")
  return(features)
}

features=featuring(data,ONDS_names)
features=scale(features)

  ## Number of clusters
nbr_clusters_max = 4

rm(pct_rel_error)
pct_rel_error = matrix(, nrow = nbr_clusters_max, ncol = nbr_clusters_max-1)
for (k in 2:nbr_clusters_max) {
  model_clustering=kmeans(features,k)
  rm(Matrix)
  Matrix=data.frame(list_ONDS,model_clustering$cluster)
  
  for (j in 1:k) {  
    ## Number of the cluster in {1, .., k}
    # Pour chaque j in {1, .., k}
    rm(M3)
    rm(data_AF_cluster)
    M3=subset(Matrix,Matrix$model_clustering.cluster==j) 
    data_AF_cluster=subset(data_AF,data_AF$COD_CTY_DST %in% M3$list_ONDS)
    
    
# Partitionnement train / test
    rm(x)
    x=data_AF_cluster
    x$ind=1
    x=aggregate(ind~COD_CTY_DST,data=x,sum)
    city_test = as.character(x$COD_CTY_DST[which(x$ind==max(x$ind))])
      
    train=subset(data_AF_cluster, data_AF_cluster$COD_CTY_DST != city_test)
    test=subset(data_AF_cluster, data_AF_cluster$COD_CTY_DST == city_test)
    
    
# Get test all onds          #
    temp=which(colnames(test)%in% c("DAY","COD_CTY_ORI","CONTINENT_ORI","COD_CTY_DST","CONTINENT_DST"))
    temp1=which(grepl("1",names(test)))
    temp2=which(grepl("2",names(test)))
    itin1=test[,c(temp,temp1)]
    names(itin1)=gsub("_1", "", names(itin1))
    itin2=test[,c(temp,temp2)]
    colnames(itin2)=colnames(itin1)
    itin_test=unique(rbind(itin1,itin2))
    
    
# Model with several variables  #
    model_regression <- glm(CHOICE~NBR_SEGTS + FREQUENCY + PCT_CPY_ORI 
                 + ELAPSE_TM + PREF_DEP_TM,
                 weights = WEIGHT, data=train, family = binomial("logit"))
    #summary(model_regression)
    
    
# predict FMS               #
    lin_pred<-predict(model_regression,itin_test)
    FMS<-(exp(lin_pred)/sum(exp(lin_pred)))
    nb_bkg_predict<-(exp(lin_pred)/sum(exp(lin_pred)))*sum(itin_test$NBR_BKG)
    
    
    rel_dif=(nb_bkg_predict-itin_test$NBR_BKG)/itin_test$NBR_BKG # écart relatif
    pct_rel_error[j, k-1] = mean(sqrt(rel_dif*rel_dif))*100 # pct d'erreur en valeur absolue : 246%
  }
}
pct_rel_error

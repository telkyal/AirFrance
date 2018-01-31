#-------------------------------------------------------------------------#

#                                 PIE                                     #

#-------------------------------------------------------------------------#

setwd("D:/pie")
set.seed(1)

# Données d'entraînement:
data_AF <- read.csv("new_dataset_supaero.csv",header = T, sep = ",")
data_AF=na.omit(data_AF)

# Données du test:
data_test <- read.csv("dataset_supaero_prediction.csv",header = T, sep = ";")
data_test=na.omit(data_test)



data_AF=data_AF[,names(data_test)]


## Creation des variables pour data_AF:
d <- data_AF

data1=d
data1$CHOICE=TRUE
data2=d
data2$CHOICE=FALSE
d=rbind(data1,data2)

d$WEIGHT=0
d$WEIGHT[d$CHOICE==TRUE]=d$NBR_BKG_1[d$CHOICE==TRUE]
d$WEIGHT[d$CHOICE==FALSE]=d$NBR_BKG_2[d$CHOICE==FALSE]

d$NBR_SEGTS=d$NBR_SEGTS_1-d$NBR_SEGTS_2
d$FREQUENCY=d$FREQUENCY_1-d$FREQUENCY_2
d$ELAPSE_TM=d$ELAPSE_TM_1-d$ELAPSE_TM_2
d$CNX_TM=d$CNX_TM_1-d$CNX_TM_2
d$NBR_CODE_SHARE=d$NBR_CODE_SHARE_1-d$NBR_CODE_SHARE_2
d$PCT_CPY_ORI=d$PCT_CPY_ORI_1-d$PCT_CPY_ORI_2  
d$PREF_DEP_TM=d$PREF_DEP_TM_1-d$PREF_DEP_TM_2 

data_AF <- d

## Creation de variables pour data_test
d <- data_test

data1=d
data1$CHOICE=TRUE
data2=d
data2$CHOICE=FALSE
d=rbind(data1,data2)

d$WEIGHT=0
d$WEIGHT[d$CHOICE==TRUE]=d$NBR_BKG_1[d$CHOICE==TRUE]
d$WEIGHT[d$CHOICE==FALSE]=d$NBR_BKG_2[d$CHOICE==FALSE]

d$NBR_SEGTS=d$NBR_SEGTS_1-d$NBR_SEGTS_2
d$FREQUENCY=d$FREQUENCY_1-d$FREQUENCY_2
d$ELAPSE_TM=d$ELAPSE_TM_1-d$ELAPSE_TM_2
d$CNX_TM=d$CNX_TM_1-d$CNX_TM_2
d$NBR_CODE_SHARE=d$NBR_CODE_SHARE_1-d$NBR_CODE_SHARE_2
d$PCT_CPY_ORI=d$PCT_CPY_ORI_1-d$PCT_CPY_ORI_2  
# d$PCT_CPY_DST=d$PCT_CPY_DST_1-d$PCT_CPY_DST_2
d$PREF_DEP_TM=d$PREF_DEP_TM_1-d$PREF_DEP_TM_2 
# d$NBR_BKG_BC=d$NBR_BKG_BC_1-d$NBR_BKG_BC_2
# d$NBR_BKG_HC=d$NBR_BKG_HC_1-d$NBR_BKG_HC_2

data_test <- d


rm(d)
# Creation du data frames des itineraires pour le kmeans
create_itineraries <- function(d){
  ## List of itineraries
  temp=which(colnames(d)%in% c("DAY","COD_CTY_ORI","CONTINENT_ORI","COD_CTY_DST","CONTINENT_DST"))
  end1=which(grepl("1",names(d)))
  end2=which(grepl("2",names(d)))
  itin1=d[,c(temp,end1)]
  itin2=d[,c(temp,end2)]
  names(itin1)=gsub("_1", "", names(itin1))
  colnames(itin2)=colnames(itin1)
  data=unique(rbind(itin1,itin2))
  return(data)
}

data_AF_itin <- create_itineraries(data_AF)
data_test_itin <- create_itineraries(data_test)

## k-means

VARIABLES=c("NBR_SEGTS","FREQUENCY","ELAPSE_TM","CNX_TM","NBR_BKG")

featuring <- function(data,ONDS_names,VARIABLES,size)
{
  if(size==TRUE)
  {
    features=matrix(, nrow = 0, ncol = length(VARIABLES)+1)
  }
  
  if(size==FALSE)
  {
    features=matrix(, nrow = 0, ncol = length(VARIABLES))
  }
  
  
  for(dst in ONDS_names){
    data_dst=subset(data,data$COD_CTY_DST==dst)
    names(data_dst)
    data_dst=data_dst[complete.cases(data_dst[,c(which(colnames(data_dst)=="FREQUENCY")
                                                 ,which(colnames(data_dst)=="ELAPSE_TM"))]), ]
    
    k=1
    col=c(mean(data_dst[,which(colnames(data_dst)==VARIABLES[k])]))
    for(k in 2:length(VARIABLES))
    {
      
      col=c(col,mean(data_dst[,which(colnames(data_dst)==VARIABLES[k])]))
    }
    
    if(size==TRUE)
    {
      col=c(col,nrow(data_dst))
      
    }
    features=rbind(features,col)
    
    
    
  }
  features=data.frame(features,row.names=ONDS_names)
  
  
  
  
  for(k in 1:length(VARIABLES))
  {
    names(features)[k]=paste("MEAN",VARIABLES[k])  }
  
  if(size==TRUE)
  {
    names(features)[length(features)]="SIZE"  }
  
  
  return(features)
}

ONDS_names=unique(data_AF_itin$COD_CTY_DST)
list_ONDS = as.vector(ONDS_names)


ONDS_names_test=unique(data_test_itin$COD_CTY_DST)
list_ONDS_test = as.vector(ONDS_names_test)



VARIABLES=c("NBR_SEGTS","FREQUENCY","ELAPSE_TM","CNX_TM","NBR_BKG")


v1 <- VARIABLES

comb_variables=do.call("c", lapply(seq_along(v1), function(i) combn(v1, i, FUN = list)))


n_var=length(comb_variables)

clusters=2:10
clusters=2*clusters
n_clusters=length(clusters)


#matrice 
error_rel=matrix(, nrow = n_clusters, ncol = n_var)
  
  
for(c in clusters)
{

#  for(v in 6:n_var)
 # {
    
    library(clue) # for kmeans.predict (cl_predict)
    v=31
    k=c
     features_AF=featuring(data_AF_itin,ONDS_names,unlist(comb_variables[v]),TRUE) # on applique le featuring à data_AF
     features_AF=scale(features_AF)
  
    features_test=featuring(data_test_itin,ONDS_names_test,unlist(comb_variables[v]),TRUE) # on applique le featuring à data_test
    features_test=scale(features_test)
    model_clustering=kmeans(features_AF,k) # on applique le kmeans
    features_test_df <- data.frame(features_test)
    # features_test_df <- features_test_df[-6] #We remove economy ratio, as there is no economy ratio
    cl <- cl_predict(object = model_clustering, newdata = features_test_df)
    Matrix_test <- data.frame(row.names(features_test_df),as.integer(cl)) # on a le cluster auquel chaque OND des données test appartient
    Matrix_AF=data.frame(list_ONDS,model_clustering$cluster)
    pct_rel_error = matrix(, nrow = k, ncol = 2)
    
    for (j in 1:k) {  
      ## Number of the cluster in {1, .., k}
      # Pour chaque j in {1, .., k}
      
      M3=subset(Matrix_AF,Matrix_AF$model_clustering.cluster==j) 
      data_AF_cluster=subset(data_AF,data_AF$COD_CTY_DST %in% M3$list_ONDS)
      
      M4=subset(Matrix_test,Matrix_test$as.integer.cl.==j) 
      data_test_cluster=subset(data_test,data_test$COD_CTY_DST %in% M4$row.names.features_test_df.)
      
      train = data_AF_cluster
      test = data_test_cluster
      
      # Model with several variables  #
      model_regression <- glm(CHOICE~NBR_SEGTS + FREQUENCY + PCT_CPY_ORI 
                              + ELAPSE_TM + PREF_DEP_TM,
                              weights = WEIGHT, data=train, family = binomial("logit"))
      #summary(model_regression)
      ####################################################################
      
      itin_test <- create_itineraries(test)
      # predict FMS               #
      lin_pred<-predict(model_regression,itin_test)
      FMS<-(exp(lin_pred)/sum(exp(lin_pred)))
      nb_bkg_predict<-(exp(lin_pred)/sum(exp(lin_pred)))*sum(itin_test$NBR_BKG)
      
      
      rel_dif=(nb_bkg_predict-itin_test$NBR_BKG)/itin_test$NBR_BKG # écart relatif
      pct_rel_error[j, 1] = mean(sqrt(rel_dif*rel_dif))*100 # pct d'erreur en valeur absolue
      pct_rel_error[j, 2] = length(rel_dif)
      
     # print(j)
    }
    
    
    error_rel[k/2-3,v]=sum(pct_rel_error[,1]*pct_rel_error[,2])/sum(pct_rel_error[,2])
    
    print("cluster")
    print(k)
    print(v)
    
    
  }
  
#}






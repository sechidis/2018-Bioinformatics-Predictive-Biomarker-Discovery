###################################################################
########## Predictive Rankings ####################################
###################################################################


###################################################################
########## Output Categorical - Covariates Categorical ############
###################################################################
#### First Order Rankings - INFO ####
#####################################
 INFO.Output_Categorical.Covariates_Categorical <- function(data,labels,treatment){
  
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ### Calculate the I(Y;T|X) for each Xs 
  for (index_feature in 1:num_features){ 
      mi_scores[index_feature] =  condinformation_normalised(treatment,labels,data[,index_feature])# condinformation(treatment,labels,data[,index_feature],method="shrink")  
    }
  
  #### Prepare the return functions
  sorted_scores <- sort(mi_scores, decreasing=T,method='shell',index.return=TRUE) 
  ranking_scores <- sort(sorted_scores$ix, decreasing=F,method='shell',index.return=TRUE)
  results <- list("scores" = mi_scores, "ranking" = sorted_scores$ix, "ranking_scores" = ranking_scores$ix)
  
  return(results)
  
}
#######################################
#### Second order rankings - INFO+ ####
#######################################
 INFOplus.Output_Categorical.Covariates_Categorical <- function(data,labels,treatment, top_k){
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ranking_scores <- 	rep(0, num_features)
  ranking <- 	rep(0, num_features)
  selected_features <- 0   
  ### First Step: Select the first covariate (this is equivalent of using the  INFO and take the first features) 
  VT.First <-  INFO.Output_Categorical.Covariates_Categorical(data,labels,treatment)
  
  selected_features[1]<-VT.First$ranking[1]
  ranking_scores[selected_features[1]] <- 1
  mi_scores[selected_features[1]] <- VT.First$scores[VT.First$ranking[1]]
  
  #### Second Step: Iteretavely rank the features, by estimating the second order criterion for each one of them, and take the features with the highest score
  not_selected_features <- setdiff(1:num_features,selected_features)
  score_per_feature <- array(0,dim=c(1,num_features))
  
  score_per_feature[selected_features[1]]<-NA
  count_cmi <- num_features
  for (count in 2:top_k){
    ### Check the score of each feature not selected so far
    
    for (index_feature_ns in 1:length(not_selected_features)){
      ## To calculate this score we should calculate the conditional mutual information with the features selected
      conditioning_features <- do.call(interaction,data[,c(not_selected_features[index_feature_ns], selected_features[count-1])])
      score_per_feature[not_selected_features[index_feature_ns]] <-  score_per_feature[not_selected_features[index_feature_ns]] + condinformation_normalised(treatment,labels,conditioning_features)# condinformation(treatment,labels,conditioning_features,method="shrink") 
      count_cmi <- count_cmi+1
      
    }
    
    selected_features[count] <- which.max(score_per_feature) ### It ignores the NA, for that reason I check all of the features (the already selected they have score NA)
    ranking_scores[selected_features[count]] <- count
    
    mi_scores[selected_features[count]] <-  score_per_feature[selected_features[count]]
    score_per_feature[selected_features[count]]<-NA
    not_selected_features <- setdiff(1:num_features,selected_features)
    
  }
  
  ranking_scores[ranking_scores==0] <- (top_k+1):num_features
  results <- list("scores" = mi_scores, "ranking" = selected_features, "ranking_scores" = ranking_scores,"count_cmi" = count_cmi)
  
  return(results)
}

###################################################################
########## Output Categorical - Covariates Continuous# ############
###################################################################
#### First Order Rankings - INFO ####
#####################################
 INFO.Output_Categorical.Covariates_Continuous <-  function(data,labels,treatment){
  num_features <- ncol(data)
  ### First step - Discretization
  for (index_feature in 1:num_features){ 
    ### Use Scott's rule to discretize
    data[,index_feature] = discretize( data[,index_feature], disc="equalwidth", nbins=nclass.scott(data[,index_feature]))
  }
  
  ### Second step - Derive ranking, by normalising with the conditional entropy
  mi_scores <- 	rep(0, num_features)
  ### Calculate the I(Y;T|X) for each Xs 
  for (index_feature in 1:num_features){ 
    mi_scores[index_feature] =  condinformation_normalised(treatment,labels,data[,index_feature])  
  }
  
  #### Prepare the return functions
  sorted_scores <- sort(mi_scores, decreasing=T,method='shell',index.return=TRUE) 
  ranking_scores <- sort(sorted_scores$ix, decreasing=F,method='shell',index.return=TRUE)
  results <- list("scores" = mi_scores, "ranking" = sorted_scores$ix, "ranking_scores" = ranking_scores$ix)
  
  return(results)
}

#######################################
#### Second order rankings - INFO+ ####
#######################################
 INFOplus.Output_Categorical.Covariates_Continuous <-  function(data,labels,treatment, top_k){

  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ranking_scores <- 	rep(0, num_features)
  ranking <- 	rep(0, num_features)
  selected_features <- 0   
  
  ### First Step: Select the first covariate (this is equivalent of using the  INFO and take the first features) 
  VT.First <-  INFO.Output_Categorical.Covariates_Continuous(data,labels,treatment)
  
  selected_features[1]<-VT.First$ranking[1]
  ranking_scores[selected_features[1]] <- 1
  mi_scores[selected_features[1]] <- VT.First$scores[VT.First$ranking[1]]
 
  ### Discretization
   for (index_feature in 1:num_features){ 
     ### Use Scott's rule to discretize
     data[,index_feature] = discretize( data[,index_feature], disc="equalwidth", nbins=nclass.scott(data[,index_feature]))
    }
  

  #### Second Step: Iteratively rank the features, by estimating the second order criterion for each one of them, and take the features with the highest score
  not_selected_features <- setdiff(1:num_features,selected_features)
  score_per_feature <- array(0,dim=c(1,num_features))
  
  score_per_feature[selected_features[1]]<-NA
  count_cmi <- num_features
  for (count in 2:top_k){
    ### Check the score of each feature not selected so far
    
    for (index_feature_ns in 1:length(not_selected_features)){
      ## To calculate this score we should calculate the conditional mutual information with the features selected
      conditioning_features <- do.call(interaction,data[,c(not_selected_features[index_feature_ns], selected_features[count-1])])
      score_per_feature[not_selected_features[index_feature_ns]] <-  score_per_feature[not_selected_features[index_feature_ns]] + condinformation_normalised(treatment,labels,conditioning_features) 
      count_cmi <- count_cmi+1
      
    }
    
    selected_features[count] <- which.max(score_per_feature) ### It ignores the NA, for that reason I check all of the features (the already selected they have score NA)
    ranking_scores[selected_features[count]] <- count
    
    mi_scores[selected_features[count]] <-  score_per_feature[selected_features[count]]
    score_per_feature[selected_features[count]]<-NA
    not_selected_features <- setdiff(1:num_features,selected_features)
    
  }
  
  ranking_scores[ranking_scores==0] <- (top_k+1):num_features
  results <- list("scores" = mi_scores, "ranking" = selected_features, "ranking_scores" = ranking_scores,"count_cmi" = count_cmi)
  
  return(results)
}
# The normalized conditional mutual information with respect to conditional entropy
condinformation_normalised <- function(treatment, labels, features)
{
  
  cmi_normalised <- condinformation(treatment,labels,features,method="shrink")   / sqrt(condentropy(treatment, features, method="shrink")*condentropy(labels, features, method="shrink"))
  return(cmi_normalised)
}




###################################################################
########## Output Survival - Covariates Categorical ###############
###################################################################
#### Estimate conditional mutual informaiton with survival outputs
condinformation_survival_normalised <- function(treatment, labels, features, times, censor_groups){
  sample_size <- length(labels)
  time_disc <- sort(unique(times))
  
  # Follow SIDES approach to use an extra parameter 
  times_steps <-  seq(0, length(time_disc), length.out = censor_groups + 1)
  cmi_normalised<-integer(length(times_steps)-1)
  
  for (index_steps in 2:(length(times_steps))){ 
    
    labels_step <- integer(sample_size)
    labels_step[which(times<=time_disc[times_steps[index_steps]])] <-  labels[which(times<=time_disc[times_steps[index_steps]])]
    
    
    cmi_normalised[index_steps] <-  condinformation(treatment,labels_step,features,method="shrink")/sqrt(condentropy(treatment, features, method="shrink")*condentropy(labels_step, features, method="shrink"))
    
  }
  
  return( mean(cmi_normalised,na.rm=TRUE))  
  
}

#####################################
#### First Order Rankings - INFO ####
#####################################
 INFO.Output_Survival.Covariates_Categorical <- function(data,labels,treatment,times,censor_groups){
  
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ### Calculate the I(Y;T|X) for each Xs 
  for (index_feature in 1:num_features){ 
    mi_scores[index_feature] =  condinformation_survival_normalised(treatment,labels,data[,index_feature], times, censor_groups)  
  }
  
  #### Prepare the return functions
  sorted_scores <- sort(mi_scores, decreasing=T,method='shell',index.return=TRUE) 
  ranking_scores <- sort(sorted_scores$ix, decreasing=F,method='shell',index.return=TRUE)
  results <- list("scores" = mi_scores, "ranking" = sorted_scores$ix, "ranking_scores" = ranking_scores$ix)
  
  return(results)
  
}

#######################################
#### Second order rankings - INFO+ ####
#######################################
 INFOplus.Output_Survival.Covariates_Categorical <- function(data,labels, treatment, times, censor_groups, top_k){
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ranking_scores <- 	rep(0, num_features)
  ranking <- 	rep(0, num_features)
  selected_features <- 0   
  ### First Step: Select the first covariate (this is equivalent of using the  INFO and take the first features) 
  VT.First <-  INFO.Output_Survival.Covariates_Categorical (data,labels,treatment,times, censor_groups)
  
  selected_features[1]<-VT.First$ranking[1]
  ranking_scores[selected_features[1]] <- 1
  mi_scores[selected_features[1]] <- VT.First$scores[VT.First$ranking[1]]
  
  #### Second Step: Iteratively rank the features, by estimating the second order criterion for each one of them, and take the features with the highest score
  not_selected_features <- setdiff(1:num_features,selected_features)
  score_per_feature <- array(0,dim=c(1,num_features))
  
  score_per_feature[selected_features[1]]<-NA
  count_cmi <- num_features
  for (count in 2:top_k){
    ### Check the score of each feature not selected so far
    
    for (index_feature_ns in 1:length(not_selected_features)){
      ## To calculate this score we should calculate the conditional mutual information with the features selected
      conditioning_features <- do.call(interaction,data[,c(not_selected_features[index_feature_ns], selected_features[count-1])])
      score_per_feature[not_selected_features[index_feature_ns]] <-  score_per_feature[not_selected_features[index_feature_ns]] + condinformation_survival_normalised(treatment,labels,conditioning_features,times, censor_groups) #/  condentropy(treatment + 2*labels, data[,not_selected_features[index_feature_ns]], method="shrink")
      count_cmi <- count_cmi+1
      
    }
    
    selected_features[count] <- which.max(score_per_feature) ### It ignores the NA, for that reason I check all of the features (the already selected they have score NA)
    ranking_scores[selected_features[count]] <- count
    
    mi_scores[selected_features[count]] <-  score_per_feature[selected_features[count]]
    score_per_feature[selected_features[count]]<-NA
    not_selected_features <- setdiff(1:num_features,selected_features)
    
  }
  
  ranking_scores[ranking_scores==0] <- (top_k+1):num_features
  results <- list("scores" = mi_scores, "ranking" = selected_features, "ranking_scores" = ranking_scores,"count_cmi" = count_cmi)
  
  return(results)
}

###################################################################
########## Output Survival - Covariates Continuous ################
###################################################################
#### First order rankings - INFO ####
#####################################
 INFO.Output_Survival.Covariates_Continuous <-  function(data, labels, treatment, times, censor_groups){
  ### First step - Discretization
  for (index_feature in 1:num_features){ 
    ### Use Scott's rule to discretize
    data[,index_feature] = discretize( data[,index_feature], disc="equalwidth", nbins=nclass.scott(data[,index_feature]))
  }
  
  ### Second step - Derive ranking, by normalizing with the conditional entropy
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ### Calculate the I(Y;T|X) for each Xs 
  for (index_feature in 1:num_features){ 
    mi_scores[index_feature] =    condinformation_survival_normalised(treatment,labels,data[,index_feature], times, censor_groups) 
  }

  #### Prepare the return functions
  sorted_scores <- sort(mi_scores, decreasing=T,method='shell',index.return=TRUE) 
  ranking_scores <- sort(sorted_scores$ix, decreasing=F,method='shell',index.return=TRUE)
  results <- list("scores" = mi_scores, "ranking" = sorted_scores$ix, "ranking_scores" = ranking_scores$ix)
  
  return(results)
}

#######################################
#### Second order rankings - INFO+ ####
#######################################
 INFOplus.Output_Survival.Covariates_Continuous <-  function(data, labels, treatment, times, censor_groups, top_k){
  ### First step - Discretization
  for (index_feature in 1:num_features){ 
    ### Use Scott's rule to discretize
	
    data[,index_feature] = discretize( data[,index_feature], disc="equalwidth", nbins=nclass.scott(data[,index_feature]))
  }
  
  ### Second step - Derive ranking, by normalising with the conditional entropy
  num_features <- ncol(data)
  mi_scores <- 	rep(0, num_features)
  ranking_scores <- 	rep(0, num_features)
  ranking <- 	rep(0, num_features)
  selected_features <- 0   
  ### First Step: Select the first covariate (this is equivalent of using the  INFO and take the first features) 
  VT.First <-  INFO.Output_Survival.Covariates_Continuous(data,labels,treatment, times, censor_groups)
  
  selected_features[1]<-VT.First$ranking[1]
  ranking_scores[selected_features[1]] <- 1
  mi_scores[selected_features[1]] <- VT.First$scores[VT.First$ranking[1]]
  
  #### Second Step: Iteretavely rank the features, by estimating the second order criterion for each one of them, and take the features with the highest score
  not_selected_features <- setdiff(1:num_features,selected_features)
  score_per_feature <- array(0,dim=c(1,num_features))
  
  score_per_feature[selected_features[1]]<-NA
  count_cmi <- num_features
  for (count in 2:top_k){
    ### Check the score of each feature not selected so far
    
    for (index_feature_ns in 1:length(not_selected_features)){
      ## To calculate this score we should calculate the conditional mutual information with the features selected
      conditioning_features <- do.call(interaction,data[,c(not_selected_features[index_feature_ns], selected_features[count-1])])
      score_per_feature[not_selected_features[index_feature_ns]] <-  score_per_feature[not_selected_features[index_feature_ns]] + condinformation_survival_normalised(treatment, labels, conditioning_features, times, censor_groups) 
      count_cmi <- count_cmi+1
      
    }
    
    selected_features[count] <- which.max(score_per_feature) ### It ignores the NA, for that reason I check all of the features (the already selected they have score NA)
    ranking_scores[selected_features[count]] <- count
    
    mi_scores[selected_features[count]] <-  score_per_feature[selected_features[count]]
    score_per_feature[selected_features[count]]<-NA
    not_selected_features <- setdiff(1:num_features,selected_features)
    
  }
  
  ranking_scores[ranking_scores==0] <- (top_k+1):num_features
  results <- list("scores" = mi_scores, "ranking" = selected_features, "ranking_scores" = ranking_scores,"count_cmi" = count_cmi)
  
  return(results)
}





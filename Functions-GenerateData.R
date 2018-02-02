Generate.Data <- function(sample_size,num_features,theta_pred,model){
  
  
  ####
  #### To generate the treatment
  ####
  treatment <- rbinom(sample_size,1,0.5)
  
  
  ####
  #### To generate the labels I need to create for each model the logistic regression function
  ####
  switch(model,
         { #### Model 1
           
           sigma <- diag(num_features)
           correl <-0
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)

  
           prog_part <- apply(covariates[,1:5],1,sum); # prog_part <- prog_part - mean(prog_part);
           pred_part <-  apply(covariates[,4:8],1,sum);# pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part + (treatment+0.1) * theta_pred * pred_part ;
           prog_features <- 1:5; pred_features <- 4:8;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
           covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },
         { #### Model 2
           
           sigma <- diag(num_features)
           correl <-0
           sigma[seq(1,15,by=2),seq(1,15,by=2)] <-correl # correlation between odds features
           sigma[seq(2,15,by=2),seq(2,15,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)

           prog_part <- apply(covariates[,1:5],1,sum); #  prog_part <- prog_part - mean(prog_part);
           pred_part <- apply(covariates[,6:10],1,sum);#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part + (treatment+0)*theta_pred * pred_part ;
           prog_features <- 1:5; pred_features <- 6:10;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },   
         { #### Model 3
           
           sigma <- diag(num_features)
           correl <-0.70
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)

           

           prog_part <- apply(covariates[,1:5],1,sum); #  prog_part <- prog_part - mean(prog_part);
           pred_part <- apply(covariates[,6:10],1,sum);#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <- prog_part + (treatment+0)*theta_pred * pred_part ;
           prog_features <- 1:5; pred_features <- 6:10;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },                  
         { #### Model 4
           
           sigma <- diag(num_features)
           correl <-0.70
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)

  
           prog_part <- covariates[,1] + apply(covariates[,c(2,3)],1,prod) + apply(covariates[,c(4,5)],1,prod) ;#  prog_part <- prog_part - mean(prog_part);
           pred_part <-   ( covariates[,6] + apply(covariates[,c(7,8)],1,prod) + apply(covariates[,c(9,10)],1,prod) );#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part +  treatment * theta_pred * pred_part;
           prog_features <- 1:5; pred_features <- 6:10;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },
         
         
         { #### Model 5
           sigma <- diag(num_features)
           correl <-0.70
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)
           prog_part <-  covariates[,1]*(covariates[,2]*covariates[,3]-covariates[,4]^2) ;#   prog_part <- prog_part - mean(prog_part);
           pred_part <-   covariates[,5]*exp(covariates[,6]*covariates[,7]-covariates[,8]^2)  ;#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part +  treatment*theta_pred * pred_part;
           prog_features <- 1:4; pred_features <- 5:8;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },
         
         { #### Model  6
           sigma <- diag(num_features)
           correl <-0.70
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)
  
           prog_part <-  covariates[,1] + (covariates[,2]>-0.545) * (covariates[,3]<0.545) ;#  prog_part <- prog_part - mean(prog_part);
           pred_part <-  covariates[,4] + (covariates[,5]>-0.545) * (covariates[,6]<0.545) ;#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part +  treatment*theta_pred * pred_part;
           prog_features <- 1:3; pred_features <- 4:6;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
           
         },
         { #### Model  7
           sigma <- diag(num_features)
           correl <-0.70
           sigma[seq(1,num_features,by=2),seq(1,num_features,by=2)] <-correl # correlation between odds features
           sigma[seq(2,num_features,by=2),seq(2,num_features,by=2)] <-correl # correlation between even features
           diag(sigma) <- 1
           covariates <- mvrnorm(sample_size, rep(0, num_features), sigma) # I need install.packages("MASS") and library(MASS)
           prog_part <-  covariates[,1] + (covariates[,2]>-0.545) * (covariates[,3]<0.545)*(covariates[,4]>0);#   prog_part <- prog_part - mean(prog_part);
           pred_part <-  covariates[,5] + (covariates[,6]>-0.545) * (covariates[,7]<0.545)*(covariates[,8]>0);#  pred_part <- pred_part - mean(pred_part);
           logit_pY1 <-  prog_part +  treatment*theta_pred * pred_part;
           prog_features <- 1:4; pred_features <- 5:8;
           irr_features <-setdiff(seq(1,num_features,by=1),c(prog_features,pred_features))

           for (index_feature in 1:num_features){ 
             covariates[,index_feature] = t(discretize( covariates[,index_feature], disc="equalwidth", nbins= sample(c(2,3,4,5),1)))
           }
           
           covariates <- as.data.frame.matrix(covariates)
         },
         
         
    
         stop("Wrong model")
  )
  
  
  
  pY1 <- 1/(1+exp(-logit_pY1))
  labels <-  rbinom(sample_size,1,pY1);
  
  

  synthetic_datasets <- numeric(0)
  synthetic_datasets$data <- covariates
  synthetic_datasets$treatment <- treatment
  synthetic_datasets$labels <- labels
  synthetic_datasets$prog_features <- prog_features
  synthetic_datasets$pred_features <- pred_features  
  synthetic_datasets$irr_features <- irr_features

  return(synthetic_datasets)
  
}





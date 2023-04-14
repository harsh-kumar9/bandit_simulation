source("Contextual configration.R")


######################################################################
##############   everything that needs to be defined, begin  ############################
n <- 1000 # or 100 #number of participants
burnin <- n#burnin=1 means we use TS, or set burnin =n, if use UR

#Setting 1 ############
true_reward <- 'mu0'
reward_model <- 'b0+b1*a1+b2*c1+b3*a1*c1'
mu <- c(0.5) #true value of mu_i in true_reward 
########################

#Setting 2 ############
true_reward <- 'mu0+mu1*a1'
reward_model <- 'b0+b1*a1+b2*c1+b3*a1*c1'
mu <- c(0.5,1/8) #true value of mu_i in true_reward 
########################

#Setting 3 ############
true_reward <- 'mu0+mu1*a1+mu2*c1+mu3*a1*c1'
reward_model <- 'b0+b1*a1+b2*c1+b3*a1*c1'
mu <- c(0.5,3/8,-1/4,-5/8) #true value of mu_i in true_reward 
########################


####### settings needed for all 3 cases ############
sig_err <- 1/36 #variance of reward error
reward_setting <- list(mu=mu,sig_err=sig_err)
reward_setting$reward_generation <- function(mean_reward){
  y <- rnorm(1,mean=mean_reward,sd=sqrt(sig_err))
  y <- median(c(0,1,round(y*4)/4))
  return(y)
}
k <- str_count(reward_model,'b') #by default the dim 'k' is the number of 'b_i' in reward model
para_priors <- list(a=2,b=1,mu=rep(0,k),L=0.01*diag(k))
process_reward_model(true_reward,reward_model,name_list)
########################


#### start simulation setting
bb <- 300 #number of simulations
bayesian_test <- array(dim=c(bb,k)) #array to save results
rewards <- array(dim=c(bb,3))
for (i in 1:bb){
  set.seed(i)
  #n sample size
  #k number of betas
  res <- Contextual_Bandit(para_priors,reward_setting,n=n,contextual_matrix,epsilon=0,burnin=burnin,batch_size=1)
  process_result(res)
  res1 <- process_result(res)
  rewards[i,] <- res1$reward
  bayesian_test[i,] <- res1$CI[1,]*res1$CI[3,]>0
}
summary(lm(data=data.frame(res$his),reward~a1*c1))
apply(rewards,2,mean)
apply(bayesian_test,2,mean) #calculate power on: intercept/Rationale/Mood/Rationale*Mood


#FMS MODEL


model{
  #Likelihood
  for(i in 1:nobs) {
    #Model prediction for how long fish was 1 month after fish capture
    L1_mod[i,t1[i]]<- L1_obs[i]
    SL1_mod[i,t1[i]] <- (L1_mod[i,t1[i]]-350)/110
    
    
    #Iterate through months to make predictions
    for(j in (t1[i]+1):t2[i]) {
      #Predict growth - LIKELIHOOD
      deltaL_mon[i,j]<- pred_int[reach[i],(j-1)] + (pred_slope[reach[i],(j-1)])*SL1_mod[i,(j-1)]
      #define L1_mod
      L1_mod[i,j]<-L1_mod[i,j-1]+deltaL_mon[i,j]
      #Standardize L1
      SL1_mod[i,j] <- (L1_mod[i,j]-350)/110
    }
    
    #LIKELIHOOD: How long fish will be at second recapture, comparing to data
    L2[i]~dt(L1_mod[i,t2[i]],L_obs_err_tau[i],1)
    L_obs_err_tau[i] <- pow(L_obs_err_var[i],-1)
    L_obs_err_var[i] <- L_obs_var + dT2[i]*L_proc_var
  }	
  
  
  
  #Constrain error- if fish is caught twice in same trip, there is no growth, but here you can get an independent measurement of observation error
  #Fit distribution for observation error - difference between two observations within a trip for entire dataset
  for(i in 1:nobs_withintrip) {
    deltaL_obs_withintrip[i]~dt(0,L_obs_err_tau_within,1)
  }
  
  #Priors and Transformation of Priors
  #Will want to monitor these
  beta_temp ~ dnorm(0,tau_temp)
  beta_turb ~ dnorm(0,tau_turb)
  beta_temp2 ~ dnorm(0,tau_temp2)
  beta_tempQ ~ dnorm(0,tau_tempQ)
  beta_temp2Q ~ dnorm(0,tau_temp2Q)
  beta_turb2 ~ dnorm(0,tau_turb2)
  beta_gpp ~ dnorm(0,tau_gpp)
  beta_gpp2 ~ dnorm(0,tau_gpp2)
 
  
  p_temp ~ dbern(0.5)
  p_turb ~ dbern(0.5)
  p_temp2 ~ dbern(0.5)
  p_tempQ ~ dbern(0.5)
  p_temp2Q ~ dbern(0.5)
  p_turb2 ~ dbern(0.5)
  p_gpp ~ dbern(0.5)
  p_gpp2 ~ dbern(0.5)
  
  tau_temp <-p_temp*0.25 + (1-p_temp) * 10000
  tau_turb <-p_turb*0.25 + (1-p_turb) * 10000
  tau_temp2 <-p_temp2*0.25 + (1-p_temp2) * 10000
  tau_tempQ <-p_tempQ*0.25 + (1-p_tempQ) * 10000
  tau_temp2Q <-p_temp2Q*0.25 + (1-p_temp2Q) * 10000
  tau_turb2 <-p_turb2*0.25 + (1-p_turb2) * 10000
  tau_gpp <-p_gpp*0.25 + (1-p_gpp) * 10000
  tau_gpp2 <-p_gpp2*0.25 + (1-p_gpp2) * 10000
  
  for(i in 1:nreach) {
  beta0[i]~ dnorm(0,0.01)
  beta_Li[i] ~ dnorm(0,0.01)

  }
  
  #Measurement Error
  L_obs_err_tau_within<-pow(L_obs_sig, -2)
  L_obs_sig~dunif(0, 5)
  L_obs_var <- pow(L_obs_sig, 2)
  #Process Error
  L_proc_sig~dunif(0, 5)
  L_proc_var <- pow(L_proc_sig, 2)
  
  #reach and time random effect
  for(i in 1:nreach) {
    for(j in 1:nmonth){
      eps[i,j] ~ dnorm(0,eps_tau)
      eps2[i,j] ~ dnorm(0,eps_tau2)
      
      #r2
      pred_int [i,j] <- beta0[i] + beta_temp* temp[i,j] + beta_tempQ* tempQ[i,j]+ beta_turb* turb[i,j] + beta_gpp* GPP[i,j] + eps[i,j]
      pred_slope[i,j] <- beta_Li[i] + beta_temp2* temp[i,j] + beta_temp2Q* tempQ[i,j]+beta_turb2* turb[i,j] + beta_gpp2 * GPP[i,j] + eps2[i,j]
    }
  }
  eps_tau<- pow(eps_sig,-2)
  eps_tau2<- pow(eps_sig2,-2)
  eps_sig ~ dunif(0,5)
  eps_sig2 ~ dunif(0,5)
  
  #Define R2 
  sd_eps <- sd(eps)
  sd_eps2 <- sd(eps2)
  sd_int <- sd(pred_int)
  sd_slope <-sd(pred_slope)
  
  var_eps <- pow(sd_eps,2)
  var_eps2 <- pow(sd_eps2,2)
  var_int <- pow(sd_int,2)
  var_slope <- pow(sd_slope,2)
}

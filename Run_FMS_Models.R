library(rjags)
library(R2jags)
library(mcmcplots)
library(dplyr)
library(standardize)

setwd("~/LH Growth Models/FMS")







############################################################################################################################################
### ORGANIZE DATA      -      TRUNCATED DATA  ###
  #ALL THESE DATA BEGIN IN 2012, WHEN GAGE DATA FOR TURBIDITY AND GPP ARE AVAILABLE
  #AND exclude FMS from GCD reach 


# load data
fmsbayesian_Trunc2<-read.csv("fmsbayesian2_Trunc3.csv")
hist(fmsbayesian_Trunc2$Length.1, breaks = 20)
str(fmsbayesian_Trunc2)

Temp_Matrix_Trunc2<-read.csv("Temp_AveragedbetweenReaches_Trunc2.csv") #For regular Temp
Temp_Matrix_Trunc2Q<-read.csv("Temp_AveragedbetweenReaches_Trunc2.csv") #For Quadratic
GPP_Matrix_Trunc2<-read.csv("GPP_Trunc2.csv")
GPP_Matrix_Trunc3<-read.csv("GPP_Trunc3.csv")
Turb_Matrix_Threshstan<-read.csv("Turb_Threshold_Matrix2.csv") #Turb days over 50 FNU


#Isolate within trip captures
#combine PITTAG and Trip ID into one column (for each Trip 1 and 2)
fmsbayesian_Trunc2$PitTrip1 <- paste(fmsbayesian_Trunc2$PITTAG,fmsbayesian_Trunc2$Trip_ID.1)
fmsbayesian_Trunc2$PitTrip2 <- paste(fmsbayesian_Trunc2$PITTAG,fmsbayesian_Trunc2$Trip_ID.2)

#Create matrix with within trip recaps only
withintriprecaps_Trunc2 <- filter(fmsdata_Trunc2b,PitTrip1 == PitTrip2)
#Create matrix with NO within trip recaps
fmsdata_Trunc2 <- filter(fmsdata_Trunc2b,PitTrip1 != PitTrip2)


#Standardize Temp
head(Temp_Matrix_Trunc2)
Temp_Matrix_Trunc2$X1_Glen.Canyon.Dam.Gage <- (Temp_Matrix_Trunc2$X1_Glen.Canyon.Dam.Gage - 12.48) / 2.29
Temp_Matrix_Trunc2$X2_Lees.Ferry.Gage <- (Temp_Matrix_Trunc2$X2_Lees.Ferry.Gage - 12.48) / 2.29
Temp_Matrix_Trunc2$X3_Little.Colorado.River.Gage <- (Temp_Matrix_Trunc2$X3_Little.Colorado.River.Gage - 12.48) / 2.29
Temp_Matrix_Trunc2$X4_Phantom.Ranch.Gage <- (Temp_Matrix_Trunc2$X4_Phantom.Ranch.Gage - 12.48) / 2.29
Temp_Matrix_Trunc2$X5_National.Gage <- (Temp_Matrix_Trunc2$X5_National.Gage - 12.48) / 2.29
Temp_Matrix_Trunc2$X6_Diamond.Gage <- (Temp_Matrix_Trunc2$X6_Diamond.Gage - 12.48) / 2.29
#Reshape
Temp_Matrix2b=t(Temp_Matrix_Trunc2)

#Standardize GPP
summary(GPP_Matrix_Trunc2)
GPP_Matrix_Trunc2$X1_Glen.Canyon.Dam.Gage <- (GPP_Matrix_Trunc2$X1_Glen.Canyon.Dam.Gage - 1.71) / 1.28
GPP_Matrix_Trunc2$X2_Lees.Ferry.Gage <- (GPP_Matrix_Trunc2$X2_Lees.Ferry.Gage - 1.71) / 1.28
GPP_Matrix_Trunc2$X3_Little.Colorado.River.Gage <- (GPP_Matrix_Trunc2$X3_Little.Colorado.River.Gage - 1.71) / 1.28
GPP_Matrix_Trunc2$X4_Phantom.Ranch.Gage <- (GPP_Matrix_Trunc2$X4_Phantom.Ranch.Gage - 1.71) / 1.28
GPP_Matrix_Trunc2$X5_National.Gage <- (GPP_Matrix_Trunc2$X5_National.Gage - 1.71) / 1.28
GPP_Matrix_Trunc2$X6_Diamond.Gage <- (GPP_Matrix_Trunc2$X6_Diamond.Gage - 1.71) / 1.28
#Reshape
GPP_Matrix2=t(GPP_Matrix_Trunc2)


#Standardize Turbidity
Turb_Matrix_Threshstan$X1_Glen.Canyon.Dam.Gage <- (Turb_Matrix_Threshstan$X1_Glen.Canyon.Dam.Gage - 0.35) / .35
Turb_Matrix_Threshstan$X2_Lees.Ferry.Gage <- (Turb_Matrix_Threshstan$X2_Lees.Ferry.Gage - 0.35) / .35
Turb_Matrix_Threshstan$X3_Little.Colorado.River.Gage <- (Turb_Matrix_Threshstan$X3_Little.Colorado.River.Gage - 0.35) / .35
Turb_Matrix_Threshstan$X4_Phantom.Ranch.Gage <- (Turb_Matrix_Threshstan$X4_Phantom.Ranch.Gage - 0.35) / .35
Turb_Matrix_Threshstan$X5_National.Gage <- (Turb_Matrix_Threshstan$X5_National.Gage - 0.35) / .35
Turb_Matrix_Threshstan$X6_Diamond_Gage <- (Turb_Matrix_Threshstan$X6_Diamond_Gage - 0.35) / .35
#Reshape
Turb_Matrix_Thresh2stan=t(Turb_Matrix_Threshstan)


#Standardize Temp Quadratic
Temp_Matrix_Trunc2Q$X1_Glen.Canyon.Dam.Gage <- ((Temp_Matrix_Trunc2Q$X1_Glen.Canyon.Dam.Gage - 12.48) / 2.29)^2
Temp_Matrix_Trunc2Q$X2_Lees.Ferry.Gage <- ((Temp_Matrix_Trunc2Q$X2_Lees.Ferry.Gage - 12.48) / 2.29)^2
Temp_Matrix_Trunc2Q$X3_Little.Colorado.River.Gage <- ((Temp_Matrix_Trunc2Q$X3_Little.Colorado.River.Gage - 12.48) / 2.29)^2
Temp_Matrix_Trunc2Q$X4_Phantom.Ranch.Gage <- ((Temp_Matrix_Trunc2Q$X4_Phantom.Ranch.Gage - 12.48) / 2.29)^2
Temp_Matrix_Trunc2Q$X5_National.Gage <- ((Temp_Matrix_Trunc2Q$X5_National.Gage - 12.48) / 2.29)^2
Temp_Matrix_Trunc2Q$X6_Diamond.Gage <- ((Temp_Matrix_Trunc2Q$X6_Diamond.Gage - 12.48) / 2.29)^2
#Reshape
Temp_Matrix2bQ=t(Temp_Matrix_Trunc2Q)

#Standardize GPP 3
summary(GPP_Matrix_Trunc3)
GPP_Matrix_Trunc3$X1_Glen.Canyon.Dam.Gage <- (GPP_Matrix_Trunc3$X1_Glen.Canyon.Dam.Gage - 1.71) / 1.28
GPP_Matrix_Trunc3$X2_Lees.Ferry.Gage <- (GPP_Matrix_Trunc3$X2_Lees.Ferry.Gage - 1.71) / 1.28
GPP_Matrix_Trunc3$X3_Little.Colorado.River.Gage <- (GPP_Matrix_Trunc3$X3_Little.Colorado.River.Gage - 1.71) / 1.28
GPP_Matrix_Trunc3$X4_Phantom.Ranch.Gage <- (GPP_Matrix_Trunc3$X4_Phantom.Ranch.Gage - 1.71) / 1.28
GPP_Matrix_Trunc3$X5_National.Gage <- (GPP_Matrix_Trunc3$X5_National.Gage - 1.71) / 1.28
GPP_Matrix_Trunc3$X6_Diamond.Gage <- (GPP_Matrix_Trunc3$X6_Diamond.Gage - 1.71) / 1.28
#Reshape
GPP_Matrix3=t(GPP_Matrix_Trunc3)


dattrunc <- list(L2=as.vector(fmsdata_Trunc2$Length.2), L1_obs=as.vector(fmsdata_Trunc2$Length.1),  nobs=length(fmsdata_Trunc2$PITTAG), month=as.vector(fmsdata_Trunc2$Delta_T),nt=79,
             nobs_withintrip=length(withintriprecaps_Trunc2$PITTAG), L1_rep_wtrip=as.vector(withintriprecaps_Trunc2$Length.1), L2_rep_wtrip=as.vector(withintriprecaps_Trunc2$Length.2),
             t1=fmsdata_Trunc2$Month.1, t2=fmsdata_Trunc2$Month.2, temp = as.matrix(Temp_Matrix2b[4:7,]), tempQ = as.matrix(Temp_Matrix2bQ[4:7,]),  turb = as.matrix(Turb_Matrix_Thresh2stan[5:8,]), GPP = as.matrix(GPP_Matrix2[4:7,]),
             dT=as.vector(fmsdata_Trunc2$Delta_T), dT_wtrip=as.vector(withintriprecaps_Trunc2$Delta_T), dT2 = as.vector(fmsdata_Trunc2$Delta_T_Squared), nreach=4, nmonth=79, reach=as.vector(fmsdata_Trunc2$Reach.2),
             L2_oos=as.vector(outofsample2$Length.2), L1_obs_oos=as.vector(outofsample2$Length.1),  nobs_oos=length(outofsample2$PITTAG),
             t1_oos=outofsample2$Month.1, t2_oos=outofsample2$Month.2, reach_oos=as.vector(outofsample2$Reach.2), dT2_oos= as.vector(outofsample2$Delta_T_Squared),
             pi=3.14159)





#########################################################################################################################################################
# SLAB AND SPIKE Test Set Up#

# load data
fmsbayesian_Trunc2<-read.csv("fmsbayesian2_Trunc3.csv")

#Isolate within trip captures
#combine PITTAG and Trip ID into one column (for each Trip 1 and 2)
fmsbayesian_Trunc2$PitTrip1 <- paste(fmsbayesian_Trunc2$PITTAG,fmsbayesian_Trunc2$Trip_ID.1)
fmsbayesian_Trunc2$PitTrip2 <- paste(fmsbayesian_Trunc2$PITTAG,fmsbayesian_Trunc2$Trip_ID.2)

#Create matrix with within trip recaps only
withintriprecaps_Trunc_slabspike <- filter(fmsbayesian_Trunc2,PitTrip1 == PitTrip2)
#Create matrix with NO within trip recaps
fmsdata_Trunc_slabspike <- filter(fmsbayesian_Trunc2,PitTrip1 != PitTrip2)

dattrunc2 <- list(L2=as.vector(fmsdata_Trunc_slabspike$Length.2), L1_obs=as.vector(fmsdata_Trunc_slabspike$Length.1),  nobs=length(fmsdata_Trunc_slabspike$PITTAG), month=as.vector(fmsdata_Trunc_slabspike$Delta_T),nt=79,
                 nobs_withintrip=length(withintriprecaps_Trunc_slabspike$PITTAG), L1_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.1), L2_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.2),
                 t1=fmsdata_Trunc_slabspike$Month.1, t2=fmsdata_Trunc_slabspike$Month.2, temp = as.matrix(Temp_Matrix2b[4:7,]), tempQ = as.matrix(Temp_Matrix2bQ[4:7,]),  turb = as.matrix(Turb_Matrix_Thresh2stan[5:8,]), GPP = as.matrix(GPP_Matrix2[4:7,]),
                 dT=as.vector(fmsdata_Trunc_slabspike$Delta_T), dT_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Delta_T), dT2 = as.vector(fmsdata_Trunc_slabspike$Delta_T_Squared), nreach=4, nmonth=79, reach=as.vector(fmsdata_Trunc_slabspike$Reach.2)
                 )


dattrunc3 <- list(L2=as.vector(fmsdata_Trunc_slabspike$Length.2), L1_obs=as.vector(fmsdata_Trunc_slabspike$Length.1),  nobs=length(fmsdata_Trunc_slabspike$PITTAG), month=as.vector(fmsdata_Trunc_slabspike$Delta_T),nt=79,
                  nobs_withintrip=length(withintriprecaps_Trunc_slabspike$PITTAG), L1_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.1), L2_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.2),
                  t1=fmsdata_Trunc_slabspike$Month.1, t2=fmsdata_Trunc_slabspike$Month.2, temp = as.matrix(Temp_Matrix2b[4:7,]), tempQ = as.matrix(Temp_Matrix2bQ[4:7,]),  turb = as.matrix(Turb_Matrix_Thresh2stan[5:8,]), GPP = as.matrix(GPP_Matrix3[4:7,]),
                  dT=as.vector(fmsdata_Trunc_slabspike$Delta_T), dT_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Delta_T), dT2 = as.vector(fmsdata_Trunc_slabspike$Delta_T_Squared), nreach=4, nmonth=79, reach=as.vector(fmsdata_Trunc_slabspike$Reach.2)
)

dattrunc4 <- list(L2=as.vector(fmsdata_Trunc_slabspike$Length.2), L1_obs=as.vector(fmsdata_Trunc_slabspike$Length.1),  nobs=length(fmsdata_Trunc_slabspike$PITTAG), month=as.vector(fmsdata_Trunc_slabspike$Delta_T),nt=79,
                  nobs_withintrip=length(withintriprecaps_Trunc_slabspike$PITTAG), L1_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.1), L2_rep_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Length.2),
                  t1=fmsdata_Trunc_slabspike$Month.1, t2=fmsdata_Trunc_slabspike$Month.2, temp = as.matrix(Temp_Matrix2b[4:7,]), tempQ = as.matrix(Temp_Matrix2bQ[4:7,]),  turb = as.matrix(Turb_Matrix_Thresh2stan[5:8,]), GPP = as.matrix(GPP_Matrix2[5:8,]),
                  dT=as.vector(fmsdata_Trunc_slabspike$Delta_T), dT_wtrip=as.vector(withintriprecaps_Trunc_slabspike$Delta_T), dT2 = as.vector(fmsdata_Trunc_slabspike$Delta_T_Squared), nreach=4, nmonth=79, reach=as.vector(fmsdata_Trunc_slabspike$Reach.2)
)


#monitor betas once covariates added to model
jags.params<- c("beta0","beta_Li", "beta_temp","beta_temp2", "beta_turb", "beta_turb2", "beta_gpp","beta_gpp2", "beta_tempQ","beta_temp2Q",
                       "p_temp","p_temp2", "p_turb", "p_turb2", "p_gpp","p_gpp2", "p_tempQ","p_temp2Q", "pred_int", "pred_slope",
                       "deviance", "L_obs_sig", "L_proc_sig", "eps", "eps2", "eps_sig", "eps_sig2", " L_obs_err_tau_within",
                       "var_eps", "var_eps2", "var_int", "var_slope")

########################################################################################################

### LOG Version of MODELS INTS ONLY ####
#length + temp + tempQ + turb + GPP[i], SLAB AND SPIKE TEST



# Initialize model
start_time <- Sys.time()
model_log_nogppxreach<-jags.parallel(model.file="log_model_slabandspiketest_nogppxreach.R", data=dattrunc2,  inits=NULL, n.chains = 3, jags.params, n.iter=40000, n.thin=10)
end_time <- Sys.time()
end_time - start_time

fms_log_output <-model_log_nogppxreach$BUGSoutput$summary

save.image(file='FMS_Final_Models_AdjustedforGPP.RData')


print(model_log_nogppxreach, pars = c('beta', 'p'), digits=2, prob=c(.025, .5, .975))




plot(model_log_nogppxreach$BUGSoutput$summary[seq(27,343,1),1], main="Residuals for epsilons") #All EPS

par(mfrow=c(2,2))
plot(model_log_nogppxreach$BUGSoutput$summary[seq(27,343,4),1], main="Residuals for Lee's Ferry Reach") #Lees Ferry
plot(model_log_nogppxreach$BUGSoutput$summary[seq(28,344,4),1], main="Residuals for Little Colorado River Reach") #LCR
plot(model_log_nogppxreach$BUGSoutput$summary[seq(29,345,4),1], main="Residuals for Bright Angel Creek Reach") #Phantom
plot(model_log_nogppxreach$BUGSoutput$summary[seq(30,346,4),1], main="Residuals for National Creek Reach") #National

traceplot(model_log_nogppxreach,varname='beta_temp')



########################################################################################################

### LINEAR version of LENGTH MODELS INTS ONLY ####
#LINEAR length + temp + tempQ + turb + GPP, SLAB AND SPIKE TEST



# Initialize model
start_time <- Sys.time()
model_linear_nogppxreach_original<-jags.parallel(model.file="linear_model_slabandspiketest_nogppxreach.R", data=dattrunc2,  inits=NULL, n.chains = 3, jags.params, n.iter=50000, n.thin=10)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
model_linear_nogppxreach_nobac<-jags.parallel(model.file="linear_model_slabandspiketest_nogppxreach.R", data=dattrunc3,  inits=NULL, n.chains = 3, jags.params, n.iter=50000, n.thin=10)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
model_linear_nogppxreach_withbac<-jags.parallel(model.file="linear_model_slabandspiketest_nogppxreach.R", data=dattrunc4,  inits=NULL, n.chains = 3, jags.params, n.iter=100000, n.thin=10)
end_time <- Sys.time()
end_time - start_time

fms_linear_output<-model_linear_nogppxreach_original$BUGSoutput$summary
fms_linear_output_nobac<-model_linear_nogppxreach_nobac$BUGSoutput$summary
fms_linear_output_withbac<-model_linear_nogppxreach_withbac$BUGSoutput$summary

save.image(file='FMS_Final_Models_AdjustedforGPP.RData')



plot(model_linear_nogppxreach$BUGSoutput$summary[seq(27,343,1),1], main="Residuals for epsilons") #All EPS

par(mfrow=c(1,1))
plot(model_linear_nogppxreach$BUGSoutput$summary[seq(27,343,4),1], main="Residuals for Lee's Ferry Reach") #Lees Ferry
plot(model_linear_nogppxreach$BUGSoutput$summary[seq(28,344,4),1], main="Residuals for Little Colorado River Reach") #LCR
plot(model_linear_nogppxreach$BUGSoutput$summary[seq(29,345,4),1], main="Residuals for Bright Angel Creek Reach") #Phantom
plot(model_linear_nogppxreach$BUGSoutput$summary[seq(30,346,4),1], main="Residuals for National Creek Reach") #National

traceplot(model_linear_nogppxreach,varname='beta_temp')







########################################################################################################

### CHOSEN MODEL LINEAR LENGTH MODEL INTS ONLY - P VALUES REMOVED ####
#LINEAR length + temp + tempQ + turb + GPP, SLAB AND SPIKE TEST


jags.params.chosen<- c("beta0","beta_Li", "beta_temp","beta_temp2", "beta_turb","beta_turb2", "beta_gpp","beta_gpp2",
                       "deviance", "L_obs_sig", "L_proc_sig", "eps", "eps2", "eps_sig", "eps_sig2", " L_obs_err_tau_within",
                       "var_eps", "var_eps2", "var_int", "var_slope")

memory.limit(60000)
# Initialize model
start_time <- Sys.time()
model_chosen_linear_model_nogppxreach<-jags.parallel(model.file="model_chosen_linear_model_slabandspiketest_nogppxreach.R", data=dattrunc4,  inits=NULL, n.chains = 3, jags.params.chosen, n.iter=20000, n.thin=5)
end_time <- Sys.time()
end_time - start_time

output_fms_chosen <- model_chosen_linear_model_nogppxreach$BUGSoutput$summary

save.image(file='FMS_Final_Models_AdjustedforGPP.RData')

write.csv (x=output_fms_chosen, file="C:/Users/lhansen/Desktop/FMS_Growth/Stats/output_fms_chosen.csv")



plot(model_chosen_linear_model_nogppxreach$BUGSoutput$summary[seq(27,343,1),1], main="Residuals for epsilons") #All EPS

par(mfrow=c(1,1))
plot(model_chosen_linear_model_nogppxreach$BUGSoutput$summary[seq(19,335,4),1], main="Residuals for Lee's Ferry Reach") #Lees Ferry
plot(model_chosen_linear_model_nogppxreach$BUGSoutput$summary[seq(20,336,4),1], main="Residuals for Little Colorado River Reach") #LCR
plot(model_chosen_linear_model_nogppxreach$BUGSoutput$summary[seq(21,337,4),1], main="Residuals for Bright Angel Creek Reach") #Phantom
plot(model_chosen_linear_model_nogppxreach$BUGSoutput$summary[seq(22,338,4),1], main="Residuals for National Creek Reach") #National

traceplot(model_chosen_linear_model_nogppxreach,varname='beta_temp')



#######################   RESIDUALS   ##################################################
Residuals
par(mfrow=c(1,1))
jm6$BUGSoutput$summary
plot(jm6$BUGSoutput$summary[seq(15,334,1),1], main="Residuals for epsilons") #All EPS

par(mfrow=c(2,2))
plot(jm6$BUGSoutput$summary[seq(15,331,4),1], main="Residuals for Lee's Ferry Reach") #Lees Ferry
plot(jm6$BUGSoutput$summary[seq(16,332,4),1], main="Residuals for Little Colorado River Reach") #LCR
plot(jm6$BUGSoutput$summary[seq(17,333,4),1], main="Residuals for Bright Angel Creek Reach") #Phantom
plot(jm6$BUGSoutput$summary[seq(18,334,4),1], main="Residuals for National Creek Reach") #National


#Residuals vs. covariates
windows()
plot(subset(cbind(as.vector(tempmatrix[4:7,]),exp(residual_mean+as.numeric(as.vector(tempmatrix[4:7,]))*1.16), reach ==1)))
# Resid v. temp
plot(subset(cbind(as.vector(tempmatrix[4:7,]), residual_mean), reach==1))
#Resid v. turb
plot(subset(cbind(as.vector(turbmatrix[5:8,]), residual_mean), reach==1))
#Resid V. GPP
plot(subset(cbind(as.vector(gppmatrix[4:7,]), residual_mean), reach==2))

# Flannelmouth-Sucker-Growth

Project Title: 
Linking ecosystem processes to consumer growth rates: gross primary productivity as a driver of fish growth

Primary Author:
Lindsay E Hansen

4/5/2022

Abstract:
Individual growth can be constrained by rates of resource acquisition and environmental conditions and can exert strong control on population dynamics. 
Accurately quantifying resource availability over large spatial extents and at high temporal frequency is often difficult and limits attempts to understand 
the extent to which resources (i.e., food) limit individual growth. In freshwater systems, daily time series of stream metabolism are increasingly available 
but have not, to our knowledge, been linked to fish growth. Here we estimate the extent to which gross primary productivity, water temperature and turbidity 
predict growth of a native fish species, flannelmouth sucker, over a 300 km segment of a regulated river. We do this using state space growth models fit to six 
years of mark-recapture data. Consistent with past research in the system, we find that water temperature is the dominant driver of spatiotemporal variation in 
growth. However, we also find that GPP, a proxy for resource availability, is strongly linked to fish growth. Linking fish growth to stream metabolism is likely 
to improve understanding of ecosystem drivers in specific systems as well as answering broader questions regarding how ecosystems will respond to ongoing 
environmental change.

Model Description: 
The state-space Bayesian growth models included in this repository allow us to
1. compare and select a growth in length model that best describes flannelmouth sucker growth in the Grand Canyon, and 
2. Use the environmental conditions of gross primary productivity, water temperature, and turbidity to model fish growth in length.

Additional Model Info:
All analyses were conducted with JAGS (Plummer 2003) using the package r2jags in R (R Core Development Team 2019). Models were run with three chains of Markov Chain 
Monte Carlo (MCMC) simulations and judged to have converged when all monitored r-hat values were less than 1.1 (Tierney 1994). We did not specify initial values and 
all priors were specified to be only weakly informative. For more details on the observation model and priors, including model code, see the appendix and 
supplementary materials.

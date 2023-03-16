# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                           #
#    Predicting Individualized Effects of Internet-Based Treatment          #
#              Genito-Pelvic Pain/Penetration Disorder:                     #
#             Development and Internal Validation of a                      #
#                 Multivariable Decision Tree Model                         #
#                                                                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(dplyr)
library(skimr)
library(tidyverse)
library(purrr)
library(psych)
library(ggplot2)
library(partykit)
library(mobForest)

load("datforanalysis_27012023.rda")


# 1. mobForest ---------------------------------------------------------------

# inspect data for analyses 
skim(dat_imp)

dat_imp <- dat_imp %>% select(
  # treatment indicator
  group, 
  
  #outcome (post and baseline)
  gpsps_sym_1, gpsps_sym_0, 
  
  #moderators (composite outcome variables)
  fsq.c.0, fsq.nc.0, fsfi.pain.0, fsfi.sat.0,
  noncoital_se.0, pain_int.0, gpsps.gv.0,
  
  #other moderators
  dci.cdc.0, noncoital_pe.0, who5.0, vpcq.pos.0, vpcq.neg.0, vpcq.gen.0, vpcq.cont.0,
  vpcq.cat.0, stai.t.0, pqs.happiness.0, pqs.0, fsfi.org.0, fsfi.lub.0, fsfi.des.0,
  fsfi.aro.0, esteem.0 , dci.edc.0, dci.ddcp.0, GPSPS_lifelong.0, rel.duration,
  ethn, prevtraining, prevpsychoth, degree, child, rel, age, gv.attempts, ctq.sa,
  gad.0, GPSPS_prevtreatment.0, GPSPS_duration.0
  
)

skim(dat_imp)



# run random forest 
formula = as.formula(paste("gpsps_sym_1", "~", " gpsps_sym_0 + group", sep = " "))

mobf.sexfunc.1 = mobforest.analysis(formula, 
                                    partition_vars = dat_imp %>% 
                                      dplyr::select(-gpsps_sym_1, -group,
                                              -gpsps_sym_0) %>% 
                                      colnames(.),
                                    data = dat_imp,
                                    mobforest_controls = mobforest.control(ntree = 300,
                                                                           bonferroni = T,
                                                                           replace = F,
                                                                           alpha = 0.05), 
                                    model = linearModel, 
                                    seed = 123)



## 1.1 Predictive Accuracy ---------------------------------------------------

mobf.sexfunc.1 # model
mobf.sexfunc.1@oob_predictions@overall_r2_or_acc



## 1.2 Variable importances --------------------------------------------------

# extract all predictors whose importance value is positive
selvars_pos = Filter(function(x) any(x > 0), get.varimp(mobf.sexfunc.1)) %>% names()
Filter(function(x) any(x > 0), get.varimp(mobf.sexfunc.1)) %>% names() #inspect




# 2. MOB (tree) analysis ----------------------------------------------------

formula = as.formula(paste0("gpsps_sym_1 ~ gpsps_sym_0 + group|", 
                              paste(selvars_pos, collapse = "+")))

mob.sexfunc.1 = partykit::lmtree(formula, data = dat_imp, alpha = 0.05,
                                   bonferroni = T) 

print(mob.sexfunc.1)
plot(mob.sexfunc.1)


## 2.1 evaluate  terminal node models  -------------------------------------- 

summary(mob.sexfunc.1, node=2:3) # inspect R^2, p- values, parameter estimates

bind_cols(
  pred_values = predict(mob.sexfunc.1, dat_imp, type= "response"), # predicted values
  pred_node = predict(mob.sexfunc.1, dat_imp, type= "node"),  # predicted nodes
  gpsps_sym_1 = dat_imp$gpsps_sym_1,
  group = dat_imp$group
  ) -> eval_mob

# plot predicted vs. observed values (overall)
plot(eval_mob$pred_values, eval_mob$gpsps_sym_1)

# plot predicted vs. observed values (nodes)
eval_mob %>% filter(pred.node ==3) %>%      # for node 3
  select(c(pred_values, gpsps_sym_1)) %>% 
  plot()

eval_mob %>% filter(pred.node ==2) %>%      # for node 2
  select(c(pred_values, gpsps_sym_1)) %>% 
  plot()



## 2.2 number of persons per subgroup in treatment group -----------------------

eval_mob %>% filter(pred.node==3) %>% select(group) %>% table() # node 3
eval_mob %>% filter(pred.node==2) %>% select(group) %>% table() # node 2


## 2.3 distribution of (observed) outcome in each node -----------------------

eval_mob %>% 
  filter(pred.node==3) %>% 
  dplyr::select(c("gpsps_sym_1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))

eval_mob %>% 
  filter(pred.node==2) %>% 
  dplyr::select(c("gpsps_sym_1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))



## 2.3 inspect partitioning variable ----------------------------------------

dat_imp %>% select("dci.cdc.0") %>%
  psych::describe()

## dci: cut off is more than one sd below the mean (M= 17.13, SD= 3.84, cut-off= 13)



# 3. Cohen's d ------------------------------------------------------------

node_2= eval_mob%>% filter(pred.node==2)
node_3= eval_mob %>% filter(pred.node==3)

# calculate cohen's d 

effectsize::cohens_d(gpsps_sym_1~group, data=node_2, pooled_sd = T, paired=F) # node 2
effectsize::cohens_d(gpsps_sym_1~group, data=node_3, pooled_sd = T, paired=F) # node 3




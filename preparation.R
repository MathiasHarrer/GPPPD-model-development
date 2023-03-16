# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                           #
#    Predicting Individualized Effects of Internet-Based Treatment          #
#              Genito-Pelvic Pain/Penetration Disorder:                     #
#            Development and Internal Validation of a                       #
#                Multivariable Decision Tree Model                          #
#                                                                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(readr)
library(dplyr)
library(purrr)
library(psych)
library(ggplot2)
library(missForest)
library(skimr)
library(sn)
library(haven)
library(hablar)

# data
gpsps_200 <- read_csv("gpsps_200.csv")
GPSPS_V13_Datensatz_06_20_imp_ag_auto_FINAL <- read_sav("GPSPS - V13 Datensatz 06.20 imp ag auto FINAL.sav")


# 1. Define Variables -----------------------------------------------------

## Merge Further Variables -------------------------------------------------
GPSPS_V13_Datensatz_06_20_imp_ag_auto_FINAL$id.old = GPSPS_V13_Datensatz_06_20_imp_ag_auto_FINAL$ClientExternalID %>% retype()
join <- GPSPS_V13_Datensatz_06_20_imp_ag_auto_FINAL %>% 
  select(id.old, 
         SDF_10_GPSPS_lifelong_t0,  # GPSPS lifelong GPPPD diagnosis
         SDF_9_Dauer_GPSPS_inJ_t0,  # duration of GPPPD
         SDF_26_Behandlung_GPSPS_vergangen_t0 # previous GPPPD treatment
         )



dat <- left_join(gpsps_200, join, by= "id.old") %>% 
  rename(
    "GPSPS_lifelong.0" = SDF_10_GPSPS_lifelong_t0,
    "GPSPS_duration.0" = SDF_9_Dauer_GPSPS_inJ_t0,
    "GPSPS_prevtreatment.0" = SDF_26_Behandlung_GPSPS_vergangen_t0,
    "pqs.happiness.0" = pqs.0.i10,
    "pain_int.0"= vag.0,
    "pain_int.1"=vag.1,
    "noncoital_pe.0"= gpsps.pe.0,
    "noncoital_pe.1"= gpsps.pe.1,
    "noncoital_se.0"= gpsps.se.0,
    "noncoital_se.1"= gpsps.se.1,
  )


# PFB Questionnaire Happiness Item
dat$pqs.happiness.0 %>% mean()

# exclude HAPA
dat <- dat %>% select(-c(hapa.absrisk.0,hapa.relrisk.0, hapa.oe.0, hapa.risksev.0,
                         hapa.tse.0, hapa.i.0))

## Define Outcomes ---------------------------------------------------------


# FSFI

## pain subscale
dat$fsfi.pain.1<-rowSums(dat[c("fsfi.1.i17", "fsfi.1.i18", "fsfi.1.i19")])*0.4
dat$fsfi.pain.0<-rowSums(dat[c("fsfi.0.i17", "fsfi.0.i18", "fsfi.0.i19")])*0.4

# satisfaction subscale
dat$fsfi.sat.1<-rowSums(dat[c("fsfi.1.i14", "fsfi.1.i15", "fsfi.1.i16")])*0.4
dat$fsfi.sat.0<-rowSums(dat[c("fsfi.0.i14", "fsfi.0.i15", "fsfi.0.i16")])*0.4



# FSQ (according to ter Kuile et al., 2007)

## coital subscale
dat$fsq.c.1 <- rowSums(dat[c("fsq.1.i4", "fsq.1.i5", "fsq.1.i7")])
dat$fsq.c.0 <- rowSums(dat[c("fsq.0.i4", "fsq.0.i5", "fsq.0.i7")])

## noncoital subscale
dat$fsq.nc.1 <- rowSums(dat[c("fsq.1.i1", "fsq.1.i2", "fsq.1.i3",
                                          "fsq.1.i6", "fsq.1.i8")])
dat$fsq.nc.0 <- rowSums(dat[c("fsq.0.i1", "fsq.0.i2", "fsq.0.i3",
                                      "fsq.0.i6", "fsq.0.i8")])

# penetration difficulties (non- coital, self- related)
dat$noncoital_se.1<- rowSums(dat[c("gpsps.1.i1", "gpsps.1.i2", "gpsps.1.i5")])
dat$noncoital_se.0 <- rowSums(dat[c("gpsps.0.i1", "gpsps.0.i2", "gpsps.0.i5")])


# Vaginismus items (according to DSM Diagnostic Criteria for Vaginismus (Binik, 2010))
dat$pain_int.1 <- rowSums(dat[c("vag.1.i1", "vag.1.i2", "vag.1.i3")])
dat$pain_int.0 <- rowSums(dat[c("vag.0.i1", "vag.0.i2", "vag.0.i3")])




## Sample Characteristics --------------------------------------------------

# select baseline and outcome Variables 
dat_bl <- dat[grep("\\.0", colnames(dat))]

# sum score level only excluding all itemlevels
dat_bl <- dat_bl[grep("\\.i", colnames(dat_bl), invert= TRUE)]

# exclude interim assessment 
dat_bl <- dat_bl %>%  select(-contains("0_1"))

# exclude partner variables
dat_bl <- dat_bl %>%  select(-contains(".0.p"))

# accidentially deleted variables
delvars <- dat[c("ctq.sa")]

# select demographic variables
demo <- dat[c("id","group", "age", "sex", "rel", "child","degree", "prevpsychoth", 
              "prevtraining",  "sess", "ethn", "rel.duration", "gv.attempts")]

# select outcome variable
outcome_var <- dat[c("fsq.nc.1", "fsq.c.1", "fsfi.pain.1", "fsfi.sat.1", "gpsps.gv.1",
                     "noncoital_se.1", "pain_int.1")]

# bind relevant demos, baseline, outcome variables and accidentially deleted variables
dat <- as.data.frame(cbind(demo, dat_bl, outcome_var, delvars)) 

# select factors (and numeric variables with five or fewer unique values)
sapply(dat, function(x) length(unique(x)))

factors = c("group","sex", "rel", "child", "degree", "prevtraining", "prevpsychoth", 
            "ethn","gpsps.gv.0", "gpsps.gv.1", "GPSPS_lifelong.0", "GPSPS_prevtreatment.0")


dat[factors] = dat %>% select(factors) %>% 
  map_dfr(function(x) as.factor(x))

str(dat)


## Descriptive Analyses ----------------------------------------------------

char$numeric = psych::describe(dat %>% select(-factors))
char$numeric.bygroups = psych::describeBy(dat %>% select(-factors), dat$group)

char$factor = summary(dat %>% select(factors))
char$factor.bygroups = by(dat %>% select(factors), dat$group, summary)

# dropout in outcome variables per group
desc_miss <- dat %>% 
  select(contains(c("fsq", "fsfi", "gpsps.gv", "pain_int", "concoital_se", "group"))) %>% 
  group_by(group) %>% 
  skim()


# 2. Imputation ------------------------------------------------------------
# Imputation (single imputation via missForest) 

# exclude variables for that the imputation is not relevant (sess, id)
datforimp <- dat %>% select(-c(sess, id)) 

## impute
dat.imp <- missForest(datforimp, maxiter = 50, verbose = TRUE)


## Descriptives after Imputation -------------------------------------------

dat_imp <- dat.imp$ximp
skim(dat_imp)  

## compare with unimputed dataset

dat_imp %>% select(-factors) %>% psych::describe()
dat %>% select(-factors) %>% psych::describe() # data before imputation

dat_imp %>% select(factors) %>% summary()
dat %>% select(factors) %>% summary() # data before imputation

# only 1 person who had a missing on gpsps.gv.1 before got "gv possible" after imputation

# histograms of numeric variables
dat_imp %>% select(-factors) %>% psych::multi.hist()


# 3.Composite measure (post) ----------------------------------------------

## Recode Scores -----------------------------------------------------------
# that higher score means higher functioning

# FSQ
## coital subscale
dat_imp$fsq.c.1 <- 16 - dat_imp$fsq.c.1

## non- coital subscale
dat_imp$fsq.nc.1 <- 26 - dat_imp$fsq.nc.1

# Vaginismus
dat_imp$pain_int.1 <- 16 - dat_imp$pain_int.1


## Compute Composite Measure ------------------------------------------------

# To ensure comparability between scales prior to composite measure 
# computation, all scales included in the composite measure are divided into 
# eleven quantiles (taking individual skewness of each scale into account) and 
# scale values are allocated to the respective quantiles.

# calculate skewness
desc_fsfi.pain.1 <- psych::describe(dat_imp$fsfi.pain.1)
desc_fsfi.sat.1  <- psych::describe(dat_imp$fsfi.sat.1)
desc_fsq.c.1     <- psych::describe(dat_imp$fsq.c.1)
desc_fsq.nc.1    <- psych::describe(dat_imp$fsq.nc.1)
desc_noncoital_se.1  <- psych::describe(dat_imp$noncoital_se.1)
desc_pain_int.1       <- psych::describe(dat_imp$pain_int.1)

# FSQ (coital subscale)

## fsq.c (taking skewness into account)
quant11.fsq.c.norm.1 <- sn::qsn(seq(0, 1, by = 1/11), 
                                xi = c(mean(dat_imp$fsq.c.1, na.rm = T)), 
                                omega = c(sd(dat_imp$fsq.c.1, na.rm = T)), 
                                alpha = desc_fsq.c.1$skew)


dat_imp <- dat_imp %>%
  mutate(fsq.c.q11.norm.1 = case_when(
    fsq.c.1 < quant11.fsq.c.norm.1[2] ~  1,
    fsq.c.1 < quant11.fsq.c.norm.1[3] & fsq.c.1 > quant11.fsq.c.norm.1[2] ~ 2, 
    fsq.c.1 < quant11.fsq.c.norm.1[4] & fsq.c.1 > quant11.fsq.c.norm.1[3] ~ 3, 
    fsq.c.1 < quant11.fsq.c.norm.1[5] & fsq.c.1 > quant11.fsq.c.norm.1[4] ~ 4, 
    fsq.c.1 < quant11.fsq.c.norm.1[6] & fsq.c.1 > quant11.fsq.c.norm.1[5] ~ 5, 
    fsq.c.1 < quant11.fsq.c.norm.1[7] & fsq.c.1 > quant11.fsq.c.norm.1[6] ~ 6, 
    fsq.c.1 < quant11.fsq.c.norm.1[8] & fsq.c.1 > quant11.fsq.c.norm.1[7] ~ 7,
    fsq.c.1 < quant11.fsq.c.norm.1[9] & fsq.c.1 > quant11.fsq.c.norm.1[8] ~ 8,
    fsq.c.1 < quant11.fsq.c.norm.1[10]& fsq.c.1 > quant11.fsq.c.norm.1[9] ~ 9,
    fsq.c.1 < quant11.fsq.c.norm.1[11]& fsq.c.1 > quant11.fsq.c.norm.1[10] ~ 10,
    fsq.c.1 > quant11.fsq.c.norm.1[11] ~ 11
  ))

# fsq.nc

## fsq.c (taking skewness into account)
quant11.fsq.nc.norm.1<-sn::qsn(seq(0, 1, by = 1/11), 
                               xi = c(mean(dat_imp$fsq.nc.1, na.rm = T)), 
                               omega = c(sd(dat_imp$fsq.nc.1, na.rm = T)), 
                               alpha = desc_fsq.nc.1$skew)

dat_imp <- dat_imp %>%
  mutate(fsq.nc.q11.norm.1 = case_when(
    fsq.nc.1 < quant11.fsq.nc.norm.1[2] ~  1,
    fsq.nc.1 < quant11.fsq.nc.norm.1[3] & fsq.nc.1 > quant11.fsq.nc.norm.1[2] ~ 2, 
    fsq.nc.1 < quant11.fsq.nc.norm.1[4] & fsq.nc.1 > quant11.fsq.nc.norm.1[3] ~ 3, 
    fsq.nc.1 < quant11.fsq.nc.norm.1[5] & fsq.nc.1 > quant11.fsq.nc.norm.1[4] ~ 4, 
    fsq.nc.1 < quant11.fsq.nc.norm.1[6] & fsq.nc.1 > quant11.fsq.nc.norm.1[5] ~ 5, 
    fsq.nc.1 < quant11.fsq.nc.norm.1[7] & fsq.nc.1 > quant11.fsq.nc.norm.1[6] ~ 6, 
    fsq.nc.1 < quant11.fsq.nc.norm.1[8] & fsq.nc.1 > quant11.fsq.nc.norm.1[7] ~ 7,
    fsq.nc.1 < quant11.fsq.nc.norm.1[9] & fsq.nc.1 > quant11.fsq.nc.norm.1[8] ~ 8,
    fsq.nc.1 < quant11.fsq.nc.norm.1[10]& fsq.nc.1 > quant11.fsq.nc.norm.1[9] ~ 9,
    fsq.nc.1 < quant11.fsq.nc.norm.1[11]& fsq.nc.1 > quant11.fsq.nc.norm.1[10] ~ 10,
    fsq.nc.1 > quant11.fsq.nc.norm.1[11] ~ 11
  ))

# FSFI (pain and satisfaction subscales)

## fsfi.pain (taking skewness into account)
quant11.fsfi.pain.norm.1<-sn::qsn(seq(0, 1, by = 1/11), 
                                  xi = c(mean(dat_imp$fsfi.pain.1, na.rm = T)), 
                                  omega = c(sd(dat_imp$fsfi.pain.1, na.rm = T)), 
                                  alpha = desc_fsfi.pain.1$skew)


dat_imp <- dat_imp %>%
  mutate(fsfi.pain.q11.norm.1 = case_when(
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[2] ~  1,
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[3] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[2] ~ 2, 
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[4] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[3] ~ 3, 
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[5] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[4] ~ 4, 
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[6] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[5] ~ 5, 
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[7] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[6] ~ 6, 
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[8] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[7] ~ 7,
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[9] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[8] ~ 8,
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[10] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[9] ~ 9,
    fsfi.pain.1 < quant11.fsfi.pain.norm.1[11] & fsfi.pain.1 > quant11.fsfi.pain.norm.1[10] ~ 10,
    fsfi.pain.1 > quant11.fsfi.pain.norm.1[11] ~ 11
  ))

## fsfi.satisfaction

## fsfi.satisfaction (taking skewness into account)
quant11.fsfi.sat.norm.1<-sn::qsn(seq(0, 1, by = 1/11), 
                                 xi = mean(dat_imp$fsfi.sat.1, na.rm = T), 
                                 omega = sd(dat_imp$fsfi.sat.1, na.rm = T), 
                                 alpha= desc_fsfi.sat.1$skew)


dat_imp <- dat_imp %>%
  mutate(fsfi.sat.q11.norm.1 = case_when(
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[2] ~  1,
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[3] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[2] ~ 2, 
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[4] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[3] ~ 3, 
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[5] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[4] ~ 4, 
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[6] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[5] ~ 5, 
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[7] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[6] ~ 6, 
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[8] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[7] ~ 7,
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[9] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[8] ~ 8,
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[10] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[9] ~ 9,
    fsfi.sat.1 < quant11.fsfi.sat.norm.1[11] & fsfi.sat.1 > quant11.fsfi.sat.norm.1[10] ~ 10,
    fsfi.sat.1 > quant11.fsfi.sat.norm.1[11] ~ 11
  ))

# PEQ (non- coital insertion subscale)

## PEQ non- coital insertion subscale (taking skewness into account)
quant11.noncoital_se.norm.1<-sn::qsn(seq(0, 1, by = 1/11), 
                                 xi = mean(dat_imp$noncoital_se.1, na.rm = T), 
                                 omega = sd(dat_imp$noncoital_se.1, na.rm = T), 
                                 alpha = desc_noncoital_se.1$skew)


dat_imp <- dat_imp %>%
  mutate(noncoital_se.q11.norm.1 = case_when(
    noncoital_se.1 < quant11.noncoital_se.norm.1[2] ~  1,
    noncoital_se.1 < quant11.noncoital_se.norm.1[3]  & noncoital_se.1 > quant11.noncoital_se.norm.1[2] ~ 2, 
    noncoital_se.1 < quant11.noncoital_se.norm.1[4]  & noncoital_se.1 > quant11.noncoital_se.norm.1[3] ~ 3, 
    noncoital_se.1 < quant11.noncoital_se.norm.1[5]  & noncoital_se.1 > quant11.noncoital_se.norm.1[4] ~ 4, 
    noncoital_se.1 < quant11.noncoital_se.norm.1[6]  & noncoital_se.1 > quant11.noncoital_se.norm.1[5] ~ 5, 
    noncoital_se.1 < quant11.noncoital_se.norm.1[7]  & noncoital_se.1 > quant11.noncoital_se.norm.1[6] ~ 6, 
    noncoital_se.1 < quant11.noncoital_se.norm.1[8]  & noncoital_se.1 > quant11.noncoital_se.norm.1[7] ~ 7,
    noncoital_se.1 < quant11.noncoital_se.norm.1[9]  & noncoital_se.1 > quant11.noncoital_se.norm.1[8] ~ 8,
    noncoital_se.1 < quant11.noncoital_se.norm.1[10] & noncoital_se.1 > quant11.noncoital_se.norm.1[9] ~ 9,
    noncoital_se.1 < quant11.noncoital_se.norm.1[11] & noncoital_se.1 > quant11.noncoital_se.norm.1[10] ~ 10,
    noncoital_se.1 > quant11.noncoital_se.norm.1[11] ~ 11
  ))

# Vaginismus (extracted from DSM- IV criteria)

## Vaginismus (taking into account the skewness)
quant11.pain_int.q11.norm.1<-sn::qsn(seq(0, 1, by = 1/11), 
                            xi = mean(dat_imp$pain_int.1, na.rm = T), 
                            omega = sd(dat_imp$pain_int.1, na.rm = T), 
                            alpha = desc_pain_int.1$skew)


dat_imp <- dat_imp %>%
  mutate(pain_int.q11.norm.1 = case_when(
    pain_int.1 < quant11.pain_int.q11.norm.1[2] ~  1,
    pain_int.1 < quant11.pain_int.q11.norm.1[3]  & pain_int.1 > quant11.pain_int.q11.norm.1[2] ~ 2, 
    pain_int.1 < quant11.pain_int.q11.norm.1[4]  & pain_int.1 > quant11.pain_int.q11.norm.1[3] ~ 3, 
    pain_int.1 < quant11.pain_int.q11.norm.1[5]  & pain_int.1 > quant11.pain_int.q11.norm.1[4] ~ 4, 
    pain_int.1 < quant11.pain_int.q11.norm.1[6]  & pain_int.1 > quant11.pain_int.q11.norm.1[5] ~ 5, 
    pain_int.1 < quant11.pain_int.q11.norm.1[7]  & pain_int.1 > quant11.pain_int.q11.norm.1[6] ~ 6, 
    pain_int.1 < quant11.pain_int.q11.norm.1[8]  & pain_int.1 > quant11.pain_int.q11.norm.1[7] ~ 7,
    pain_int.1 < quant11.pain_int.q11.norm.1[9]  & pain_int.1 > quant11.pain_int.q11.norm.1[8] ~ 8,
    pain_int.1 < quant11.pain_int.q11.norm.1[10] & pain_int.1 > quant11.pain_int.q11.norm.1[9] ~ 9,
    pain_int.1 < quant11.pain_int.q11.norm.1[11] & pain_int.1 > quant11.pain_int.q11.norm.1[10] ~ 10,
    pain_int.1 > quant11.pain_int.q11.norm.1[11] ~ 11
  ))

# sexual intercourse (yes/ no)
# assign values to extreme categories
dat_imp <- dat_imp %>%
  mutate(gpsps.gv.q11.1 = case_when(
    gpsps.gv.1 == 0 ~ 1,
    gpsps.gv.1 == 1 ~ 11
  ))

## addition to one overall score 

## taking skewness into account
dat_imp$gpsps_sym_1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1",
           "fsfi.sat.q11.norm.1", "noncoital_se.q11.norm.1", "pain_int.q11.norm.1", 
           "gpsps.gv.q11.1")) %>% 
  rowSums()


## Inspect Outcome ---------------------------------------------------------

# descriptives
skim(dat_imp$gpsps_sym_1)
psych::describe(dat_imp$gpsps_sym_1)

# distribution
hist(dat_imp$gpsps_sym_1)

# Compute the density data
dens <- density(dat_imp$gpsps_sym_1)

# plot density
plot(dens, frame = FALSE, col = "steelblue", 
     main = "Density plot of sexfunc")  

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5, color= "lightblue", fill= "lightblue") +
  geom_vline(aes(xintercept=0))+
  theme_minimal()


## Intervention vs. control group 

# separately for intervention vs. control group

ig <- dat_imp %>% dplyr::filter(group == 1)
cg <- dat_imp %>% dplyr::filter(group == 0)

ig %>% 
  dplyr::select(c("gpsps_sym_1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5, color= "lightblue", fill= "lightblue") +
  geom_vline(aes(xintercept=0))+
  theme_minimal()

cg%>% 
  dplyr::select(c( "gpsps_sym_1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5, color= "lightblue", fill= "lightblue") +
  geom_vline(aes(xintercept=0))+
  theme_minimal()



## Leave-one-out plots --------------------------------------------------

# without fsq.c
dat_imp$gpsps_sym_1_without_fsq.c.1 <- dat_imp %>%
  select(c("fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1", "fsfi.sat.q11.norm.1", 
           "noncoital_se.q11.norm.1", "pain_int.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_fsq.c.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


# without fsq.nc
dat_imp$gpsps_sym_1_without_fsq.nc.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsfi.pain.q11.norm.1", "fsfi.sat.q11.norm.1", 
           "noncoital_se.q11.norm.1", "pain_int.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_fsq.nc.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


# without fsfi.pain
dat_imp$gpsps_sym_1_without_fsfi.pain.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.sat.q11.norm.1", 
           "noncoital_se.q11.norm.1", "pain_int.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_fsfi.pain.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


# without fsfi.sat
dat_imp$gpsps_sym_1_without_fsfi.sat.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1",
           "noncoital_se.q11.norm.1", "pain_int.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_fsfi.sat.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


# without gpsps.se
dat_imp$gpsps_sym_1_without_noncoital_se.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1",
           "fsfi.sat.q11.norm.1", "pain_int.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_noncoital_se.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


# without vag
dat_imp$gpsps_sym_1_without_pain_int.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1",
           "fsfi.sat.q11.norm.1",  "noncoital_se.q11.norm.1", "gpsps.gv.q11.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_pain_int.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))



# without gpsps.gv
dat_imp$gpsps_sym_1_without_gv.1 <- dat_imp %>%
  select(c("fsq.c.q11.norm.1", "fsq.nc.q11.norm.1", "fsfi.pain.q11.norm.1",
           "fsfi.sat.q11.norm.1",  "noncoital_se.q11.norm.1", "pain_int.q11.norm.1")) %>% 
  rowSums()

dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_gv.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))


ig <- dat_imp %>% dplyr::filter(group == 1)
cg <- dat_imp %>% dplyr::filter(group == 0)


# compare all leave-one-out plots
dat_imp %>% 
  dplyr::select(c("gpsps_sym_1_without_fsq.c.1", "gpsps_sym_1_without_fsq.nc.1", 
                  "gpsps_sym_1_without_fsfi.pain.1", "gpsps_sym_1_without_fsfi.sat.1", 
                  "gpsps_sym_1_without_noncoital_se.1", "gpsps_sym_1_without_pain_int.1", 
                  "gpsps_sym_1_without_gv.1")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))+
  theme_minimal()



## all quite similar. if gpsps.se is left out less bimodal distribution
multi.hist(dat_imp[c("gpsps_sym_1_without_fsq.c.1", "gpsps_sym_1_without_fsq.nc.1", 
                     "gpsps_sym_1_without_fsfi.pain.1", "gpsps_sym_1_without_fsfi.sat.1", 
                     "gpsps_sym_1_without_noncoital_se.1", "gpsps_sym_1_without_pain_int.1", 
                     "gpsps_sym_1_without_gv.1")])

dat_imp %>% select(c("gpsps_sym_1_without_fsq.c.1", "gpsps_sym_1_without_fsq.nc.1", 
                     "gpsps_sym_1_without_fsfi.pain.1", "gpsps_sym_1_without_fsfi.sat.1", 
                     "gpsps_sym_1_without_noncoital_se.1", "gpsps_sym_1_without_pain_int.1", 
                     "gpsps_sym_1_without_gv.1")) %>% psych::describe()

# separately for IG vs. CG
dat_imp %>% 
  filter(group ==0) %>% 
  select(c("gpsps_sym_1_without_fsq.c.1", "gpsps_sym_1_without_fsq.nc.1", 
           "gpsps_sym_1_without_fsfi.pain.1", "gpsps_sym_1_without_fsfi.sat.1", 
           "gpsps_sym_1_without_noncoital_se.1", "gpsps_sym_1_without_pain_int.1", 
           "gpsps_sym_1_without_gv.1")) %>% 
  multi.hist()

dat_imp %>% 
  filter(group ==1) %>% 
  select(c("gpsps_sym_1_without_fsq.c.1", "gpsps_sym_1_without_fsq.nc.1", 
           "gpsps_sym_1_without_fsfi.pain.1", "gpsps_sym_1_without_fsfi.sat.1", 
           "gpsps_sym_1_without_noncoital_se.1", "gpsps_sym_1_without_pain_int.1", 
           "gpsps_sym_1_without_gv.1")) %>% 
  multi.hist()



# 4. Composite Measure (baseline) ------------------------------------------

## Recode Scores -----------------------------------------------------------
# that higher score means higher functioning

# FSQ
## coital subscale
dat_imp$fsq.c.0 <- 16 - dat_imp$fsq.c.0

## non- coital subscale
dat_imp$fsq.nc.0 <- 26 - dat_imp$fsq.nc.0

# Vaginismus
dat_imp$pain_int.0 <- 16 - dat_imp$pain_int.0


## Compute Composite Score ------------------------------------------------

# calculate skewness
desc_fsfi.pain.0 <- psych::describe(dat_imp$fsfi.pain.0)
desc_fsfi.sat.0  <- psych::describe(dat_imp$fsfi.sat.0)
desc_fsq.c.0     <- psych::describe(dat_imp$fsq.c.0)
desc_fsq.nc.0    <- psych::describe(dat_imp$fsq.nc.0)
desc_noncoital_se.0  <- psych::describe(dat_imp$noncoital_se.0)
desc_pain_int.0       <- psych::describe(dat_imp$pain_int.0)

# FSQ (coital subscale)

## fsq.c (taking skewness into account)
quant11.fsq.c.norm.0 <- sn::qsn(seq(0, 1, by = 1/11), 
                                xi = c(mean(dat_imp$fsq.c.0, na.rm = T)), 
                                omega = c(sd(dat_imp$fsq.c.0, na.rm = T)), 
                                alpha = desc_fsq.c.0$skew)


dat_imp <- dat_imp %>%
  mutate(fsq.c.q11.norm.0 = case_when(
    fsq.c.0 < quant11.fsq.c.norm.0[2] ~  1,
    fsq.c.0 < quant11.fsq.c.norm.0[3] & fsq.c.0 > quant11.fsq.c.norm.0[2] ~ 2, 
    fsq.c.0 < quant11.fsq.c.norm.0[4] & fsq.c.0 > quant11.fsq.c.norm.0[3] ~ 3, 
    fsq.c.0 < quant11.fsq.c.norm.0[5] & fsq.c.0 > quant11.fsq.c.norm.0[4] ~ 4, 
    fsq.c.0 < quant11.fsq.c.norm.0[6] & fsq.c.0 > quant11.fsq.c.norm.0[5] ~ 5, 
    fsq.c.0 < quant11.fsq.c.norm.0[7] & fsq.c.0 > quant11.fsq.c.norm.0[6] ~ 6, 
    fsq.c.0 < quant11.fsq.c.norm.0[8] & fsq.c.0 > quant11.fsq.c.norm.0[7] ~ 7,
    fsq.c.0 < quant11.fsq.c.norm.0[9] & fsq.c.0 > quant11.fsq.c.norm.0[8] ~ 8,
    fsq.c.0 < quant11.fsq.c.norm.0[10]& fsq.c.0 > quant11.fsq.c.norm.0[9] ~ 9,
    fsq.c.0 < quant11.fsq.c.norm.0[11]& fsq.c.0 > quant11.fsq.c.norm.0[10] ~ 10,
    fsq.c.0 > quant11.fsq.c.norm.0[11] ~ 11
  ))

# fsq.nc

## fsq.c (taking skewness into account)
quant11.fsq.nc.norm.0<-sn::qsn(seq(0, 1, by = 1/11), 
                               xi = c(mean(dat_imp$fsq.nc.0, na.rm = T)), 
                               omega = c(sd(dat_imp$fsq.nc.0, na.rm = T)), 
                               alpha = desc_fsq.nc.0$skew)

dat_imp <- dat_imp %>%
  mutate(fsq.nc.q11.norm.0 = case_when(
    fsq.nc.0 < quant11.fsq.nc.norm.0[2] ~  1,
    fsq.nc.0 < quant11.fsq.nc.norm.0[3] & fsq.nc.0 > quant11.fsq.nc.norm.0[2] ~ 2, 
    fsq.nc.0 < quant11.fsq.nc.norm.0[4] & fsq.nc.0 > quant11.fsq.nc.norm.0[3] ~ 3, 
    fsq.nc.0 < quant11.fsq.nc.norm.0[5] & fsq.nc.0 > quant11.fsq.nc.norm.0[4] ~ 4, 
    fsq.nc.0 < quant11.fsq.nc.norm.0[6] & fsq.nc.0 > quant11.fsq.nc.norm.0[5] ~ 5, 
    fsq.nc.0 < quant11.fsq.nc.norm.0[7] & fsq.nc.0 > quant11.fsq.nc.norm.0[6] ~ 6, 
    fsq.nc.0 < quant11.fsq.nc.norm.0[8] & fsq.nc.0 > quant11.fsq.nc.norm.0[7] ~ 7,
    fsq.nc.0 < quant11.fsq.nc.norm.0[9] & fsq.nc.0 > quant11.fsq.nc.norm.0[8] ~ 8,
    fsq.nc.0 < quant11.fsq.nc.norm.0[10]& fsq.nc.0 > quant11.fsq.nc.norm.0[9] ~ 9,
    fsq.nc.0 < quant11.fsq.nc.norm.0[11]& fsq.nc.0 > quant11.fsq.nc.norm.0[10] ~ 10,
    fsq.nc.0 > quant11.fsq.nc.norm.0[11] ~ 11
  ))

# FSFI (pain and satisfaction subscales)

## fsfi.pain (taking skewness into account)
quant11.fsfi.pain.norm.0<-sn::qsn(seq(0, 1, by = 1/11), 
                                  xi = c(mean(dat_imp$fsfi.pain.0, na.rm = T)), 
                                  omega = c(sd(dat_imp$fsfi.pain.0, na.rm = T)), 
                                  alpha = desc_fsfi.pain.0$skew)


dat_imp <- dat_imp %>%
  mutate(fsfi.pain.q11.norm.0 = case_when(
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[2] ~  1,
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[3] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[2] ~ 2, 
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[4] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[3] ~ 3, 
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[5] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[4] ~ 4, 
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[6] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[5] ~ 5, 
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[7] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[6] ~ 6, 
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[8] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[7] ~ 7,
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[9] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[8] ~ 8,
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[10] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[9] ~ 9,
    fsfi.pain.0 < quant11.fsfi.pain.norm.0[11] & fsfi.pain.0 > quant11.fsfi.pain.norm.0[10] ~ 10,
    fsfi.pain.0 > quant11.fsfi.pain.norm.0[11] ~ 11
  ))

## fsfi.satisfaction

## fsfi.satisfaction (taking skewness into account)
quant11.fsfi.sat.norm.0<-sn::qsn(seq(0, 1, by = 1/11), 
                                 xi = mean(dat_imp$fsfi.sat.0, na.rm = T), 
                                 omega = sd(dat_imp$fsfi.sat.0, na.rm = T), 
                                 alpha= desc_fsfi.sat.0$skew)


dat_imp <- dat_imp %>%
  mutate(fsfi.sat.q11.norm.0 = case_when(
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[2] ~  1,
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[3] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[2] ~ 2, 
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[4] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[3] ~ 3, 
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[5] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[4] ~ 4, 
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[6] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[5] ~ 5, 
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[7] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[6] ~ 6, 
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[8] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[7] ~ 7,
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[9] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[8] ~ 8,
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[10] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[9] ~ 9,
    fsfi.sat.0 < quant11.fsfi.sat.norm.0[11] & fsfi.sat.0 > quant11.fsfi.sat.norm.0[10] ~ 10,
    fsfi.sat.0 > quant11.fsfi.sat.norm.0[11] ~ 11
  ))

# PEQ (non- coital insertion subscale)

## PEQ non- coital insertion subscale (taking skewness into account)
quant11.noncoital_se.norm.0<-sn::qsn(seq(0, 1, by = 1/11), 
                                 xi = mean(dat_imp$noncoital_se.0, na.rm = T), 
                                 omega = sd(dat_imp$noncoital_se.0, na.rm = T), 
                                 alpha = desc_noncoital_se.0$skew)


dat_imp <- dat_imp %>%
  mutate(noncoital_se.q11.norm.0 = case_when(
    noncoital_se.0 < quant11.noncoital_se.norm.0[2] ~  1,
    noncoital_se.0 < quant11.noncoital_se.norm.0[3] &  noncoital_se.0 > quant11.noncoital_se.norm.0[2] ~ 2, 
    noncoital_se.0 < quant11.noncoital_se.norm.0[4] &  noncoital_se.0 > quant11.noncoital_se.norm.0[3] ~ 3, 
    noncoital_se.0 < quant11.noncoital_se.norm.0[5] &  noncoital_se.0 > quant11.noncoital_se.norm.0[4] ~ 4, 
    noncoital_se.0 < quant11.noncoital_se.norm.0[6] &  noncoital_se.0 > quant11.noncoital_se.norm.0[5] ~ 5, 
    noncoital_se.0 < quant11.noncoital_se.norm.0[7] &  noncoital_se.0 > quant11.noncoital_se.norm.0[6] ~ 6, 
    noncoital_se.0 < quant11.noncoital_se.norm.0[8] &  noncoital_se.0 > quant11.noncoital_se.norm.0[7] ~ 7,
    noncoital_se.0 < quant11.noncoital_se.norm.0[9] &  noncoital_se.0 > quant11.noncoital_se.norm.0[8] ~ 8,
    noncoital_se.0 < quant11.noncoital_se.norm.0[10] & noncoital_se.0 > quant11.noncoital_se.norm.0[9] ~ 9,
    noncoital_se.0 < quant11.noncoital_se.norm.0[11] & noncoital_se.0 > quant11.noncoital_se.norm.0[10] ~ 10,
    noncoital_se.0 > quant11.noncoital_se.norm.0[11] ~ 11
  ))

# Vaginismus (extracted from DSM- IV criteria)

## Vaginismus (taking into account the skewness)
quant11.pain_int.norm.0<-sn::qsn(seq(0, 1, by = 1/11), 
                            xi = mean(dat_imp$pain_int.0, na.rm = T), 
                            omega = sd(dat_imp$pain_int.0, na.rm = T), 
                            alpha = desc_pain_int.0$skew)


dat_imp <- dat_imp %>%
  mutate(pain_int.q11.norm.0 = case_when(
    pain_int.0 < quant11.pain_int.norm.0[2] ~  1,
    pain_int.0 < quant11.pain_int.norm.0[3] &  pain_int.0 > quant11.pain_int.norm.0[2] ~ 2, 
    pain_int.0 < quant11.pain_int.norm.0[4] &  pain_int.0 > quant11.pain_int.norm.0[3] ~ 3, 
    pain_int.0 < quant11.pain_int.norm.0[5] &  pain_int.0 > quant11.pain_int.norm.0[4] ~ 4, 
    pain_int.0 < quant11.pain_int.norm.0[6] &  pain_int.0 > quant11.pain_int.norm.0[5] ~ 5, 
    pain_int.0 < quant11.pain_int.norm.0[7] &  pain_int.0 > quant11.pain_int.norm.0[6] ~ 6, 
    pain_int.0 < quant11.pain_int.norm.0[8] &  pain_int.0 > quant11.pain_int.norm.0[7] ~ 7,
    pain_int.0 < quant11.pain_int.norm.0[9] &  pain_int.0 > quant11.pain_int.norm.0[8] ~ 8,
    pain_int.0 < quant11.pain_int.norm.0[10] & pain_int.0 > quant11.pain_int.norm.0[9] ~ 9,
    pain_int.0 < quant11.pain_int.norm.0[11] & pain_int.0 > quant11.pain_int.norm.0[10] ~ 10,
    pain_int.0 > quant11.pain_int.norm.0[11] ~ 11
  ))

# sexual intercourse (yes/ no)
# assign values to extreme categories
dat_imp <- dat_imp %>%
  mutate(gpsps.gv.q11.0 = case_when(
    gpsps.gv.0 == 0 ~ 1,
    gpsps.gv.0 == 1 ~ 11
  ))

## Addition to one overall score 

## taking skewness into account
dat_imp$gpsps_sym_0 <- dat_imp %>%
  select(c("fsq.c.q11.norm.0", "fsq.nc.q11.norm.0", "fsfi.pain.q11.norm.0",
           "fsfi.sat.q11.norm.0", "noncoital_se.q11.norm.0", "pain_int.q11.norm.0", 
           "gpsps.gv.q11.0")) %>% 
  rowSums()


## Inspect outcome ---------------------------------------------------------

# descriptives
skim(dat_imp$gpsps_sym_0)
psych::describe(dat_imp$gpsps_sym_0)

# distribution
hist(dat_imp$gpsps_sym_0)
histBy(gpsps_sym_0~group, data=dat_imp)

# plot the density data

dat_imp %>% 
  dplyr::select(c("gpsps_sym_0")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))

# intervention versus control group
dat_imp %>% 
  filter(group==0) %>% 
  dplyr::select(c("gpsps_sym_0")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))

dat_imp %>% 
  filter(group==1) %>% 
  dplyr::select(c("gpsps_sym_0")) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0))



# 5. Addtitional Tidying -----------------------------------------------------



# sort variables
dat_imp <- dat_imp %>%
  select("group", "age", "sex", "rel", "child", "degree", 
         "prevpsychoth", "prevtraining", "ethn", "rel.duration", 
         "gv.attempts","GPSPS_lifelong.0","GPSPS_duration.0",
         "GPSPS_prevtreatment.0", sort(names(.)))


skim(dat_imp)



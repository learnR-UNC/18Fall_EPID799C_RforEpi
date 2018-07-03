#............................
# Births 2012 Analysis for EPID 799C
# Mike Dolan Fliss, August 2017
#............................
# Aim: Does early prenatal care (<month 5) decrease the risk of preterm (<week 37) birth? # (HW1.Q1)
#............................
# Project Notes:
# Birth Certificate Revisions: https://www.cdc.gov/nchs/nvss/vital_certificate_revisions.htm 
#............................
 
#............................
# Libraries, directories, load data ####
#............................
# May need to install these packages in advance on your local machine.
# install.packages("tableone")
library(tidyverse) # for ggplot, dplyr in HWX
library(lubridate) # for dates in HWX
library(tableone) # used in HWX
library(GGally) # for optional Q on HW1

data_dir = paste0(getwd(), "/data")
output_dir = paste0(getwd(), "/data")
# map_dir = paste0(data_dir, "/GIS") # used later

# (HW1.Q2)
births = read.csv("data/births2012.csv", stringsAsFactors = F)
head(births)
#births = read.csv("births2012_small.csv", stringsAsFactors = F) #start with small version
names(births) = tolower(names(births)) #drop names to lowercase
#..................................................
# (HW1.Q3) Notice all these nice blocks?  I like to 
# have a header at the top, and block comments at the bottom.
#..................................................


# ......................................
# Exploring Data (class only)
# ......................................
## (HW1.Q4) 
dim(births)
str(births[,c("mage", "wksgest", "mdif")])
summary(births[,c("wksgest", "mdif")])
## (HW1.Q5) 
births_sample = births[1:10000, c("mage", "mdif", "visits", "wksgest", "mrace")]
plot(births_sample)
births_sample = births[sample(nrow(births), 1000), c("mage", "mdif", "visits", "wksgest", "mrace")]
ggpairs(births_sample) # later I'll demo how to use color here after some recoding.
# ......................................


# ......................................
# Recode Variables: Outcome, Exposure & Covariates #### (HW1)
# ......................................
## (HW1.Q6) 

# Date variables
births$dob_d = ymd(births$dob) # leave dobmonth, day, year as is.

### A. Outcome variables - to do later: double check with Sara!

### Gestational Age: weeks (716.A1.1)
hist(births$wksgest) #quick look
table(births$wksgest, useNA = "always") #table it
sd(births$wksgest)
quantile(births$wksgest, probs=c(.05,.95))
births$pwk = ifelse(births$wksgest>37, 17, births$wksgest-19.5) #A3.1 pwk - person time at risk.
table(births$wksgest, births$pwk) #TODO - this isn't right. See the 17.5 record.

# preterm variable - (716.A4.1) weird order here.
hist(births$wksgest)
births$hx_preterm = births$ppb #already exists as a hx of preterm birth.
table(cut(births$wksgest, breaks = c(0,19,36,99), labels =  c("too young", "preterm", "term"))) #should be no births too young at this point.
births$preterm_f = cut(births$wksgest, breaks = c(19,36,99), labels =  c("preterm", "term")) #should be no births too young at this point.
table(births$wksgest, births$preterm_f)
births$preterm = ifelse(births$preterm_f=="term", 0, 1) #as int, might use later.

### Prenatal / pnc5
summary(births$visits) # (716.A1.B1.1)
births$visits[births$visits==99] = NA
table(births$visits, useNA="always") #Looks ok
hist(births$visits)
births$mdif[births$mdif %in% c(99)] = NA # (716.A1.B2.1) pnc5
births$pnc5 = ifelse(births$mdif<=5, 1, 0) #NA carries forward
table(births$pnc5, births$mdif, useNA = "always")
births$pnc5_f = factor(births$pnc5, levels=0:1, labels = c("No PNC before 5 mo", "PNC starts in first 5 mo"))

### SMOKING - NOTE: We used to have CIGBEF and CIGDUR, not sure what happened. (716.A1.C9)
births$smoker = births$cigbef # retain old variable
births$smoker[births$smoker=="U"] = NA
births$smoker = ifelse(births$smoker == "N", 0, 1)
births$smoker_f = factor(births$smoker, levels=0:1, labels=c("Non smoker", "Smoker"))
table(births$smoker_f, useNA="always")

### sex (716.A1.C10)
table(births$sex)
births$sex[births$sex==9] = NA
births$sex = factor(births$sex, levels=1:2, labels=c("Male", "Female"))
table(births$sex)

### Race-eth: FIX Back out of using this formatter.
# Typically I might save these recodes in a csv file rather than have constants in my code...
formatter = read.csv("R format helper 2012.csv", stringsAsFactors=F) 
formatter[formatter$variable=="mrace",]
births$race_f = factor(births$mrace, levels = 1:4, 
                       labels=formatter[formatter$variable=="mrace",]$recode, 
                       ordered = T) # a few ways to do this.
births$raceeth = ifelse(births$mrace == 1 & births$methnic == "N", "WnH", 
                        ifelse(births$mrace == 1 & births$methnic == "Y", "WH", 
                               ifelse(births$mrace==2, "AA", 
                                      ifelse(births$mrace==3, "AI/AN", "Other"))))
births$raceeth_f = factor(births$raceeth, levels=c("WnH", "AA", "WH", "AI/AN", "Other"))
table(births$race_f, births$methnic, births$raceeth_f)

births$mage[births$mage == 99] = NA

table(births$wksgest)
births$wksgest[births$wksgest==99] = NA # Very common syntax
# ......................................
# NOTES: 
# ......................................

# ......................................
# Eligibility criteria & core recoding  (HW2 part 1)
# ......................................
# Create many inclusion variables, all of which have to be =1 to be included.
births$weeknum = week(births$dob_d) #thanks, lubridate! (HW2.Q1)
# 1. Has gestation data (weeks), and at least 20 weeks gestation pre-birth (weeks >= 20)
births$incl_hasgest = as.numeric(!is.na(births$wksgest)) #(HW2.Q2a)
births$incl_enoughgest = as.numeric(births$wksgest >= 20) #(HW2.Q2b)
# 2. Less than 20 weeks gestation before Jan 1 of year (LMP > Aug 20), or weeks-weeknum>=19, w/ weeknum=1 for Jan 1-7
births$incl_lateenough = as.numeric(births$wksgest - births$weeknum <= 19) #(HW2.Q2c)
# 3. Start date of gestation was 45 (max gest in'11) w prior to Jan 1 of next year, so all births are observed in year
births$incl_earlyenough = as.numeric(births$weeknum - births$wksgest <=7) #(HW2.Q2d)
# 4. singletons only
births$incl_singleton = as.numeric(births$plur == 1) #(HW2.Q2e)
# births$incl_hasnumdata = as.numeric(births$plur != 9) #not bothering to recode is ok. Isn't this one redundant with above?
# 5. no congential anomalies - Let's use apply for efficiency
congen_anom = c("anen","mnsb","cchd", "cdh", "omph","gast", "limb", "cl","cp","dowt","cdit", "hypo") #(HW2.Q3a)
births$incl_hasanomdata = as.numeric(apply(births[,congen_anom]!="U", FUN=all, 1)) #All not missing = not any missing
births$incl_noanomalies = as.numeric(apply(births[,congen_anom]=="N", FUN=all, 1)) #All not present #(HW2.Q3b)

grepl("a", c("banana", "peach", "ornge"))
names(births)
eligibility_drops = nrow(births) - apply(births[,grepl("incl_", names(births))], FUN=sum, MARGIN = 2, na.rm=T)
eligibility_drops #(HW2.Q4b optional)

births$include_allpass = apply(births[, grepl("incl_", names(births))], FUN=all, MARGIN = 1) #(HW2.Q3c)
old_births = births #Small enough dataset that we might like to hold it. births=old_births #(HW2.Q4a)
births = births[births$include_allpass, ]

warnings() # Just letting me know I'm casting those 1s as TRUEs. That's ok.

# Results of exclusion
cat("Leaving eligibility checks with n =", formatC(nrow(births), big.mark = ","), 
    "births of original", formatC(nrow(old_births), big.mark = ",")) #(HW2.Q4d)
names(births)
vars_of_interest = c("pnc5_f", "preterm_f", "smoker_f", "wksgest", "sex", "meduc", "raceeth_f", "weeknum", "include_allpass")
CreateTableOne(data=old_births[, vars_of_interest]) #(HW2.Q5)
CreateTableOne(data=old_births[, vars_of_interest], strata="include_allpass")
# ......................................
# NOTES: Finishes with 62,370 of 122,513 on 8/5/2017. Double check with Sarah.
# ......................................
# dplyr: https://blog.rstudio.org/2016/06/27/dplyr-0-5-0/
# ......................................


# ......................................
# Explore data (HW2 part 2)
# ......................................
#A1.2: Graph weeks
npop = formatC(sum(!is.na(births$wksgest)), big.mark=',') #(HW2.Q6a)
ggplot(data=births, aes(x=wksgest, y=..prop..))+geom_bar() + #Does what you need, as does geom_histogram()
  labs(title="Figure 1: Proportional distribution of gestational age at birth", 
       subtitle=paste0("From ", npop, " births in NC in 2003 from the NC SCHS, posted at Odum Institute"), 
       x="Weeks of gestation", y="Proportion of population") + 
  theme_bw()

#A2.1 : Working with weeknum. - should double check this. Just used %V above.
hist(births$weeknum, breaks = min(births$weeknum):max(births$weeknum)) # Looks weird

#A2.2 : Graph weeknum #(HW2.Q6b)
ggplot(births, aes(x=weeknum, y=..prop..)) + geom_bar() +
  labs(title="Throwaway: Investigating Weeknum", 
       subtitle="Note to self : Weeknum needs some work, doesn't quite match SAS", 
       x="Week number of birth", y="Proportion of population") +
  theme_bw()

#A2.3 : weeks vs. weeknum #(HW2.Q6c)
# Sampling here for R markdown....
ggplot(births[sample(1:nrow(births), 1000),], aes(x=wksgest, y=weeknum)) + 
  geom_jitter(width=1, height=1, pch=".", alpha=0.3)+
  labs(title="Throwaway: week vs. weeknum", 
       subtitle="Note to self: Does this match what we expect given the inclusion criteria?", 
       x="weeks of gestation", y="week number of birth in year")+
  theme_bw()


# Print some useful Table 1 versions
table(births$mage)
births$mage_cat_f = cut(births$mage, seq(9, 59, by=10))
table(births$mage, births$mage_cat_f)
      
#Outcome strata - preterm
t1=CreateTableOne(data=births[,c("pnc5_f", "preterm_f", "smoker","sex", "race_f", "wksgest", "mage", "meduc")], 
                  strata=c("preterm_f"), includeNA = T, argsNonNormal = c("wksgest"))
print(t1,showAllLevels=T )
# Thinking ahead to EMM: Race/eth variable
t2=CreateTableOne(data=births[,c("pnc5_f", "preterm_f", "smoker","sex", "race_f", "wksgest", "mage", "meduc")], 
                  strata=c("race_f"), includeNA = T, argsNonNormal = c("wksgest"))
print(t2, showAllLevels=T) #print.TableOne # let's see the guts.
# Mage - thinking ahead to functional form at least...
t3=CreateTableOne(data=births[,c("pnc5_f", "preterm_f", "smoker","sex", "race_f", "wksgest", "mage", "meduc", "mage_cat_f")], 
                  strata=c("mage_cat_f"), includeNA = T, argsNonNormal = c("wksgest"))
print(t3) #print.TableOne # let's see the guts.

#How to get this to excel... could be improved
write.table(print(t2, showAllLevels=T, quote = T), "clipboard", sep="\t") 
# ......................................


# ......................................
# Functional Form of maternal age w/ dplyr and ggplot2 #(HW3 part 1)
# ......................................
mage_df = births %>% group_by(mage) %>%
  summarize(n=n(), 
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T))
head(mage_df)

ggplot(mage_df, aes(mage, pct_preterm))+
  geom_point(aes(size=n))+
  geom_smooth(aes(weight=n), color="blue")+
  geom_smooth(aes(weight=n), method="lm", formula=y ~ poly(x, 2), color="red", lty=2)+
  labs(title="% Preterm vs. Maternal Age", 
       x="maternal age", y="% preterm", 
       subtitle="Investigating function form of maternal age", 
       caption="Note for future glms: Seems quadratic. Blue is loess, red is square linear.")
# ......................................


# ......................................
# County specific stories w/ dplyr and ggplot2 #(HW3 part 2)
# ......................................
# Load format helper
format_helper = read.csv("birth format helper 2012.csv", stringsAsFactors = F)

county_data = format_helper[format_helper$variable == "cores",] #base R way
names(county_data) = c("variable", "cores", "county_name", "FIPS")
county_data$var = NULL #drop in base R. There are other ways...
county_data$cores = as.numeric(county_data$cores)

head(format_helper) #dplyr way
county_data = format_helper %>% 
  filter(variable == "cores") %>%
  rename(cores=code, county_name=recode, FIPS=helper) %>% 
  select(-variable) %>%
  mutate(cores = as.numeric(cores))


# Load tier data 
# https://www.nccommerce.com/research-publications/incentive-reports/county-tier-designations 
county_tiers = read.csv("county_tiers.csv", stringsAsFactors = F) # shell.exec("county_tiers.csv")
county_tiers$econ_tier = ordered(county_tiers$econ_tier)
county_tiers$county_name %in% county_data$county_name

county_df = births %>% group_by(cores) %>%
  summarize(n=n(), 
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T)) %>%
  left_join(county_data) %>% 
  left_join(county_tiers)
head(county_df)

# Create a plot
ggplot(county_df, aes(x=pct_earlyPNC, y=pct_preterm, color=econ_tier))+
  geom_point(aes(size=n))+geom_text(aes(label=county_name), nudge_y=.01, alpha=0.5)+
  geom_smooth(se=F)

# Practice gather
county_name_ord = factor(county_df$county_name, county_df$county_name[order(county_df$pct_earlyPNC)], ordered=T)
county_df %>% mutate(county_name_ord = county_name_ord) %>%
  gather(key=variable, value=value, n, pct_earlyPNC, pct_preterm) %>% head()
  ggplot(aes(county_name_ord, value, fill=econ_tier))+geom_col()+coord_flip()+facet_wrap(~variable, scales = "free_x")
# ......................................

# ......................................
# D Missing data summary ####
# ......................................
# See above for CreateTableOne, includeNAs = T
# install.packages("mice")
library(mice) #A multiple imputation package
md.pattern(births) #which variables matter here?
CreateTableOne(data=births[,c("pnc5", "race_f", "smoker","sex", "preterm_f")], strata=c("preterm_f"),
               includeNA = T, argsNonNormal = c("weeks"))
# https://github.com/kaz-yos/tableone/issues/22#issuecomment-320535295
# weeks, prenatal, race, hispmom, cigdur, sex, mage, idnum, weeknum
# See VIM package, missing by map, etc.
# http://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/


# ......................................
# Graphs
# ......................................
county_data = births %>% 
  group_by(cores) %>%
  summarise(n = n(), preterm=sum(preterm, na.rm=T), pnc5=sum(pnc5, na.rm=T)) %>%
  mutate(pct_pnc5 = pnc5/n*100, pct_preterm=preterm/n*100)

county_data = merge(formatter[formatter$variable == "cores",] %>% mutate(code = as.numeric(code)), 
                    county_data, by.x="code", by.y="cores") %>% rename(name=recode)

head(county_data)
ggplot(county_data, aes(pct_pnc5, pct_preterm, size=n, weight=n, label=name))+
  geom_point(aes(color=name))+geom_smooth(se=T, show.legend = F)+geom_text(aes(color=name), nudge_y = .5)+
  labs(title="% Preterm Birth vs. % Recieving Early Prenatal Care", x="% Early PNC", y="% Preterm Birth")+
  guides(color=F)

library(plotly); ggplotly()         
ggplot2::last_plot() + scale_x_continuous(limits=c(85,95))+scale_y_continuous(limits=c(5,15))
# a bivariate choropleth would work too.
# ......................................


#............................................
# Model Playground
#............................................
# Crude effects #HW4.Q1
table(births$pnc5_f, births$preterm_f)
prop.table(table(births$pnc5_f, births$preterm_f), margin = 1) #risks
pt = prop.table(table(births$pnc5_f, births$preterm_f), margin = 1) 
pt[2,1]-pt[1,1] # RD -0.0335764
pt[2,1]/pt[1,1] # RR 0.7374673
(pt[2,1]/pt[1,1])/(pt[2,2]/pt[1,2]) # OR 0.7101271
# For tabular effects, see epi2by2 and epitools packages... or not.


# Thinking ahead to EMM #HW4.Q2
pt2 = prop.table(table(births$pnc5_f, births$preterm_f, births$raceeth_f), 
                 margin = c(1,3)) #risks. Note we're getting into dplyr is better territory...
pt2[2,1,]-pt2[1,1,] #Crude RDs
pt2[2,1,]/pt2[1,1,] # RRs
(pt2[2,1,]/pt2[1,1,])/(pt2[2,2,]/pt2[1,2,]) # ORs
pt2[1,2,] # term births, no PNC

# ggplot #HW4.Q3
pt2_df = data.frame(pt2); names(pt2_df) = c("exposure", "outcome", "group", "estimate")
ggplot(data=pt2_df[pt2_df$outcome == "preterm",], 
       aes(x=exposure, y=estimate, color=group, group=group))+
  geom_point()+geom_path()+geom_hline(yintercept=0.125)+
  annotate("text", x=.7, y=.13, label="EMM note line")+
  labs(title="Quick look at Crude EMM of RD by Race-Ethnicity", x= "Prevalence of Preterm Birth",
       subtitle="Note possible EMM in the angle of the lines... \n ...and disparities more impactful than our intervention. ")

#RD model # HW4.Q4
m_crude_rd = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="identity")) 

# factor referrent matters! #HW4.Q5
# births$preterm_f = relevel(births$preterm_f, ref = "preterm")
births$preterm_f = relevel(births$preterm_f, ref = "term")
m_crude_rd = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="identity")) 

# Model object structure HW4.Q6
str(m_crude_rd)
str(m_crude_rd, max.level = 1)

# HW4.Q7
coef(m_crude_rd)
confint(m_crude_rd)

# Clean outputs for graphing HW4.Q4e
m_crude_results = data.frame(model="M1: Crude", cbind(broom::tidy(m_crude_rd), confint(m_crude_rd)), stringsAsFactors = F)[2,]
str(m_crude_results)

# Or for the fancy, create a list of models and use lapply to extract what you need for ggplot...

# Fuller models
births$mage_sq = births$mage^2 # Can also use I(), or poly() or polym(), but be mindful...
model_vars = c("pnc5_f", "preterm_f", "raceeth_f", "smoker_f", "mage", "mage_sq") #in consideration from DAG
model_data = births[,model_vars]

m_plussmoke_rd = glm(data=births, preterm_f ~ pnc5_f + smoker_f, family=binomial(link="identity")) 
model_results = rbind(m_crude_results, data.frame(model="M2: M1+smoking", cbind(broom::tidy(m_plussmoke_rd), confint(m_plussmoke_rd)), stringsAsFactors = F)[2,])

m_plusmage_rd = glm(data=births, preterm_f ~ pnc5_f + smoker_f + mage, family=binomial(link="identity")) 
model_results = rbind(model_results, data.frame(model="M3: M2+mage", cbind(broom::tidy(m_plusmage_rd), confint(m_plusmage_rd)), stringsAsFactors = F)[2,])

m_plusmagesq_rd = glm(data=births, preterm_f ~ pnc5_f + smoker_f + mage + mage_sq, family=binomial(link="identity")) 
model_results = rbind(model_results, data.frame(model="M4: M3+mage_sq", cbind(broom::tidy(m_plusmagesq_rd), confint(m_plusmagesq_rd)), stringsAsFactors = F)[2,])

ggplot(model_results, aes(model, estimate,color=std.error, fill=std.error))+
  geom_linerange(aes(ymin=X2.5.., ymax=X97.5..), size=1)+
  geom_point(shape=15, size=4, color="white")+ geom_point(shape=15)+
  scale_y_continuous(limits=c(NA,0))+
  labs(title="Model results", subtitle="Mirroring tufte boxplot aesthetics, see ggthemes::geom_tufteboxplot()")+
  ggthemes::theme_tufte()

write.table(model_results, "clipboard", sep="\t", row.names = F)
# or write csv and shell.exec
write.csv(model_results, "births_table2.csv", row.names = F)
# shell.exec("births_table2.csv")

# RR and OR models #HW4.Q10
m1_rr = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="log")) #RR model
exp(coef(m1_rr))
m1_or = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="logit")) #OR model
exp(coef(m1_or))


# Model comparison HW4.Q11
model_data = na.omit(model_data) # also see complete.cases() for T/F vector. Nice for model comparisons where nesting is required.
m_crude_rd = glm(data=model_data, preterm_f ~ pnc5_f, family=binomial(link="identity")) 
m_plussmoke_rd = glm(data=model_data, preterm_f ~ pnc5_f + smoker_f, family=binomial(link="identity")) 
anova(m_crude_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

m_plusmage_rd = glm(data=model_data, preterm_f ~ pnc5_f + smoker_f + mage, family=binomial(link="identity")) 
anova(m_plusmage_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

m_plusmagesq_rd = glm(data=births, preterm_f ~ pnc5_f + smoker_f + mage + mage_sq, family=binomial(link="identity")) 
anova(m_plusmagesq_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

# Model predictions
# str(mod1)
# mod1 = glm(data=births, visits ~ mage, family=binomial(link="identity"))
# head(new_df)
# new_df = mod1$model; new_df$preds = predict(mod1) #could give new data in here. One way to get contrasts?
# predict_df = data.frame(mage = c(10, 20, 30))
# predict(mod1, predict_df, se.fit = T)
# head(new_df)
# ggplot(new_df, aes(mage, visits)) + 
#   geom_jitter(alpha=0.1)+
#   geom_smooth()+
#   geom_line(aes(y=preds), color="red", lty=2)
# preds_obj = predict(mod1, se.fit = T)


#............................................
# I'd like to add g-formula in the future... as well as bias-variance trade offs
#............................................

#............................................
# EMM / final model
#............................................
glm(data=births, preterm_f ~ pnc5_f + smoker_f, family=binomial(link="identity")) #Discussion of model spec

head(births)
table(births$race_f, births$methnic) # note my "ethnicity" coding may not be ideal.
mfull_emm_rd = glm(data=births, 
                   preterm_f ~ pnc5_f * raceeth_f + 
                     smoker_f + mage + mage_sq, 
                   family=binomial(link="identity"))

summary(mfull_emm_rd)
table(interaction(births$pnc5_f, births$raceeth_f)) # : = interaction()
broom::tidy(mfull_emm_rd)

# Point estimates by hand, a few ways
mfull_emm_rd$coefficients[2] #WnH
RD_WnH = mfull_emm_rd$coefficients["pnc5_fPNC starts in first 5 mo"] #WnH
RD_WnH + mfull_emm_rd$coefficients["pnc5_fPNC starts in first 5 mo:raceeth_fAA"] #AA
sum(mfull_emm_rd$coefficients[c(2,10)]) # AA
sum(mfull_emm_rd$coefficients[c(2,11)]) # WH
sum(coef(mfull_emm_rd)[c(2,12)]) # AI/AN
sum(coef(mfull_emm_rd)[c(2,13)]) # Other


C(births$raceeth_f) # get or set contrasts
contrasts(births$raceeth_f) # Let's look: contrast matrix
# See contr.treatment or contr.sum

# Contrasts using contrast package
library(contrast)
contrast(mfull_emm_rd, # demo one-level
         a=list(pnc5_f = "PNC starts in first 5 mo", raceeth_f = "AA", smoker_f = "Non smoker", mage=0, mage_sq=0),
         b=list(pnc5_f = "No PNC before 5 mo", raceeth_f = "AA", smoker_f="Non smoker", mage=0, mage_sq=0))

raceeth_diff = contrast(mfull_emm_rd, 
         a=list(pnc5_f = "PNC starts in first 5 mo", raceeth_f = levels(births$raceeth_f), smoker_f="Non smoker", mage=0, mage_sq=0),
         b=list(pnc5_f = "No PNC before 5 mo", raceeth_f = levels(births$raceeth_f), smoker_f="Non smoker", mage=0, mage_sq=0))

print(raceeth_diff, X=T)
str(raceeth_diff)
EMM_df = data.frame(model=paste0("M5: ", raceeth_diff$raceeth_f), 
                    estimate=raceeth_diff$Contrast, std.error = raceeth_diff$SE,
                   X2.5..=raceeth_diff$Lower, X97.5..=raceeth_diff$Upper, stringsAsFactors = F)
EMM_df
ggplot(bind_rows(model_results,EMM_df), aes(model, estimate,color=estimate, fill=estimate))+
  geom_linerange(aes(ymin=X2.5.., ymax=X97.5..), size=1)+
  geom_point(shape=15, size=4, color="white")+ geom_point(shape=15)+
  scale_y_continuous(limits=c(NA,NA), breaks=seq(-0.15, 0.05, 0.01))+
  geom_hline(yintercept = 0, lty=2, color="grey")+
  geom_vline(xintercept = 4.5, lty=2, color="blue")+
  annotate(geom = "text", x = 4.6, y=-0.05, angle=90, label="M5: Race-Ethnicity EMM: Stratum-Specific RDs")+
  labs(title="Model results", subtitle="Mirroring tufte boxplot aesthetics, see ggthemes::geom_tufteboxplot()")+
  ggthemes::theme_tufte()

# head(predict.glm(mfull_emm_rr, type="link"))
# head(predict.glm(mfull_emm_rr, type="response"))
# head(predict.glm(mfull_emm_rr, type="terms"))
# ??model.matrix.default

# Contrasts - exploring different contrast matrix
# births$raceeth_f2 = births$raceeth_f
# contrasts(births$raceeth_f2) = contr.sum
# births$pncf_f_raceeth_f = interaction(births$pnc5_f,births$raceeth_f)
# births$re_int = ifelse()
# table(births$pncf_f_raceeth_f)
# mfull_emm_rd2 = glm(data=births, 
#                    preterm_f ~ pncf_f_raceeth_f + 
#                      smoker_f + mage + mage_sq, 
#                    family=binomial(link="identity"))
# broom::tidy(mfull_emm_rd2)


# Backwards selection demo - In Epi we don't do this, but... can backwards select using fit. Again, take 718.
# library(MASS)
# stepAIC(m_plusmagesq_rd, direction="both")
# library(leaps)
# leaps = regsubsets(m_plusmagesq_rd$formula, data=m_plusmagesq_rd$model)
# plot(leaps)
# plot(leaps, scale="adjr2")
#............................................
# http://www.goldsteinepi.com/blog/epivignettesinteractionandeffectmodification
# https://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
#............................................

#............................................
# Table Outputs
#............................................
write.csv(bind_rows(model_results,EMM_df), 
          "table2_birth_EMM_df.csv", row.names = F)
# shell.exec("table2_birth_EMM_df.csv")
write.table(bind_rows(model_results,EMM_df), 
            "clipboard", sep="\t", quote=F, row.names = F) # Table 2

t1=CreateTableOne(data=births[,c("pnc5_f", "preterm_f", "smoker_f","sex", "raceeth_f", "wksgest", "mage", "meduc")], 
                  includeNA = T, argsNonNormal = c("wksgest"))
print(t1, showAllLevels=T) #print.TableOne # let's see the guts.
write.table(print(t1, showAllLevels=T, quote = T), "clipboard",   # Table 1A
            sep="\t", quote = F) 

t2=CreateTableOne(data=births[,c("pnc5_f", "preterm_f", "smoker_f","sex", "raceeth_f", "wksgest", "mage", "meduc")], 
                  strata=c("raceeth_f"), includeNA = T, argsNonNormal = c("wksgest"))
print(t2, showAllLevels=T) #print.TableOne # let's see the guts. 
write.table(trimws(print(t2, showAllLevels=T, quote = F)), "clipboard", sep="\t", quote = F)  # Table 1B

# yuck. leading and tailing spaces? trimws can help

county_data %>% 
  bind_rows(births %>% 
              summarise(n = n(), preterm=sum(preterm, na.rm=T), pnc5=sum(pnc5, na.rm=T)) %>%
              mutate(pct_pnc5 = pnc5/n*100, pct_preterm=preterm/n*100) %>%
              mutate(code=NA, variable="cores", name="North Carolina", helper=37)
    ) %>%
  write.table("clipboard", sep="\t", row.names = F)
  
#............................................

save.image("birth_results.RData")

#............................................
# Bias variance trade off...
#............................................


# ......................................
# Assignments # This section is YUCK!!!! ####
# ......................................
### A1
hist(births$weeks)
### A2
#A1.1
#Make a, b n table.
table(births$preterm_f, births$pnc5_f)
#calculate risks, CIs
#install.packages("epicalc")
library(epiR) #for epi2x2
library(epitools) #for epitab()
#library(epicalc) #Busted for newer R versions
library(Epi)
#http://www.inside-r.org/packages/cran/epiR/docs/epi.2by2
findings = function(e,o,s){
  #Takes t as top=D-, D+; left=E-, E+   #Convert to top=D+, D1; left=E+, E1
  #  t2 = matrix(c(t[2,2], t[2,1], t[1,2], t[1,1]), nrow = 2, byrow = T)
  
  l = unique(s)
  for(strata in l){
    # e = births$pnc5_f; o = births$preterm_f
    t = table(e[s %in% strata], o[s %in% strata])
    cat("\n\nStrata=", strata, "\n")
    print(t)
    #e = epi.2by2(t, units=1) #frustratingly confusing.
    R0=t[2,2]/(t[2,2]+t[2,1])
    cat("\nRisk in Exposed:", R0, "\n")
    R1=t[1,2]/(t[1,2]+t[1,1])
    cat("Risk in Unexposed:", R1, "\n")
    R = (t[1,2]+t[2,2])/(sum(t))
    cat("Risk Overall:", R, "\n")
    #cat("Prevalence of outcome")
    cat("RD: ", R1-R0) #update here  
  }
}

findings(births$pnc5_f, births$preterm_f, births$race_f)
#ftable(xtabs(cbind(births$pnc5, births$preterm) ~ births$race))
t = table(factor(births$pnc5_f,levels = rev(levels(births$pnc5_f))),
          factor(births$preterm_f,levels= rev(levels(births$preterm_f)))) # wrong.
t
e = better.table(t)
r0 = 357/(357+2120) #14.4% no pnc
r1 = 6582/(6582+52205) #11.2% w/ pnc
r1-r0
str(e)
unique(births$race_f)
xtabs(data=births, pnc5_f ~ preterm_f + race_f)
dat <- matrix(c(13,2163,5,3349), nrow = 2, byrow = TRUE)
rownames(dat) <- c("DF+", "DF-"); colnames(dat) <- c("FUS+", "FUS-"); dat
as.table(dat)
table(births$pnc5_f)
table(factor(births$pnc5_f,levels = rev(levels(births$pnc5_f))))
t = table(births$pnc5_f[births$race_f=="White"],births$preterm_f[births$race_f=="White"]) # wrong.
t = table(births$pnc5_f[births$race_f=="Black"],births$preterm_f[births$race_f=="Black"]) # wrong.
t = matrix(c(t[2,2], t[2,1], t[1,2], t[1,1]), nrow = 2, byrow = T)
e=epi.2by2(t, outcome = "as.rows", units=1) #frustratingly confusing.
epitab(t)
#make incident table.
#............................................


#............................................
## Clusters & Trees ####
#............................................
# kmeans
to_model = births[,c("pnc5_f", "preterm_f", "smoker", "race_f", "cores", "mage")]
to_model = na.omit(to_model)
head(to_model)
scale_01 = function(x){
  x = as.numeric(x); 
  x = (x-min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  return(x)
}
to_model_01 = data.frame(lapply(to_model, FUN=scale_01)) #note why apply doesn't work - matrix first
summary(to_model_01)
km = kmeans(to_model_01[,names(to_model_01) != "preterm_f"], 2)
to_model$cluster = km$cluster
table(to_model$cluster, to_model$preterm_f)
prop.table(table(to_model$cluster, to_model$preterm_f), margin = 2) #meh. cluster 1 is a little more preterm. 

km2 = kmeans(to_model_01[,names(to_model_01) != "preterm_f"], 20)
to_model$cluster2 = km2$cluster
prop.table(table(to_model$cluster2, to_model$preterm_f), margin = 1)*100 #meh. cluster 1 is a little more preterm. 

to_model %>% group_by(cluster2) %>% 
  summarise(n = n(), pct_preterm=sum(preterm_f=="preterm", na.rm=T)/n)
# https://datascience.stackexchange.com/questions/22/k-means-clustering-for-mixed-numeric-and-categorical-data?newreg=e85b257b6b524d768e3a8cff706840eb
#............................................
library(rpart)
library(rattle)

names(to_model)
dim(to_model)
glm1 = glm(data = to_model, preterm_f ~ pnc5_f + smoker + race_f + mage, family=binomial("logit"))
to_model$glm1_pred = predict(glm1, to_model, "response")
table(to_model$glm1_pred, to_model$preterm_f)

str(to_model)
tree1 = rpart(data = to_model, preterm_f ~ pnc5_f + smoker + race_f +mage, method="class", parms=list(split="information"),
              control=rpart.control(minsplit=2, minbucket=1, cp=0.0001)) #no interaction terms, though. dropped cores. need minbucket.
summary(tree1)
plot(tree1)
fancyRpartPlot(tree1)
to_model$rpart_pred = predict(tree1, to_model[,c("pnc5_f","smoker","race_f", "mage")], "class")
to_model$rpart_pred
table(to_model$rpart_pred, to_model$preterm_f)
table(to_model$rpart_pred)
# give each the same half, then predict and compare accuracy?
#............................................



#............................................
## Map makin' ####
#............................................
# Add tmap:: library(sp) #for spatial objects
library(sp)
library(rgdal) #for reading and writing shapefiles
library(rgeos) #for spatial tools like gCentroid
library(RColorBrewer) #For better, much better color ramps
#library(maps) # http://dmcglinn.github.io/quant_methods/lessons/shapefiles_and_rasters.html
library(tmap)

head(county_data) #formatter[formatter$variable=="cores",]

getwd()
if(!file.exists("./maps/nc counties.shp")){ #Run Once to get NC Counties.
  dir.create("./maps/")
  # ftp://ftp2.census.gov/geo/tiger/TIGER2016/ or 
  # https://www.census.gov/cgi-bin/geo/shapefiles/index.php
  download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2015/COUNTY/tl_2015_us_county.zip", "./maps/uscounties.zip")
  unzip("./maps/uscounties.zip", exdir="./maps")
  dir("./maps/")
  us_counties = readOGR(dsn="./maps", layer="tl_2015_us_county", stringsAsFactors = F)
  plot(us_counties)
  nc_counties = us_counties[us_counties$STATEFP == 37,] #look how slick this is.
  writeOGR(nc_counties, dsn="./maps", layer="nc counties", driver="ESRI Shapefile", overwrite_layer = T)
}
nc_counties = readOGR(dsn="./maps", layer="nc counties", stringsAsFactors = F)
class(nc_counties)
plot(nc_counties)
str(nc_counties, max.level = 2)
# ... or us tigris package. Also see acs package

#remember to merge with the spatial object, not just the data slot, to keep it from reordering
nc_counties = merge(nc_counties, county_data, by.x="GEOID", by.y="helper")

spplot(nc_counties, "pct_pnc5") # on spplot: https://edzer.github.io/sp/
spplot(nc_counties, c("pct_pnc5", "pct_preterm")) # on spplot: https://edzer.github.io/sp/
writeOGR(nc_counties, dsn="./maps", layer="nc counties w data", driver="ESRI Shapefile", overwrite_layer = T)

sum(nc_counties@data$n)
sum(nc_counties[nc_counties$NAME %in% c("Orange", "Durham", "Chatham"),]$n)


### Using base plot()
display.brewer.all()
nc_counties$pct_preterm_colors = as.character(cut(nc_counties$pct_preterm, 10, labels = brewer.pal(10,"RdYlGn")))
plot(nc_counties, co=nc_counties$pct_preterm_colors, main="% Preterm birth")
county_centroids = gCentroid(nc_counties, byid = T)
text(county_centroids@coords, labels=as.character(nc_counties$NAME), cex=0.4)

### Using spplot()
spplot(nc_counties, "pct_pnc5") #ugh. gorgeous
nc_counties$pct_pnc5_f = cut(nc_counties$pct_pnc5, 5)
spplot(nc_counties, "pct_pnc5_f", 
       col.regions=brewer.pal(5,"RdYlGn"), 
       main="% of Births with Early Start of Prenatal Care (<5mo)")

### Using ggplot() - nice, but slow
names(nc_counties@data)
nc_counties_fort = fortify(nc_counties, region = "GEOID")
str(nc_counties_fort)
nc_counties_fort = merge(nc_counties_fort, nc_counties, by.x="id", by.y="GEOID")
#nc.counties.fort = nc.counties.fort[order(nc.counties.fort$order),]
names(nc_counties_fort)
library(ggthemes) # for theme_map
cut(nc_counties$pct_pnc5, 5)
pretty(nc_counties$pct_pnc5) #funny name
nc_counties_fort$pct_pnc5_f = cut(nc_counties_fort$pct_pnc5, breaks=pretty(pretty(nc_counties_fort$pct_pnc5)))
table(nc_counties_fort$pct_pnc5_f)
#grep to make , a -_ move to top, like 
#http://time_com/4394141/zika-abortion-usa/
mymap = 
  ggplot(nc_counties_fort, aes(x=long, y=lat, group=group, fill=pct_pnc5_f)) + 
  geom_polygon(color="white")+coord_map()+
  labs(title=paste0("Early prenatal care by county in North Carolina,", births$byear[1]), 
       subtitle="This ggplot is gorgeous, holy cow", 
       x="", y="")+
  scale_fill_brewer(name="% Early Prenatal Care", type="seq", palette ="Greens")+
  #scale_fill_gradient(name="% Early Prenatal Care", low = "white", high = "dark green")+
  theme_map()+theme(legend.position="right")
  #theme_map()+theme(legend_position="bottom")+guides(fill=guide_legend(nrow=1))#right")
mymap 
# http://novyden.blogspot.com/2013/09/how-to-expand-color-palette-with-ggplot.html
mymap+theme_minimal() #also a nice one / clean. 

# tmap
tm_shape(nc_counties)+
  tm_fill("pct_pnc5", title="% Early PNC")+
  tm_borders()+ #could add more layers with tm_shape and then tm_whatever...
  tm_layout(main.title="% Early Prenatal Care by County")

# sf
library(sf)
nc_counties_sf = sf::read_sf(dsn="./maps", layer="nc counties", stringsAsFactors = F)
str(nc_counties_sf)
plot(nc_counties_sf)

# Demos: transform, gTouches,
proj4string(nc_counties) #or nc_counties@proj4string  
nc_stateplane_proj = "+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
nc_counties_stateplane = spTransform(nc_counties, nc_stateplane_proj)

county_cents = gCentroid(nc_counties_stateplane, byid=T) #just the spatial object
county_cents_sp = SpatialPointsDataFrame(county_cents, data=nc_counties@data) #need to add back the data

plot(nc_counties_stateplane); plot(county_cents, add=T)
plot(county_cents_sp[county_cents_sp$NAME == "Orange",], col="blue", add=T, pch="0")
plot(gBuffer(county_cents, width = 10*5280, byid = T), add=T) #10 mi buffers

plot(nc_counties[county_cents_sp$NAME == "Orange",])
orange_neighbors = gTouches(nc_counties[county_cents_sp$NAME == "Orange",], nc_counties, byid = T)
str(orange_neighbors) # note it's a matrix
plot(nc_counties); plot(nc_counties[orange_neighbors[,1],], col="blue", add=T)


# make_EPSG() # or lookup

###############################################################################
# Notes
# http://www.kevjohnson.org/making-maps-in-r/
# https://www.google.com/search?q=north+carolina+counties+table&tbm=isch&tbo=u&source=univ&sa=X&ved=0ahUKEwiLnYyDxNbNAhVQzGMKHUAUD4kQsAQIHQ&biw=1536&bih=716
###############################################################################
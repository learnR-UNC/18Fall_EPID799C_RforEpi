# __Start Homework 1 ####
#............................
# Births 2012 Analysis for EPID 799C: PNC decrease PT birth? ####
# Mike Dolan Fliss, August 2017
# Aim: Does early prenatal care (<month 5) decrease the risk of preterm (<week 37) birth?
# (This Descriptive header is HW1.Q1a)
#............................ (<- example of HW1.Q1b, comment bar)
# Project Notes:
# Birth Certificate Revisions: https://www.cdc.gov/nchs/nvss/vital_certificate_revisions.htm (HW1.Q1d, clickable link)
#............................

#............................
# Libraries, directories, load data (HW1.Q1f) ####
#............................
# NOTE: May need to install these packages in advance on your local machine.
# install.packages("tableone")
library(tidyverse) # for ggplot, dplyr in HW1-4 (<- example of a post-line code comment, HW1.Q1c)
library(lubridate) # for dates in HW2
library(tableone) # used in HW2
# Set directories.
# Mike's home dir: setwd("D:/User/Dropbox (Personal)/Education/Classes/18Fall_EPID799C_RforEpi/")
data_dir = paste0(getwd(), "/data")
output_dir = paste0(getwd(), "/data/output")
map_dir = paste0(data_dir, "/data/maps") # used later
# Note you'll need to set different ones with setwd()! Since we're building this as a github repo the wd defaults to script dir.
# More on git here: http://r-bio.github.io/intro-git-rstudio/ . There's also a datacamp on it.
#............................

#............................
# Read data (HW1.Q2) ####
#............................
births = read.csv("data/births2012.csv", stringsAsFactors = F) # (HW1.Q2a)
head(births) # (HW1.Q2b)
#births = read.csv("births2012_small.csv", stringsAsFactors = F) #start with small version

names(births) = tolower(names(births)) #drop names to lowercase
#..................................................
# (HW1.Q3) Notice all these nice blocks?  I like to
# have a header at the top, and block comments at the bottom.
#..................................................


# ......................................
# Exploring Data - (HW1Q3 & HW1Q4) ####
# ......................................
dim(births) # (HW1.Q3b)
summary(births[,c("wksgest", "mdif")]) # (HW1.Q3c)
str(births[,c("mage", "wksgest", "mdif")]) # (HW1.Q3d)
class(births$mage); class(births$wksgest); class(births$mdif)
str(births$mage); str(births$wksgest); str(births$mdif)

## (HW1.Q4)
births_sample = births[1:1000, c("mage", "mdif", "visits", "wksgest", "mrace")] #A
plot(births_sample) #B

#C
births_sample = births[sample(nrow(births), 1000), c("mage", "mdif", "visits", "wksgest", "mrace")] #C
car::scatterplotMatrix(births_sample)
GGally::ggpairs(births_sample) # later I'll demo how to use color here after some recoding.
# ......................................


# ......................................
# Recode Variables: Exposure, Outcome & Covariates (HW1.Q5) ####
# ......................................
## (HW1.Q5)
### A. Prenatal Care (Exposure)
#### i
table(births$mdif, useNA = "always")

#### ii
# See data dictionary, but 88 means no early prenatal care and 99 means missing

#### iii
births$mdif[births$mdif==99] <- NA

#### iv
boxplot(births$mdif[births$mdif != 88], main="Month Prenatal Care Began, \n Excluding Unkowns and No Prenatal Care")

#### v
births$pnc5 <- ifelse(births$mdif <= 5, 1, 0)

#### vi
births$pnc5_f <- factor(births$pnc5, levels=c(0,1), labels=c("No Early PNC", "Early PNC"))

#### vii
table(births$mdif, births$pnc5_f, useNA = "always")


# (HW1.Q5 continued)
### B. Preterm Birth (Outcome)
#### i
hist(births$wksgest) #quick look
#### ii
births$preterm <- ifelse(births$wksgest >= 20 &births$wksgest < 37, 1,
                         ifelse(births$wksgest >= 37 & births$wksgest < 99, 0, NA)
)

#### iii
births$preterm_f <- factor(births$preterm, levels=c(0,1), labels=c("term", "preterm"))

#### iv
births$preterm_f <- cut(births$wksgest, breaks = c(0,36,99), labels =  c("preterm", "term"))

# (HW1.Q5 continued)
### C. Plurality (covariate)
#### i
table(births$plur, useNA = "always") # looks fine, no 9 values for unknown

# (HW1.Q5 continued)
### D. Maternal Age (covariate)
#### i
table(births$mage, useNA = "always")

#### ii
# See data dictionary, but 99 means misssing

#### iii
births$mage[births$mage==99] <- NA

#### iv
par(mfrow=c(1,2)) # make our plot window 2 columns
boxplot(births$mage, main="Boxplot of Maternal Age Distribution")
plot(density(births$mage), main="Density Plot of Maternal Age Distribution")
par(mfrow=c(1,1)) # always always reset after this!

#### v
births$mage_centered <- births$mage - mean(births$mage, na.rm = T)
hist(births$mage) # looks to be expected
summary(births$mage)

# (HW1.Q5 continued)
### E. Cigarette Use (covariate)
#### i
table(births$cigdur, useNA = "always") # check to see what values are in cigdur
births$cigdur <- ifelse(births$cigdur == "Y", 1,
                        ifelse(births$cigdur == "N", 0, NA)) # NA for when case is U - i.e. unknown

#### ii
births$smoker_f <- factor(births$cigdur, levels=c(0,1), labels=c("Non-smoker", "Smoker"))

#### ii
table(births$cigdur, births$smoker_f, useNA = "always")


# (HW1.Q5 continued)
### F. Date of Birth (covariate)
#### i
# https://github.com/tidyverse/lubridate
#### ii
?lubridate
vignette("lubridate")

#### iii
library(lubridate) # normally this would go at the top of the script under a packages chunk

#### iv
births$dob_d <- lubridate::ymd(births$dob)

# (HW1.Q5 continued)
### G. Sex (covariate)
#### i
table(births$sex, useNA = "always") # check what initial values are
births$sex[births$sex==9] <- NA
births$sex_f <- factor(births$sex, levels=c(1,2), labels=c("Male", "Female"))
table(births$sex_f, births$sex, useNA="always")

# (HW1.Q5 continued)
### G. Maternal Race/Ethnicity (covariate)
#### i
table(births$mrace, births$methnic, useNA = "always") # check what initial values are

## ii
mergedf <- data.frame(
  mrace = c(sort(rep(seq(1,4),3))),
  methnic = as.character(rep(c("Y", "N", "U"), 4)),
  raceeth_f =factor(c("White Hispanic", "White non-Hispanic", "Other",
                      rep("African American", 3),
                      rep("American Indian or Alaska Native", 3),
                      rep("Other", 3)
  )
  ),
  stringsAsFactors = F)

births <- merge(x=births, y=mergedf, by=c("mrace", "methnic"))

#### iii
table(births$mrace, births$methnic, births$raceeth_f, useNA = "always") # looks good
# ......................................

# ......................................
# Optional data exploration (HW1.Q6) ####
# ......................................

# (HW1.Q6)
## A
CreateTableOne(vars = c("mage", "pnc5_f", "smoker_f", "sex_f"),
               strata = c("preterm_f"),
               data = births)

## B
mice::md.pattern(births)

## C
GGally::ggpairs(data = births[1:1000, c("pnc5_f", "preterm_f")],
                title="Exposure versue Outcome for First 1,000 Obserations")
# ......................................

# ......................................
# __End Homework 1 /  Start Homework 2 ####
# ......................................


# ......................................
# Eligibility criteria & core recoding  (HW2 part 1) ####
# ......................................
# Create many inclusion variables, all of which have to be =1 to be included.
births$weeknum = week(births$dob_d) #thanks, lubridate! (HW2.1.Q1)
# a. Has gestation data (weeks), and at least 20 weeks gestation pre-birth (weeks >= 20)
births$incl_hasgest = as.numeric(!is.na(births$wksgest)) #(HW2.1.Q2a)
births$incl_enoughgest = as.numeric(births$wksgest >= 20) #(HW2.1.Q2b)
# b. Less than 20 weeks gestation before Jan 1 of year (LMP > Aug 20), or weeks-weeknum>=19, w/ weeknum=1 for Jan 1-7
births$incl_lateenough = as.numeric(births$wksgest - births$weeknum <= 19) #(HW2.1.Q2c)
# c. Start date of gestation was 45 (max gest in'11) w prior to Jan 1 of next year, so all births are observed in year
births$incl_earlyenough = as.numeric(births$weeknum - births$wksgest <=7) #(HW2.1.Q2d)
# d. singletons only
births$incl_singleton = as.numeric(births$plur == 1) #(HW2.Q2e)
# births$incl_hasnumdata = as.numeric(births$plur != 9) #not bothering to recode is ok. Isn't this one redundant with above?
# e. no congential anomalies - Let's use apply for efficiency
congen_anom = c("anen","mnsb","cchd", "cdh", "omph","gast", "limb", "cl","cp","dowt","cdit", "hypo") #(HW2.1.Q3a)
# below is just a useful aside to check that there is No "U" (i.e. none missing) before running the code to make the new variable
births$incl_hasanomdata = as.numeric(apply(births[,congen_anom]!="U", FUN=all, 1)) #All not missing = not any missing
births$incl_noanomalies = as.numeric(apply(births[,congen_anom]=="N", FUN=all, 1)) #All not present #(HW2.1.Q3b)

# Below is the example grepl (HW2.1.Q4b)
grepl("a", c("banana", "peach", "ornge"))
eligibility_drops = nrow(births) - apply(births[,grepl("incl_", names(births))], FUN=sum, MARGIN = 2, na.rm=T) #(HW2.1.Q4c optional)
eligibility_drops #(HW2.1.Q4c optional)

births$include_allpass = apply(births[, grepl("incl_", names(births))], FUN=all, MARGIN = 1) #(HW2.1.Q4d indicator for the inclusion criteria yes/no)
old_births <- births #(HW2.1.Q4a)

births = births[births$include_allpass, ] #(HW2.1.Q4e now apply the inclusion criteria to subset your births data set)
# to see what warnings are being thrown you can type warnings() -- below:
warnings() # Just letting me know I'm casting those 1s as TRUEs. That's ok.

dim(old_births) #(HW2.1.Q4f)
dim(births) #(HW2.1.Q4f)

#(HW2.1.Q5 Optional Challenge)
# Results of exclusion
cat("Leaving eligibility checks with n =", formatC(nrow(births), big.mark = ","),
    "births of original", formatC(nrow(old_births), big.mark = ",")) #(HW2.1.Q4d)
names(births)
vars_of_interest = c("pnc5_f", "preterm_f", "smoker_f", "wksgest", "sex", "meduc", "mage", "raceeth_f", "weeknum", "include_allpass")
tableone::CreateTableOne(data=births[, vars_of_interest]) #(HW2.1.Q5)
tableone::CreateTableOne(data=old_births[, vars_of_interest], strata="include_allpass")

# Save a smaller version for future loading
vars_to_save = c(vars_of_interest, "cores", "kotel")
births_sm = births[, vars_to_save]
save("births_sm", file = "births_sm.rdata")
# ......................................
# Finishes with 62,370 of 122,513 ####
# ......................................

# ......................................
# __End Homework 2 /  Start Homework 3 ####
# ......................................

# ......................................
# Explore data (HW3 part 1) ####
# ......................................
#(HW2.2.Q6A) Graph weeks
npop = formatC(sum(!is.na(births$wksgest)), big.mark=',') #(HW2.2.Q6a)
ggplot(data=births, aes(x=wksgest, y=..prop..))+geom_bar() + #Does what you need, as does geom_histogram()
  labs(title="Figure 1: Proportional distribution of gestational age at birth",
       subtitle=paste0("From ", npop, " births in NC in 2012 from the NC SCHS, posted at Odum Institute"),
       x="Weeks of gestation", y="Proportion of population") +
  theme_bw()

#A2.1 : Working with weeknum. - should double check this. Just used %V above.
hist(births$weeknum, breaks = min(births$weeknum):max(births$weeknum)) # Looks weird

#(HW2.2.Q6B) : Graph weeknum
ggplot(births, aes(x=weeknum, y=..prop..)) + geom_bar() +
  labs(title="Throwaway: Investigating Weeknum",
       subtitle="Weeknum counts drop off at start and end of year",
       x="Week number of birth", y="Proportion of population") +
  theme_bw()

# Graph: wksgest vs. weeknum: jitter (HW3.Q6c) ####
ggplot(births, aes(x=wksgest, y=weeknum)) +
  geom_jitter(width=1, height=1, pch=".", alpha=0.3)+
  labs(title="Throwaway: week vs. weeknum",
       subtitle="Note to self: Does this match what we expect given the inclusion criteria?",
       x="weeks of gestation", y="week number of birth in year")+
  theme_bw()


# Graph: wksgest vs. weeknum bin2d HW3.2.Q6D)  ####
# Note here that you can ggplot objects that are "appendable"
# which is to say you can add on new features by adding on LAYERS
plotObj <- ggplot(births, aes(x=wksgest, y=weeknum)) +
  geom_jitter(width=1, height=1, pch=".", alpha=0.3)+
  labs(title="Throwaway: week vs. weeknum",
       subtitle="Note to self: Does this match what we expect given the inclusion criteria?",
       x="weeks of gestation", y="week number of birth in year")+
  theme_bw()
plot(plotObj)
## append new feature
plotObj + geom_bin2d(binwidth=1, alpha=0.8)

# (HW3.2.Q6D) ggridges
library("ggridges")
ggplot() +
  geom_density_ridges_gradient(data=births, aes(x=mage, y=factor(cut(wksgest, 10)), fill=..x..)) +
  scale_fill_gradientn(colors=c("#fc8d59", "#ffffbf", "#91cf60")) +
  facet_grid(meduc~raceeth_f) +
  ggtitle("Distribution of Weeks Gestation by Birth Outcome") +
  labs(subtitle="This is the continuous variable we dichotomized") +
  ylab("Weeks of Gestations factorized") + xlab("mage") +
  theme_bw()

# ......................................
# Functional Form of maternal age w/ dplyr and ggplot2 #(HW3 part 1) ####
# ......................................

mage_df = births %>% group_by(mage) %>% #(HW3.1.1) setup
  summarize(n=n(),
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T))
head(mage_df) #(HW3.1) answer

# Graph: Mage x % Preterm (HW3.1.2A)  ####
ggplot(mage_df, aes(mage, pct_preterm))+
  geom_point(aes(size=n))+
  geom_smooth(aes(weight=n), color="blue", method="loess")+
  labs(title="% Preterm vs. Maternal Age",
       x="maternal age", y="% preterm",
       subtitle="Investigating function form of maternal age",
       caption="Note for future glms: Seems quadratic. Blue is loess.")

# Graph: Mage x % Preterm v2 (HW3.1.2B) ####
ggplot(mage_df, aes(mage, pct_preterm)) +
  geom_point(aes(size=n)) +
  geom_smooth(aes(weight=n), color="blue") +
  geom_smooth(aes(weight=n), method="lm", formula=y ~ poly(x, 2), color="red", lty=2) + # this is the square error term
  labs(title="% Preterm vs. Maternal Age",
       x="maternal age", y="% preterm",
       subtitle="Investigating function form of maternal age",
       caption="Note for future glms: Seems quadratic. Blue is loess, red is square linear.")

# In the future, let's add some splines (e.g. restricted cubic)
# ......................................


# ......................................
# HW3.3: County stories ####
# ......................................

# ......................................
# County specific stories w/ dplyr and ggplot2
# ......................................
#(HW3.3.4A)
format_helper = read.csv("data/R for epi 2018 data pack/birth-format-helper-2012.csv", stringsAsFactors = F)
#(HW3.3.4A.i)
county_data = format_helper[format_helper$variable == "cores",] #base R way
#(HW3.2.4A.ii)
names(county_data) = c("variable", "cores", "county_name", "FIPS")
#(HW3.2.4A.iii)
county_data$var = NULL #drop in base R. There are other ways...
#(HW3.2.4A.iv)
county_data$cores = as.numeric(county_data$cores)
str(county_data$cores)

#(HW3.2.4B) same results as above but now using "tidy" methods and more human readable
county_data = format_helper %>%
  filter(variable == "cores") %>%
  rename(cores=code, county_name=recode, FIPS=helper) %>%
  select(-variable) %>%
  mutate(cores = as.numeric(cores))

#(HW3.2.4C)
# Load tier data
# https://www.nccommerce.com/research-publications/incentive-reports/county-tier-designations
county_tiers = read.csv("data/R for epi 2018 data pack/maps/county_tiers.csv", stringsAsFactors = F) # shell.exec("county_tiers.csv")
#(HW3.2.4C.i)
str(county_tiers$econ_tier)
#(HW3.2.4C.ii)
county_tiers$econ_tier = ordered(county_tiers$econ_tier)

#(HW3.2.4D)
county_df = as.tibble(births) %>% group_by(cores) %>%
  summarise(n=n(),
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T)) %>%
  left_join(county_data) %>%
  left_join(county_tiers)
head(county_df) #(HW3.2.3D) output


#(HW3.2.4E) write this out to your local directory
write.csv(x=county_df, "data/county_birth_summary.csv", row.names = F, quote = F)


#(HW3.2.5)
# Create a plot with ggplot
plotObj <- ggplot(county_df, aes(x=pct_earlyPNC, y=pct_preterm, color=econ_tier))+
  geom_point(aes(size=n))+geom_text(aes(label=county_name), nudge_y=.01, alpha=0.5)+
  geom_smooth(se=F)
plot(plotObj)
#(HW3.2.5a)
plotly::ggplotly(plotObj)

#(HW3.2.6A)
county_name_ord = factor(county_df$county_name, county_df$county_name[order(county_df$pct_earlyPNC)], ordered=T)

#(HW3.2.6B)
county_df <- county_df %>%
  mutate(county_name_ord = county_name_ord) %>%
  gather(key=variable, value=value, n, pct_earlyPNC, pct_preterm) %>%
  head()
#(HW3.2.6C)
county_df %>%
  ggplot(aes(county_name_ord, value, fill=econ_tier)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~variable, scales = "free_x")

# ......................................
# __End Homework 3 /  Start Homework 4 ####
# ......................................

#............................................
# Model prep: tabular crude estimates ####
#............................................
# (HW4.1.Q1) Crude effects
table(births$pnc5_f, births$preterm_f)
prop.table(table(births$pnc5_f, births$preterm_f), margin = 1) #risks
pt = prop.table(table(births$pnc5_f, births$preterm_f), margin = 1)
#(HW4.1.Q1A)
pt[2,1]-pt[1,1] # RD -0.0335764
#(HW4.1.Q1B)
pt[2,1]/pt[1,1] # RR 0.7374673
#(HW4.1.Q1C)
(pt[2,1]/pt[1,1])/(pt[2,2]/pt[1,2]) # OR 0.7101271

# (HW4.1.Q1) Increasingly advanced but more direct alternatives....
crude_risks = births %>%
  group_by(pnc5_f, preterm_f) %>%
  summarize(n=n()) %>% mutate(pct = n / sum(n)) %>%
  filter(preterm_f == "preterm", !is.na(pnc5_f))
r0 = crude_risks$pct[crude_risks$pnc5_f == "No Early PNC"]
r1 = crude_risks$pct[crude_risks$pnc5_f == "Early PNC"]
r1 - r0

births %>%
  group_by(raceeth_f, pnc5_f, preterm_f) %>%
  summarize(n=n()) %>%
  mutate(pct = n / sum(n)) %>%
  filter(!is.na(pnc5_f), preterm_f == "preterm") %>%
  group_by(raceeth_f) %>% mutate(rd = pct-lag(pct)) %>%
  filter(pnc5_f == "Early PNC")
# Confused about lag()? Try running: 1:10; lag(1:10)

rd_df = births_sm %>%
  group_by(raceeth_f, pnc5_f, preterm_f) %>%
  summarize(n=n()) %>%
  mutate(pct = n / sum(n)) %>%
  filter(!is.na(pnc5_f), preterm_f == "preterm") %>%
  select(-preterm_f, -n) %>%
  spread(pnc5_f, pct) %>%
  mutate(RD = `No Early PNC` - `Early PNC`)
# ^ Note backticks to reference variable names with spaces
rd_df

as.data.frame(table(births_sm$preterm_f, births_sm$pnc5_f, births_sm$raceeth_f))
rd_tbl = as.data.frame(prop.table(table(preterm = births_sm$preterm_f,
                                        pnc5 = births_sm$pnc5_f,
                                        raceeth = births_sm$raceeth_f),
                                  margin = c(2,3))) # Note vector of margins

# For tabular effects, see epi2by2 and epitools packages... or not!

#............................................
# Model prep: tabular statified estimates ####
#............................................

# (HW4.1.Q2) Three way prop table
pt2 = prop.table(table(births$pnc5_f, births$preterm_f, births$raceeth_f),
                 margin = c(1,3)) #risks. Note we're getting into "dplyr is better" territory...
pt2[2,1,]-pt2[1,1,] #Crude RDs
pt2[2,1,]/pt2[1,1,] # RRs
(pt2[2,1,]/pt2[1,1,])/(pt2[2,2,]/pt2[1,2,]) # ORs
pt2[1,2,] # term births, no PNC

births_risk_df = births_sm %>%
  group_by(raceeth_f, pnc5_f, preterm_f) %>% # careful with groups
  summarize(n=n()) %>%
  mutate(pct = n / sum(n)) %>%
  filter(!is.na(pnc5_f), preterm_f == "preterm")
births_risk_df

#............................................
# EMM graphs ####
#............................................

# (HW4.1.Q3)
# EMM ggplot Using the dplyr object
births_risk_df %>%
  ggplot(aes(pnc5_f, pct, color=raceeth_f, group=raceeth_f))+
  geom_line(size = 1)+geom_point(aes(size=n))+
  geom_hline(yintercept = 0.125, lty=2)+
  annotate(label="EMM note!", x=0.8, y=0.14, geom="text")+
  scale_y_continuous(limits=c(0,NA))+
  labs(title="Quick look at crude EMM of RD by race-ethnicity",
       subtitle="Note possible EMM (angle of lines)... and underlying disparities!",
       x="Prenatal Care", y="Prevalence of Preterm Birth",
       caption="n = 62000 or so, data from NC SCHS 2012.") +
  theme_bw()

# (HW4.1.Q3 alternative)
# EMM graph using the three-way table object
pt2_df = data.frame(pt2); names(pt2_df) = c("exposure", "outcome", "group", "estimate")
ggplot(data=pt2_df[pt2_df$outcome == "preterm",],
       aes(x=exposure, y=estimate, color=group, group=group))+
  geom_point()+geom_path()+geom_hline(yintercept=0.125)+
  annotate("text", x=.7, y=.13, label="EMM note line")+
  labs(title="Quick look at Crude EMM of RD by Race-Ethnicity", x= "Prevalence of Preterm Birth",
       subtitle="Note possible EMM in the angle of the lines... \n ...and disparities more impactful than our intervention. ")

#............................................
# Crude GLM ####
#............................................

# (HW4.2.Q4)
m_crude_rd = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="identity"))
m_crude_rd

# (HW4.Q2.5) factor referrent matters!
# births$preterm_f = relevel(births$preterm_f, ref = "preterm")
levels(births$preterm_f) # If levels are backwards you'll need something like the below line:
births$preterm_f = relevel(births$preterm_f, ref = "term")
levels(births$pnc5_f) # Looking good
m_crude_rd = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="identity"))
levels(births$raceeth_f)
births$raceeth_f = relevel(births$raceeth_f, ref = "White non-Hispanic")

# (HW4.2.Q6) Model object structure
str(m_crude_rd) #It's huge!
str(m_crude_rd, max.level = 1)
# It's a list with a buncha stuff in it!

# (HW4.2.Q7) coef() and confint()
coef(m_crude_rd)
confint(m_crude_rd)

# (HW4.2.Q8) Clean outputs for graphing
m_crude_results = data.frame(model="M1: Crude", cbind(broom::tidy(m_crude_rd), broom::confint_tidy(m_crude_rd)), stringsAsFactors = F)[2,]
str(m_crude_results)

# (HW4.2.Q9) Crude ggplot
m_crude_results %>%
  ggplot(aes(model, estimate))+
  geom_point() + geom_linerange(aes(ymin=conf.low, ymax=conf.high)) +
  scale_y_continuous(limits=c(NA,0)) # show zero as max

#............................................
# GLMs with Confounder Control ####
#............................................

# (HW4.2.Q8) Fuller models
head(births_sm)
births$mage_sq = births$mage ^ 2
m1 = glm(formula = preterm_f ~ pnc5_f, family = binomial("identity"), data = births)
m2 = glm(formula = preterm_f ~ pnc5_f + smoker_f , family = binomial("identity"), data = births)
m3 = glm(formula = preterm_f ~ pnc5_f + smoker_f + mage, family = binomial("identity"), data = births)
m4 = glm(formula = preterm_f ~ pnc5_f + smoker_f + poly(mage, 2, raw = T), family = binomial("identity"), data = births)

# method 0: no dplyr or purrr. yuck.
coef(m1); confint(m1)
c(coef(m1)[2], confint(m1)[2,])

m1_summary = summary(m1)
m1_summary$coefficients
c(coef(m1)[2], confint(m1)[2,], std.error = m1_summary$coefficients[2,2])

m1_df = c(coef(m1)[2], confint(m1)[2,], std.error = m1_summary$coefficients[2,2])
m2_df = c(coef(m2)[2], confint(m2)[2,], std.error = summary(m2)$coefficients[2,2])
m3_df = c(coef(m3)[2], confint(m3)[2,], std.error = summary(m3)$coefficients[2,2])
m4_df = c(coef(m4)[2], confint(m4)[2,], std.error = summary(m4)$coefficients[2,2])

results_df = data.frame(rbind(m1_df, m2_df, m3_df, m4_df))
results_df$model_name = row.names(results_df)

# method 1: dplyr & broom
library(broom)
m1_df = bind_cols(tidy(m1), confint_tidy(m1)) %>% filter(term == "pnc5_fEarly PNC") %>% mutate(model_name="M1: Crude")
m2_df = bind_cols(tidy(m2), confint_tidy(m2)) %>% filter(term == "pnc5_fEarly PNC") %>% mutate(model_name="M2: M1 + smoking")
m3_df = bind_cols(tidy(m3), confint_tidy(m3)) %>% filter(term == "pnc5_fEarly PNC") %>% mutate(model_name="M3: M2 + mage")
m4_df = bind_cols(tidy(m4), confint_tidy(m4)) %>% filter(term == "pnc5_fEarly PNC") %>% mutate(model_name="M4: M3 + mage^2")
#....
results_df = bind_rows(m1_df, m2_df, m3_df, m4_df)
results_df

# method 2: dplyr & purrr
model_results = function(m){bind_cols(tidy(m), confint_tidy(m)) %>% filter(term == "pnc5_fEarly PNC")}
model_results(m1)

model_df = tibble(models = list(m1, m2, m3, m4),
                  model_name = paste0("M", 1:4),
                  results = map(models, model_results),
                  estimate = map_dbl(results, "estimate"))
model_df
# ^ The estimate variable is just a demo of the list accessor

results_df = bind_rows(model_df$results) %>%
  mutate(model_name = model_df$model_name)
results_df

# method 3.... fancier purrr!
vars = c("preterm_f", "pnc5_f", "smoker_f", "mage", "mage_sq") # predictors
model_maker = function(v) { glm(preterm_f ~ ., family=binomial("identity"), data=births_sm[,v]) }
model_df = tibble(covariates = map(2:length(vars), ~ vars[1:.]), models = map(covariates, model_maker))
#....

# plotting
ggplot(results_df, aes(model_name, estimate, color=std.error, fill=std.error))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), size=1)+
  geom_point(shape=15, size=4, color="white")+ geom_point(shape=15)+
  scale_y_continuous(limits=c(NA,0))+
  labs(title="Model results", subtitle="Mirroring tufte boxplot aesthetics, see ggthemes::geom_tufteboxplot()")+
  ggthemes::theme_tufte()

#............................................
# Write model results out ####
#............................................

write.table(model_results, "clipboard", sep="\t", row.names = F)
# or write csv and shell.exec
write.csv(model_results, "births_table2.csv", row.names = F)
# shell.exec("births_table2.csv")

#............................................
# RR/OR models and LRT demo ####
#............................................

# (HW4.3.Q11) RR and OR models
m1_rr = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="log")) #RR model
exp(coef(m1_rr))
m1_or = glm(data=births, preterm_f ~ pnc5_f, family=binomial(link="logit")) #OR model
exp(coef(m1_or))


# Model comparison HW4.3.Q12
model_data = na.omit(births) # also see complete.cases() for T/F vector. Nice for model comparisons where nesting is required.
m_crude_rd = glm(data=model_data, preterm_f ~ pnc5_f, family=binomial(link="identity"))
m_plussmoke_rd = glm(data=model_data, preterm_f ~ pnc5_f + smoker_f, family=binomial(link="identity"))
anova(m_crude_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

m_plusmage_rd = glm(data=model_data, preterm_f ~ pnc5_f + smoker_f + mage, family=binomial(link="identity"))
anova(m_plusmage_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

m_plusmagesq_rd = glm(data=model_data, preterm_f ~ pnc5_f + smoker_f + mage + mage_sq, family=binomial(link="identity"))
anova(m_plusmagesq_rd, m_plussmoke_rd, test = "LRT") #Better to use bias-variance trade-off process. See EPID 718.

#............................................
# I'd like to add g-formula in the future... as well as bias-variance trade offs
#............................................

# ......................................
# __End Homework 4 /  Start Homework 5 ####
# ......................................

#............................................
# EMM / final model
#............................................
glm(data=births, preterm_f ~ pnc5_f + smoker_f, family=binomial(link="identity")) #Discussion of model spec

head(births)
table(births$raceeth_f, births$methnic) # note my "ethnicity" coding may not be ideal.
mfull_emm_rd = glm(data=births,
                   preterm_f ~ pnc5_f * raceeth_f +
                     smoker_f + mage + mage_sq,
                   family=binomial(link="identity"))

summary(mfull_emm_rd)
table(interaction(births$pnc5_f, births$raceeth_f)) # : = interaction()
broom::tidy(mfull_emm_rd)

# Point estimates by hand, a few ways
mfull_emm_rd$coefficients[2] #WnH
RD_WnH = mfull_emm_rd$coefficients["pnc5_fEarly PNC"] #WnH
RD_WnH + mfull_emm_rd$coefficients["pnc5_fEarly PNC"] #AA
sum(mfull_emm_rd$coefficients[c(2,10)]) # AA
sum(mfull_emm_rd$coefficients[c(2,11)]) # WH
sum(coef(mfull_emm_rd)[c(2,12)]) # AI/AN
sum(coef(mfull_emm_rd)[c(2,13)]) # Other


# C(births$raceeth_f) # get or set contrasts
contrasts(births$raceeth_f) # Let's look: contrast matrix
# See contr.treatment or contr.sum

#..........................................
# EMM contrasts ####
#..........................................

# Contrasts using contrast package
# install.packages("contrast")
library(contrast)
map(births[, map_lgl(births, is.factor)], levels) # level check - purrr

contrast(mfull_emm_rd, # demo one-level
         a=list(pnc5_f = "Early PNC",
                raceeth_f = "African American",
                smoker_f = "Non-smoker", mage=0, mage_sq=0),
         b=list(pnc5_f = "No Early PNC",
                raceeth_f = "African American",
                smoker_f="Non-smoker", mage=0, mage_sq=0))

# Here's the Contrast / EMM punchline!
raceeth_diff = contrast(mfull_emm_rd,
                        a=list(pnc5_f = "Early PNC",
                               raceeth_f = levels(births$raceeth_f),
                               smoker_f="Non-smoker", mage=0, mage_sq=0),
                        b=list(pnc5_f = "No Early PNC",
                               raceeth_f = levels(births$raceeth_f),
                               smoker_f="Non-smoker", mage=0, mage_sq=0))
print(raceeth_diff, X=T)
str(raceeth_diff)

EMM_df = data.frame(model=paste0("M5: ", raceeth_diff$raceeth_f),
                    estimate=raceeth_diff$Contrast,
                    std.error = raceeth_diff$SE,
                    conf.low=raceeth_diff$Lower,
                    conf.high=raceeth_diff$Upper,
                    stringsAsFactors = F)
EMM_df
results_df

as.data.frame(results_df)
full_model_results = bind_rows(results_df %>% rename(model=model_name),EMM_df)
ggplot(full_model_results, aes(model, estimate,color=estimate, fill=estimate))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high), size=1)+
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
write.csv(bind_rows(full_model_results,EMM_df),
          "table2_birth_EMM_df.csv", row.names = F)
# shell.exec("table2_birth_EMM_df.csv")
write.table(full_model_results,
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

county_results =
  county_data %>%
  left_join(births %>% group_by(cores) %>%
              summarise(n = n(), preterm=sum(preterm, na.rm=T), pnc5=sum(pnc5, na.rm=T)) %>%
              mutate(pct_pnc5 = pnc5/n*100, pct_preterm=preterm/n*100)
  )
county_results %>% write.table("clipboard", sep="\t", row.names = F)

save.image("birth_results.RData")
#............................................


#............................................
## Maps 1 - libraries & ecosystems ####
#............................................
# load("D:/User/Dropbox (Personal)/Education/Classes/18Fall_EPID799C_RforEpi/map_parts.rdata")
library(tidyverse)
library(broom)
library(sp)
library(rgdal) #for reading and writing shapefiles
library(rgeos) #for spatial tools like gCentroid
library(RColorBrewer) #For better, much better color ramps
library(tmap)
library(sf) #if you use the sf way, pretty much just need sf
library(ggthemes) # for theme_map
library(mapproj)

head(county_results) #formatter[formatter$variable=="cores",]

#.........................
# Get the county shapefile
#.........................
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

#.........................
# sp way
#.........................
nc_counties = readOGR(dsn="./maps", layer="nc counties", stringsAsFactors = F) # the sp way
class(nc_counties)
plot(nc_counties)
str(nc_counties, max.level = 2)
# ... or us tigris package. Also see acs package

#remember to merge with the spatial object, not just the data slot, to keep it from reordering!
nc_counties = merge(nc_counties, county_results, by.x="GEOID", by.y="FIPS")
head(nc_counties@data)
# Quick sp plots
spplot(nc_counties, "pct_pnc5", main="% Early PNC") # on spplot: https://edzer.github.io/sp/
spplot(nc_counties, "pct_preterm", main="% Preterm")
spplot(nc_counties, c("pct_pnc5", "pct_preterm"))
writeOGR(nc_counties, dsn="./maps", layer="nc counties w data", driver="ESRI Shapefile", overwrite_layer = T)

sum(nc_counties@data$n)
sum(nc_counties[nc_counties$NAME %in% c("Orange", "Durham", "Chatham"),]$n)

### A little prettier
nc_counties$pct_earlyPNC_f = cut(nc_counties$pct_pnc5, 7)
spplot(nc_counties, "pct_earlyPNC_f",
       col.regions=brewer.pal(7,"RdYlGn"),
       main="% of Births with Early Start of Prenatal Care (<5mo)")

### Using base plot()
display.brewer.all()
nc_counties$pct_preterm_colors = as.character(cut(nc_counties$pct_preterm, 10, labels = brewer.pal(10,"RdYlGn")))
plot(nc_counties, co=nc_counties$pct_preterm_colors, main="% Preterm birth")
county_centroids = gCentroid(nc_counties, byid = T) # aaand here's why R's great as a GIS.
text(county_centroids@coords, labels=as.character(nc_counties$NAME), cex=0.4)

### Using ggplot() OLD style - nice, but slow
names(nc_counties@data)
nc_counties_fort = fortify(nc_counties, region = "GEOID") # This is very old fashioned
str(nc_counties_fort)
nc_counties_fort = merge(nc_counties_fort, nc_counties, by.x="id", by.y="GEOID")
head(nc_counties_fort)
cut(nc_counties$pct_pnc5, 5)
pretty(nc_counties$pct_pnc5) #funny name
nc_counties_fort$pct_pnc5_f = cut(nc_counties_fort$pct_pnc5, breaks=pretty(nc_counties_fort$pct_pnc5))
table(nc_counties_fort$pct_pnc5_f)
#grep to make , a -_ move to top, like
#http://time_com/4394141/zika-abortion-usa/
mymap =
  ggplot(nc_counties_fort, aes(x=long, y=lat, group=group, fill=pct_pnc5_f)) +
  geom_polygon(color="white")+coord_map()+
  labs(title="Early prenatal care by county in North Carolina, 2012",
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
tm_shape(nc_counties)+ # skip the whole fortify thing.
  tm_fill("pct_pnc5", title="% Early PNC")+
  tm_borders()+ #could add more layers with tm_shape and then tm_whatever...
  tm_layout(main.title="tmap: % Early Prenatal Care by County")
# ^ Note ggplot like syntax.

# sf - the current best practice
library(sf)
nc_counties_sf = st_read("./maps/nc counties.shp", stringsAsFactors = F)
str(nc_counties_sf) # a dataframe
plot(nc_counties_sf)

nc_counties_sf %>% # dplyr the df right into ggplot!
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(county_results, by=c("GEOID"="FIPS")) %>%
  ggplot(aes(fill = pct_pnc5))+
  geom_sf()+ # default is that geometry "aesthetic" is in geometry column. Which it is!
  theme_minimal() + # alternatives here
  labs(title="sf and ggplot - perhaps best for now: % Early PNC ")
# make_EPSG() # or lookup

#................................

#................................
# Maps 2 - spatial analysis ####
#................................

# Spatial merging
nc_counties_sf %>% # dplyr the df right into ggplot!
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(county_results, by=c("GEOID"="FIPS")) %>%
  mutate(preterm_quintile = cut(pct_preterm, quantile(pct_preterm, seq(0,1,.2)))) %>%
  group_by(preterm_quintile) %>% # how cool is this.
  summarise(pct_pnc5 = mean(pct_pnc5, na.rm=T)) %>%
  ggplot(aes(fill = pct_pnc5))+
  geom_sf()
# ^ or spatial union, etc.

# Demos: transform, centroids, buffers, touches....
proj4string(nc_counties) #or nc_counties@proj4string
nc_stateplane_proj = "+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
nc_counties_stateplane = spTransform(nc_counties, nc_stateplane_proj)
# Examples: http://spatialreference.org/ref/epsg/wgs-84/

# centroids
county_cents = gCentroid(nc_counties_stateplane, byid=T) #just the spatial object
county_cents_sp = SpatialPointsDataFrame(county_cents, data=nc_counties@data) #need to add back the data

# buffers
plot(nc_counties_stateplane); plot(county_cents, add=T)
plot(county_cents_sp[county_cents_sp$NAME == "Orange",], col="blue", add=T, pch="0")
plot(gBuffer(county_cents, width = 10*5280, byid = T), add=T) #10 mi buffers

# spatial relationships - touching?
plot(nc_counties[county_cents_sp$NAME == "Orange",])
orange_neighbors = gTouches(nc_counties[county_cents_sp$NAME == "Orange",], nc_counties, byid = T)
str(orange_neighbors) # note it's a matrix
plot(nc_counties); plot(nc_counties[orange_neighbors[,1],], col="blue", add=T)

# A question of hospitals
# http://data.nconemap.gov/geoportal/rest/find/document?searchText=Hospitals&f=searchpage&f=searchpage
med_facilities = read_sf("./maps/medfacs.shp", stringsAsFactors = F) # the sf way
plot(nc_counties); plot(med_facilities %>% st_geometry(), add=T)
nc_counties_sf = nc_counties %>% st_as_sf()
st_crs(nc_counties_sf); st_crs(med_facilities) # oops! projection mismatch!

med_facilities = med_facilities %>% st_transform(crs = nc_stateplane_proj)
nc_counties_sf = nc_counties_sf %>% st_transform(crs = nc_stateplane_proj)
plot(nc_counties_sf %>% st_geometry()); plot(med_facilities %>% st_geometry(), add=T, col="blue", pch=".")

# spatial subsetting. this is cooool.
plot(med_facilities %>% st_geometry())
orange_county = nc_counties_sf[nc_counties_sf$NAME=="Orange",]
plot(orange_county %>% st_geometry())
plot(med_facilities[orange_county,], max.plot=1, add=T)

#what centroids are 50m from a hospital?
head(med_facilities)
table(med_facilities$STYPE)
hospitals = med_facilities[med_facilities$STYPE == "Hospital",] # nice!

# don't have birth locations - let's use centroids as a proxy
dist_matrix = st_distance(county_centroids %>% spTransform(nc_stateplane_proj) %>% st_as_sf, hospitals)
dim(dist_matrix)
dist_matrix[1,]
within_5mi = function(x){any(x<=5*5280)}
apply(dist_matrix, 1, within_5mi) # can colbind this back together.

dist_list = st_is_within_distance(county_centroids %>%
                                    spTransform(nc_stateplane_proj) %>%
                                    st_as_sf(),
                                  hospitals, 5*5280)
map_lgl(dist_list, ~ length(.x)>0)

# spatial over... so many ways
hold = st_within(hospitals, nc_counties_sf)
str(hold) # what do we know that can work with lists?
unlist(hold) # here's one way!
map_int(hold, ~.x[[1]]) # here's another flexible way.

# What about spatial autocorrelation? (the sp way)
library(spdep)
head(nc_counties@data)
# Neighbors
nb = poly2nb(nc_counties, row.names=nc_counties$NAME)
summary(nb)
str(nb)
plot(nc_counties); plot(nb, coordinates(nc_counties), col="red", add=T)

#
weight_list = nb2listw(nb, style="B") # Binary coding
moran(nc_counties$pct_preterm, weight_list, n=length(weight_list$neighbours),
      S0 = Szero(weight_list)) # Not much clustering, close to 0
moran(nc_counties$pct_pnc5, weight_list, n=length(weight_list$neighbours),
      S0 = Szero(weight_list)) # Not much clustering, close to 0

# Quick graph
tibble(orig = nc_counties$pct_pnc5,
       neighbors = lag.listw(nb2listw(nb, style="W"), nc_counties$pct_pnc5)) %>%
  ggplot(aes(orig, neighbors))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_continuous(limits=c(75,100))+
  scale_y_continuous(limits=c(75,100))

# But maybe a statistically significant (do we care?) amount?
# Monte Carlo
moran.mc(nc_counties$pct_preterm, weight_list, nsim=99)
moran.mc(nc_counties$pct_pnc5, weight_list, nsim=99)
# ^ particularly useful: check to see if model residuals are clustered spatially.
# May be a sign you're missing something.


# Save out
save(list = c("nc_counties", "nc_counties_sf", "county_results", "med_facilities", "nc_stateplane_proj"), file = "map_parts.rdata")
#.................................................
# Notes
# http://www.kevjohnson.org/making-maps-in-r/
# https://www.google.com/search?q=north+carolina+counties+table&tbm=isch&tbo=u&source=univ&sa=X&ved=0ahUKEwiLnYyDxNbNAhVQzGMKHUAUD4kQsAQIHQ&biw=1536&bih=716
#.................................................


#............................................
## MISC: Clusters & Trees ####
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
# MISC: PARKING LOT (useful code without questions) ####
#............................................

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


#............................................
# MISC: Bias variance trade off... ####
#............................................

# ......................................
# MISC: SAS Assignments # This section is YUCK!!!! ####
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

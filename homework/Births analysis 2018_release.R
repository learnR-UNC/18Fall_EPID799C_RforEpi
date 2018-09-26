# __Start Homework 1 ####
#............................
# Births 2012 Analysis for EPID 799C: PNC decrease PT birth? 
# Mike Dolan Fliss, August 2017
# Aim: Does early prenatal care (<month 5) decrease the risk of preterm (<week 37) birth?
# (This Descriptive header is HW1.Q1a)
#............................ (<- example of HW1.Q1b, comment bar)
# Project Notes:
# Birth Certificate Revisions: https://www.cdc.gov/nchs/nvss/vital_certificate_revisions.htm (HW1.Q1d, clickable link)
#............................

#............................
# Libraries, directories, load data (HW1.Q1f) 
#............................
# NOTE: May need to install these packages in advance on your local machine.
# install.packages("tableone")
library(tidyverse) # for ggplot, dplyr in HW1-4 (<- example of a post-line code comment, HW1.Q1c)
library(lubridate) # for dates in HW2
library(tableone) # used in HW2
# Set directories. 
data_dir = paste0(getwd(), "/data")
output_dir = paste0(getwd(), "/data")
# map_dir = paste0(data_dir, "/GIS") # used later
# Note you'll need to set different ones with setwd()! Since we're building this as a github repo the wd defaults to script dir.
# More on git here: http://r-bio.github.io/intro-git-rstudio/ . There's also a datacamp on it.
#............................

#............................
# Read data (HW1.Q2) 
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
# Exploring Data - (HW1Q3 & HW1Q4) 
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
# Recode Variables: Exposure, Outcome & Covariates (HW1.Q5) 
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
births$preterm_f <- cut(births$wksgest, breaks = c(0,19,36,99), labels =  c("too young", "preterm", "term")) #should be no births too young at this point

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
par(mfrow=c(1,2))
boxplot(births$mage, main="Boxplot of Maternal Age Distribution")
plot(density(births$mage), main="Density Plot of Maternal Age Distribution")

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
# Optional data exploration (HW1.Q6) 
# ......................................

# (HW1.Q6) 
## A
CreateTableOne(vars = c("preterm_f"), 
               strata = c("mage", "pnc5_f", "smoker_f", "sex_f"),
               data = births)

## B
mice::md.pattern(births)

## C
ggplot2:ggpairs(data = births[1:1000, c("pnc5_f", "preterm_f")],
        title="Exposure versue Outcome for First 1,000 Obserations")
# ......................................

# ......................................
# __End Homework 1 /  Start Homework 2 ####
# ......................................


# ......................................
# Eligibility criteria & core recoding  (HW2 part 1)
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


old_births <- births #(HW2.1.Q4a)

# Below is the example grepl (HW2.1.Q4b)
grepl("a", c("banana", "peach", "ornge")) 

eligibility_drops = nrow(births) - apply(births[,grepl("incl_", names(births))], FUN=sum, MARGIN = 2, na.rm=T) #(HW2.1.Q4c optional)
eligibility_drops #(HW2.1.Q4c optional)

births$include_allpass = apply(births[, grepl("incl_", names(births))], FUN=all, MARGIN = 1) #(HW2.1.Q4d indicator for the inclusion criteria yes/no)

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
vars_of_interest = c("pnc5_f", "preterm_f", "smoker_f", "wksgest", "sex", "meduc", "raceeth_f", "weeknum", "include_allpass")
tableone::CreateTableOne(data=births[, vars_of_interest]) #(HW2.1.Q5)
tableone::CreateTableOne(data=old_births[, vars_of_interest], strata="include_allpass")
# ......................................
# NOTES: Finishes with 62,370 of 122,513 
# ......................................

# ......................................
# __End Homework 2 /  Start Homework 3 ####
# ......................................

# ......................................
# Explore data (HW3 part 1)
# ......................................
#(HW3.1.Q1A) Graph weeks
npop = formatC(sum(!is.na(births$wksgest)), big.mark=',') #(HW3.1.Q1a)
ggplot(data=births, aes(x=wksgest, y=..prop..))+geom_bar() + #Does what you need, as does geom_histogram()
  labs(title="Figure 1: Proportional distribution of gestational age at birth", 
       subtitle=paste0("From ", npop, " births in NC in 2012 from the NC SCHS, posted at Odum Institute"), 
       x="Weeks of gestation", y="Proportion of population") + 
  theme_bw()



  #(HW3.1.Q1a): Working with weeknum. - should double check this. Just used %V above.
  hist(births$weeknum, breaks = min(births$weeknum):max(births$weeknum)) # Looks weird

#(HW3.1.Q1B) : Graph weeknum 
ggplot(births, aes(x=weeknum, y=..prop..)) + geom_bar() +
  labs(title="Throwaway: Investigating Weeknum", 
       subtitle="Note to self : Weeknum needs some work, doesn't quite match SAS", 
       x="Week number of birth", y="Proportion of population") +
  theme_bw()

#(HW3.1.Q1C) weeks vs. weeknum 
ggplot(births, aes(x=wksgest, y=weeknum)) + 
  geom_jitter(width=1, height=1, pch=".", alpha=0.3)+
  labs(title="Throwaway: week vs. weeknum", 
       subtitle="Note to self: Does this match what we expect given the inclusion criteria?", 
       x="weeks of gestation", y="week number of birth in year")+
  theme_bw()


#(HW3.1.Q1D)
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





#(HW3.1.Q1E)
require("ggridges")
ggplot() + 
  geom_density_ridges_gradient(data=births, aes(x=mage, y=factor(cut(wksgest, 10)), fill=..x..)) + 
  scale_fill_gradientn(colors=c("#fc8d59", "#ffffbf", "#91cf60")) + 
  facet_grid(meduc~raceeth_f) +
  ggtitle("Distribution of Weeks Gestation by Birth Outcome") + 
  labs(subtitle="This is the continuous variable we dichotomized") + 
  ylab("Weeks of Gestations factorized") + xlab("mage") + 
  theme_bw()




# ......................................
# Functional Form of maternal age w/ dplyr and ggplot2 #(HW3 part 2)
# ......................................

mage_df = births %>% group_by(mage) %>% #(HW3.2.2) setup 
  summarize(n=n(), 
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T))
head(mage_df) #(HW3.2.2) answer 

#(HW3.2.3A)  
ggplot(mage_df, aes(mage, pct_preterm))+ 
  geom_point(aes(size=n))+
  geom_smooth(aes(weight=n), color="blue", method="loess")+
  labs(title="% Preterm vs. Maternal Age", 
       x="maternal age", y="% preterm", 
       subtitle="Investigating function form of maternal age", 
       caption="Note for future glms: Seems quadratic. Blue is loess, red is square linear.")

#(HW3.2.3B)  Optional Challenge  
ggplot(mage_df, aes(mage, pct_preterm)) + 
  geom_point(aes(size=n)) +
  geom_smooth(aes(weight=n), color="blue") +
  geom_smooth(aes(weight=n), method="lm", formula=y ~ poly(x, 2), color="red", lty=2) + # this is the square error term
  labs(title="% Preterm vs. Maternal Age", 
       x="maternal age", y="% preterm", 
       subtitle="Investigating function form of maternal age", 
       caption="Note for future glms: Seems quadratic. Blue is loess, red is square linear.")




# ......................................


# ......................................
# County specific stories w/ dplyr and ggplot2 #(HW3 part 3)
# ......................................
#(HW3.3.4A) 
format_helper = read.csv("data/birth-format-helper-2012.csv", stringsAsFactors = F)
#(HW3.3.4A.i) 
county_data = format_helper[format_helper$variable == "cores",] #base R way
#(HW3.3.4A.ii) 
names(county_data) = c("variable", "cores", "county_name", "FIPS")
#(HW3.3.4A.iii) 
county_data$var = NULL #drop in base R. There are other ways...
#(HW3.3.4A.iv) 
county_data$cores = as.numeric(county_data$cores)
str(county_data$cores)

#(HW3.3.4B) same results as above but now using "tidy" methods and more human readable 
county_data = format_helper %>% 
  filter(variable == "cores") %>%
  rename(cores=code, county_name=recode, FIPS=helper) %>% 
  select(-variable) %>%
  mutate(cores = as.numeric(cores))

#(HW3.3.4C)
# Load tier data 
# https://www.nccommerce.com/research-publications/incentive-reports/county-tier-designations 
county_tiers = read.csv("data/county_tiers.csv", stringsAsFactors = F) # shell.exec("county_tiers.csv")
#(HW3.3.4C.i)
str(county_tiers$econ_tier)
#(HW3.3.4C.ii)
county_tiers$econ_tier = ordered(county_tiers$econ_tier)

#(HW3.3.4D)
county_df = births %>% group_by(cores) %>%
  summarize(n=n(), 
            pct_earlyPNC = mean(pnc5, na.rm=T),
            pct_preterm = mean(preterm, na.rm=T)) %>%
  left_join(county_data) %>% 
  left_join(county_tiers)
head(county_df) #(HW3.2.3D) output


#(HW3.3.4E) write this out to your local directory
write.csv(x=county_df, "data/county_birth_summary.csv", row.names = F, quote = F)


#(HW3.3.5)
# Create a plot with ggplot
plotObj <- ggplot(county_df, aes(x=pct_earlyPNC, y=pct_preterm, color=econ_tier))+
  geom_point(aes(size=n))+geom_text(aes(label=county_name), nudge_y=.01, alpha=0.5)+
  geom_smooth(se=F)
plot(plotObj)
#(HW3.3.5a)
plotly::ggplotly(plotObj)


#(HW3.3.6A)
county_name_ord = factor(county_df$county_name, county_df$county_name[order(county_df$pct_earlyPNC)], ordered=T)

#(HW3.3.6B)
county_df <- county_df %>% 
  mutate(county_name_ord = county_name_ord) %>%
  gather(key=variable, value=value, n, pct_earlyPNC, pct_preterm) %>% 
  head()
#(HW3.3.6C)
county_df %>% 
ggplot(aes(county_name_ord, value, fill=econ_tier)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~variable, scales = "free_x")


## end of homework 3

# ......................................
# __End Homework 3 /  Start Homework 4 ####
# ......................................







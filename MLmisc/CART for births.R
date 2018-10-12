# R KNIME
# https://stackoverflow.com/questions/50759098/how-to-import-r-code-in-knime
# https://www.knime.com/nodeguide/scripting/r/example-of-r-snippet
# https://www.statmethods.net/advstats/cart.html
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
#.............................
library(tidyverse)
library(broom)
library(forcats)
library(rpart)
library(rpart.plot)
#.............................

#.............................
# Data 
#.............................
births_sm <- readRDS("~/Documents/GitHub/18Fall_EPID799C_RforEpi/data/births_afterhw2.RDS")
births_sm = as.tibble(births_sm)
# Quick factor checks
levels(births_sm$preterm_f)
births_sm$preterm_f <- factor(births_sm$preterm_f)
births_sm$preterm_f = fct_rev(births_sm$preterm_f)
levels(births_sm$pnc5_f)

levels(births_sm$raceeth_f)
births_sm$raceeth_f = fct_rev(births_sm$raceeth_f)
births_sm$raceeth_f = relevel(births_sm$raceeth_f, 
                              ref = "White non-Hispanic")
births_sm$mage_sq = births_sm$mage ^ 2

keep_vars = c("pnc5_f", "preterm_f", "smoker_f", "sex", "mage", "mage_sq", "raceeth_f") #...what about cores?
df = births_sm[complete.cases(births_sm),] %>% select(keep_vars)
#......................

#......................
# Rpart model - CART - classification and regression trees
#......................
# ^ trees can't handle interaction!! Can handle missingness natively. 
names(df)
#rpart_mod = rpart(preterm_f ~ pnc5_f + smoker_f + mage + mage_sq + pnc5_f + raceeth_f, data=births_sm, method="class") 
my_rpart_control = rpart.control(minsplit=20, cp=0) # cp=0 asks for a super-regressed tree. Still dumb!
tree_mod = rpart(preterm_f ~ pnc5_f, data=df, method="class",control=my_rpart_control) # crude... just a node?
tree_mod = rpart(preterm_f ~ ., data=df, method = "class", control=my_rpart_control, model = T) 
# ^ these trees are very stupid. Very stooopid and underperforming. 

# Summary
summary(tree_mod)

# Plots
plot(tree_mod,uniform=TRUE); text(tree_mod, use.n=T, all=T, cex=0.8, pretty=0)
plot(tree_mod); text(tree_mod)
prp(tree_mod)
rpart.plot::rpart.plot(tree_mod, branch=0.3)
  
# Cross validation error rates
printcp(tree_mod) 
plotcp(tree_mod)

# Confusion matrix
births_sm$tree_class = predict(tree_mod, type="class")
table(true = births_sm$preterm_f, tree = births_sm$tree_class)

# try random forest later...


#...................
# Give up, try GLM
#...................
glm_mod = glm(data = df, preterm_f ~ pnc5_f + mage + mage_sq + pnc5_f*raceeth_f + smoker_f, family=binomial("logit"))
tidy(glm_mod); glance(glm_mod)
df$glm_risk = predict(glm_mod, type = "response") #predict.glm

# Explore risk prediction density
plot(density(df$glm_risk))
summary(df$glm_risk)
cut_point = 0.2 
# ^ arbitrary! Does the range here mean the GLM is never more than 37% sure 
# (e.g. stratified risk) that a covariate set implies preterm birth?!
df$glm_class = ifelse(predict(glm_mod, type = "response") > cut_point, "preterm", "term")
table(true = df$preterm_f, glm = df$glm_class) # confusion matrix
mean(df$preterm_f == df$glm_class) # :( when in doubt, guess term! duh...
# let's look at unique "cases"
unique_df = df[!duplicated(df),]; dim(unique_df)
head(unique_df[order(unique_df$glm_risk, decreasing = T),]) # high risk pregnancies. 30% of births are PT
# could also do the below dplyr, which took some googling...
df %>% group_by_at(vars(names(df))) %>% summarise(n=n()) %>% arrange(desc(glm_risk))

library(ROCR)
pr = prediction(p, df$preterm_f)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf) # ROC is garbage?

auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc # worse than guessing?

prf = performance(pr, measure = "sens", x.measure = "spec")
plot(prf) # ROC is garbage?

library(haven)
library(tidyverse)
library(flexplot)
PSYR6003_A2 <- read_sav("PSYR6003.A2.sav")#load the data.
View(PSYR6003_A2)

#Data Cleaning

PSYR6003_A2_noNA <- na.omit(PSYR6003_A2)#remove cases with missing data
view(PSYR6003_A2_noNA)

A2_Clean <- PSYR6003_A2_noNA %>%
  filter(sex != "Other (specify)") #remove case with "other"
view(A2_Clean)

A2_Clean_MF <- A2_Clean %>% mutate(sex = ifelse(sex == "Male", "1", "0")) %>% 
  mutate(sex = as.factor(sex))#change sex variable to be factor 
view(A2_Clean_MF)

#Reverse coding tipm.CONS2.3y
reverse_cols = c("tipm.CONS2.3y")#create vector with our variable of interest
A2_Clean_MF[, reverse_cols] = 8 - A2_Clean_MF[, reverse_cols]#subtract 8 from each value in the column. 
view(A2_Clean_MF_ST)#check to see if I messed up the data

#Creating a new variable representing the mean scores for each scale. 
A2 <- A2_Clean_MF %>%
  mutate(SPP_mean = rowMeans(select(., mps.SPP1.3y, mps.SPP2.3y, mps.SPP3.3y, mps.SPP4.3y, mps.SPP5.3y))) %>%
  mutate(NAf_mean = rowMeans(select(., guilt1.3y, guilt2.3y, guilt3.3y, dep1.3y, dep2.3y, dep3.3y, fear1.3y, fear2.3y, fear3.3y, host1.3y, host2.3y, host3.3y))) %>%
  mutate(CON_mean = rowMeans(select(., tipm.CONS1.3y, tipm.CONS2.3y)))
view(A2)

#######ANALYSES#######
#visualize univariate distributions 
library(patchwork)
a = flexplot(sex~1, data = A2)
b = flexplot(CON_mean~1, data = A2)
c = flexplot(SPP_mean~1, data = A2)
d = flexplot(NAf_mean~1, data = A2)
a+b+c+d

#visualize model
flexplot(NAf_mean~SPP_mean | CON_mean + sex, data = A2, method = "lm")

#generate AVP plot 
added.plot(NAf_mean~SPP_mean, data = A2, lm_formula = NAf_mean~CON_mean + sex)

#run the model 
model <- lm(NAf_mean~SPP_mean + CON_mean + sex, data = A2)

#check diagnostic plots to see if assumptions are met 
visualize(model, plot = "residuals")

#model comparison to see if SPP, CON, and sex are predictive of NAf
full_Hyp1 <- lm(NAf_mean~CON_mean + sex + SPP_mean, data = A2)
reduced_Hyp1 <- lm (NAf_mean~1, data = A2)
model.comparison(full_Hyp1, reduced_Hyp1)
estimates(full_Hyp1)

#model comparison to see if SPP predicts NAf above and beyond sex and CON_mean
full_Hyp2 <- lm(NAf_mean~CON_mean + sex + SPP_mean, data = A2)
reduced_Hyp2<- lm(NAf_mean~CON_mean + sex, data = A2)
model.comparison(full_Hyp2, reduced_Hyp2)
estimates(full_Hyp2)

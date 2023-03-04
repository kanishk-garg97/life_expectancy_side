install.packages("ggplot2")
install.packages("tidyverse")
install.packages("mice")
install.packages("dbplyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("grid")
install.packages("gridExtra")
install.packages("car")
install.packages("data.table")
install.packages("pysch")

library(magrittr)
library(ggplot2)
library(dbplyr)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(car)
library(data.table)


myData<-read.csv('Life_Expectancy_Data1.csv',header=T)
attach(myData)

#task 1.part a

summary(myData)

df1 <- data.frame(myData)

missing.values <- myData %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('grey', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

row.plot <- df1 %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Predictor",
       y = "Row Number", title = "Missing values in rows") +
  coord_flip()

row.plot

grid.arrange(percentage.plot, row.plot, ncol = 2)

#task 1 part b

df2 <- data.frame(df1)

#finding the p value to check if the data is normally distributed or not. 
shapiro.test(summary(df2$SP.DYN.LE00.IN))
shapiro.test(summary(df2$EG.ELC.ACCS.ZS))
shapiro.test(summary(df2$NY.ADJ.NNTY.KD.ZG))
shapiro.test(summary(df2$NY.ADJ.NNTY.PC.KD.ZG))
shapiro.test(summary(df2$SH.HIV.INCD.14))
shapiro.test(summary(df2$SE.PRM.UNER))
shapiro.test(summary(df2$SE.PRM.CUAT.ZS))
shapiro.test(summary(df2$SE.TER.CUAT.BA.ZS))
shapiro.test(summary(df2$SP.DYN.IMRT.IN))
shapiro.test(summary(df2$SE.PRM.CMPT.ZS))
shapiro.test(summary(df2$SE.ADT.LITR.ZS))
shapiro.test(summary(df2$FR.INR.RINR))
shapiro.test(summary(df2$SP.POP.GROW))
shapiro.test(summary(df2$EN.POP.DNST))
shapiro.test(summary(df2$SP.POP.TOTL))
shapiro.test(summary(df2$SH.XPD.CHEX.PC.CD))
shapiro.test(summary(df2$SH.XPD.CHEX.GD.ZS))
shapiro.test(summary(df2$SL.UEM.TOTL.NE.ZS))
shapiro.test(summary(df2$NY.GDP.MKTP.KD.ZG))
shapiro.test(summary(df2$NY.GDP.PCAP.CD))
shapiro.test(summary(df2$SP.DYN.CBRT.IN))
shapiro.test(summary(df2$SH.HIV.INCD))
shapiro.test(summary(df2$SH.H2O.SMDW.ZS))
shapiro.test(summary(df2$SI.POV.LMIC))
shapiro.test(summary(df2$SE.COM.DURS))

# find median of all all the columns grouped by continent so that the median
#value is unbiased.
#from below function u will get the values of median grouped by continents
#for each predictor values
df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(EG.ELC.ACCS.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(NY.ADJ.NNTY.KD.ZG), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(NY.ADJ.NNTY.PC.KD.ZG), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SH.HIV.INCD.14), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SE.PRM.UNER), list(name = median), na.rm = TRUE)



df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SE.PRM.CUAT.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SE.TER.CUAT.BA.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SP.DYN.IMRT.IN), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SE.PRM.CMPT.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SE.ADT.LITR.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(FR.INR.RINR), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SP.POP.GROW), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(EN.POP.DNST), list(name = median), na.rm = TRUE)



df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SP.POP.TOTL), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SH.XPD.CHEX.PC.CD), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SH.XPD.CHEX.GD.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SL.UEM.TOTL.NE.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(NY.GDP.MKTP.KD.ZG), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(NY.GDP.PCAP.CD), list(name = median), na.rm = TRUE)



df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SP.DYN.CBRT.IN), list(name = median), na.rm = TRUE)

df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SH.HIV.INCD), list(name = median), na.rm = TRUE)



df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SH.H2O.SMDW.ZS), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SI.POV.LMIC), list(name = median), na.rm = TRUE)


df1 %>%
  group_by(Continent) %>%
  summarise_at(vars(SI.POV.LMIC), list(name = median), na.rm = TRUE)

# replacing the missing values with median value grouped by continent

setDT(df1)
df1[, SP.DYN.LE00.IN := ifelse(is.na(SP.DYN.LE00.IN), 
                            median(SP.DYN.LE00.IN, na.rm = TRUE), 
                            SP.DYN.LE00.IN), by = Continent]

df1[, EG.ELC.ACCS.ZS := ifelse(is.na(EG.ELC.ACCS.ZS), 
                               median(EG.ELC.ACCS.ZS, na.rm = TRUE), 
                               EG.ELC.ACCS.ZS), by = Continent]

df1[, NY.ADJ.NNTY.KD.ZG := ifelse(is.na(NY.ADJ.NNTY.KD.ZG), 
                               median(NY.ADJ.NNTY.KD.ZG, na.rm = TRUE), 
                               NY.ADJ.NNTY.KD.ZG), by = Continent]

df1[, NY.ADJ.NNTY.PC.KD.ZG := ifelse(is.na(NY.ADJ.NNTY.PC.KD.ZG), 
                               median(NY.ADJ.NNTY.PC.KD.ZG, na.rm = TRUE), 
                               NY.ADJ.NNTY.PC.KD.ZG), by = Continent]

df1[, SH.HIV.INCD.14 := ifelse(is.na(SH.HIV.INCD.14), 
                               median(SH.HIV.INCD.14, na.rm = TRUE), 
                               SH.HIV.INCD.14), by = Continent]

df1[, SE.PRM.UNER := ifelse(is.na(SE.PRM.UNER), 
                               median(SE.PRM.UNER, na.rm = TRUE), 
                            SE.PRM.UNER), by = Continent]

df1[, SE.PRM.CUAT.ZS := ifelse(is.na(SE.PRM.CUAT.ZS), 
                               median(SE.PRM.CUAT.ZS, na.rm = TRUE), 
                               SE.PRM.CUAT.ZS), by = Continent]

df1[, SE.TER.CUAT.BA.ZS := ifelse(is.na(SE.TER.CUAT.BA.ZS), 
                               median(SE.TER.CUAT.BA.ZS, na.rm = TRUE), 
                               SE.TER.CUAT.BA.ZS), by = Continent]

df1[, SP.DYN.IMRT.IN := ifelse(is.na(SP.DYN.IMRT.IN), 
                               median(SP.DYN.IMRT.IN, na.rm = TRUE), 
                               SP.DYN.IMRT.IN), by = Continent]

df1[, SE.PRM.CMPT.ZS := ifelse(is.na(SE.PRM.CMPT.ZS), 
                               median(SE.PRM.CMPT.ZS, na.rm = TRUE), 
                               SE.PRM.CMPT.ZS), by = Continent]

df1[, SP.DYN.LE00.IN := ifelse(is.na(SP.DYN.LE00.IN), 
                               median(SP.DYN.LE00.IN, na.rm = TRUE), 
                               SP.DYN.LE00.IN), by = Continent]

df1[, SE.ADT.LITR.ZS := ifelse(is.na(SE.ADT.LITR.ZS), 
                               median(SE.ADT.LITR.ZS, na.rm = TRUE), 
                               SE.ADT.LITR.ZS), by = Continent]

df1[, FR.INR.RINR := ifelse(is.na(FR.INR.RINR), 
                               median(FR.INR.RINR, na.rm = TRUE), 
                               FR.INR.RINR), by = Continent]

df1[, SP.POP.GROW := ifelse(is.na(SP.POP.GROW), 
                               median(SP.POP.GROW, na.rm = TRUE), 
                               SP.POP.GROW), by = Continent]

df1[, EN.POP.DNST := ifelse(is.na(EN.POP.DNST), 
                               median(EN.POP.DNST, na.rm = TRUE), 
                               EN.POP.DNST), by = Continent]

df1[, SP.POP.TOTL := ifelse(is.na(SP.POP.TOTL), 
                               median(SP.POP.TOTL, na.rm = TRUE), 
                               SP.POP.TOTL), by = Continent]

df1[, SH.XPD.CHEX.PC.CD := ifelse(is.na(SH.XPD.CHEX.PC.CD), 
                               median(SH.XPD.CHEX.PC.CD, na.rm = TRUE), 
                               SH.XPD.CHEX.PC.CD), by = Continent]

df1[, SH.XPD.CHEX.GD.ZS := ifelse(is.na(SH.XPD.CHEX.GD.ZS), 
                               median(SH.XPD.CHEX.GD.ZS, na.rm = TRUE), 
                               SH.XPD.CHEX.GD.ZS), by = Continent]

df1[, SL.UEM.TOTL.NE.ZS := ifelse(is.na(SL.UEM.TOTL.NE.ZS), 
                               median(SL.UEM.TOTL.NE.ZS, na.rm = TRUE), 
                               SL.UEM.TOTL.NE.ZS), by = Continent]

df1[, NY.GDP.MKTP.KD.ZG := ifelse(is.na(NY.GDP.MKTP.KD.ZG), 
                               median(NY.GDP.MKTP.KD.ZG, na.rm = TRUE), 
                               NY.GDP.MKTP.KD.ZG), by = Continent]

df1[, NY.GDP.PCAP.CD := ifelse(is.na(NY.GDP.PCAP.CD), 
                               median(NY.GDP.PCAP.CD, na.rm = TRUE), 
                               NY.GDP.PCAP.CD), by = Continent]

df1[, SP.DYN.CBRT.IN := ifelse(is.na(SP.DYN.CBRT.IN), 
                               median(SP.DYN.CBRT.IN, na.rm = TRUE), 
                               SP.DYN.CBRT.IN), by = Continent]

df1[, SH.HIV.INCD := ifelse(is.na(SH.HIV.INCD), 
                               median(SH.HIV.INCD, na.rm = TRUE), 
                               SH.HIV.INCD), by = Continent]

df1[, SH.H2O.SMDW.ZS := ifelse(is.na(SH.H2O.SMDW.ZS), 
                               median(SH.H2O.SMDW.ZS, na.rm = TRUE), 
                               SH.H2O.SMDW.ZS), by = Continent]

df1[, SI.POV.LMIC := ifelse(is.na(SI.POV.LMIC), 
                               median(SI.POV.LMIC, na.rm = TRUE), 
                               SI.POV.LMIC), by = Continent]

df1[, SE.COM.DURS := ifelse(is.na(SE.COM.DURS), 
                               median(SE.COM.DURS, na.rm = TRUE), 
                               SE.COM.DURS), by = Continent]


#task 2
#removing all the columns with missing values even after imputation
df1 <- myData %>% select(-c(EG.FEC.RNEW.ZS,SI.POV.LMIC,
                            SE.PRM.CUAT.ZS,SE.ADT.LITR.ZS, SH.HIV.INCD.14))
df1 <- df1[,!names(df1) %in% c("Country.Name", "Continent","Country.Code")]

library(psych)
library(corrplot)
#converting the data frame to corelation matrix for visualising the corplot.
cor_data <- cor(df1)
corrplot.mixed(cor_data,
               lower = "ellipse",
               upper = "color",
               tl.col = "black",addCoef.col = 1,    
               number.cex = 0.5,tl.cex = 0.35)


df2 <- df1[,names(df1) %in% c("SH.H2O.SMDW.ZS","NY.GDP.PCAP.CD","SE.ADT.LITR.ZS", "SE.PRM.CMPT.ZS", "EG.ELC.ACCS.ZS",
           "SH.XPD.CHEX.PC.CD", "SE.TER.CUAT.BA.ZS", "SP.DYN.CBRT.IN", "SP.DYN.IMRT.IN")]
corPlot(df2, cex=0.5)

#maximum collinear predictor with life expectancy are listed below
#NY.GDP.PCAP.CD
#SE.ADT.LITR.ZS
#SE.PRM.CMPT.ZS
#SE.TER.CUAT.BA.ZS
#EG.ELC.ACCS.ZS
#SH.H2O.SMDW.ZS
#SP.DYN.IMRT.IN


df3 <- df1[,names(df1) %in% c("NY.GDP.PCAP.CD","SE.ADT.LITR.ZS", "SE.PRM.CMPT.ZS", 
                              "EG.ELC.ACCS.ZS", "SH.H2O.SMDW.ZS" ,"SE.TER.CUAT.BA.ZS",
                              "SP.DYN.LE00.IN","SP.DYN.IMRT.IN")]

setDT(df3)
df3 <- cor(df3)

#checking for colinearity again the selected predictor variables to find the minimum 
#colinearity among themselves
corrplot.mixed(df3,
               lower = "ellipse",
               upper = "color",
               tl.col = "black",addCoef.col = 1,    
               number.cex = 0.5,tl.cex = 0.35)

model <- lm(SP.DYN.LE00.IN ~ NY.GDP.PCAP.CD + SE.ADT.LITR.ZS + SE.PRM.CMPT.ZS 
            + EG.ELC.ACCS.ZS + SE.TER.CUAT.BA.ZS + SH.H2O.SMDW.ZS + SP.DYN.IMRT.IN , data = df3)
vif(model)

## final predictors for preparing the model
#NY.GDP.PCAP.CD
#SE.ADT.LITR.ZS
#SE.PRM.CMPT.ZS
#SE.TER.CUAT.BA.ZS
#EG.ELC.ACCS.ZS
#SH.H2O.SMDW.ZS
#SP.DYN.IMRT.IN
#SP.DYN.LE00.IN


#task 3 

par(mfrow = c(3,3))

model1 <- lm(SP.DYN.LE00.IN ~ NY.GDP.PCAP.CD, data = df3)
plot(NY.GDP.PCAP.CD, SP.DYN.LE00.IN, col="red")
abline(model1, lwd = 3, col="blue")
summary(model1)


model2 <- lm(SP.DYN.LE00.IN ~ SE.ADT.LITR.ZS, data = df3)
plot(SE.ADT.LITR.ZS, SP.DYN.LE00.IN, col="red")
abline(model2, lwd = 3, col="blue")
summary(model2)

model3 <- lm(SP.DYN.LE00.IN ~ SE.PRM.CMPT.ZS, data = df3)
plot(SE.PRM.CMPT.ZS, SP.DYN.LE00.IN, col="red")
abline(model3, lwd = 3, col="blue")
summary(model3)

model4 <- lm(SP.DYN.LE00.IN ~ SE.TER.CUAT.BA.ZS, data = df3)
plot(SE.TER.CUAT.BA.ZS, SP.DYN.LE00.IN, col="red")
abline(model4, lwd = 3, col="blue")
summary(model4)


model5 <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS, data = df3)
plot(EG.ELC.ACCS.ZS, SP.DYN.LE00.IN, col="red")
abline(model5, lwd = 3, col="blue")
summary(model5)


model6 <- lm(SP.DYN.LE00.IN ~ SH.H2O.SMDW.ZS, data = df3)
plot(SH.H2O.SMDW.ZS, SP.DYN.LE00.IN, col="red")
abline(model6, lwd = 3, col="blue")
summary(model6)

model7 <- lm(SP.DYN.LE00.IN ~ SP.DYN.IMRT.IN, data = df3)
plot(SP.DYN.IMRT.IN, SP.DYN.LE00.IN, col="red")
abline(model7, lwd = 3, col="blue")
summary(model7)

plot(EG.ELC.ACCS.ZS + NY.GDP.PCAP.CD + SE.ADT.LITR.ZS + SE.PRM.CMPT.ZS 
         + SE.TER.CUAT.BA.ZS + SH.H2O.SMDW.ZS + SP.DYN.IMRT.IN,SP.DYN.LE00.IN ,data = df3)
abline(model, lwd=3, col="blue")
summary(model)

summary(model)$coefficient

#NY.GDP.PCAP.CD = 5.708395e-05 * 5.7675394 = 0.00032923393
#SE.ADT.LITR.ZS = 5.077885e-02 * 0.8566781 = 0.04350112873
#SE.PRM.CMPT.ZS = 3.476691e-02 * 1.1483553 = 0.03992476536
#EG.ELC.ACCS.ZS = 1.152918e-01 * 7.2831442 = 0.83968680447
#SE.TER.CUAT.BA.ZS = 3.948916e-02 * 0.7909265 = 0.0312330231
#SH.H2O.SMDW.ZS = 8.621255e-02 * 7.0233663 = 0.6055023183
#SP.DYN.IMRT.IN = -2.397367e-01 * -13.1824459 = 0.76294907799

# from the above calculation we can clearly see that SH.H2O.SMDW.ZS and EG.ELC.ACCS.ZS
#are the predictors are which are significantly associated in changing the life expectancy
#data and hence are final model is given below

final_model1 <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN , data = df3)
plot(EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN , SP.DYN.LE00.IN , col ="red")
abline(final_model1, lwd = 3, col="blue")
summary(final_model1)
confint(final_model1)

final_model2 <- lm(SP.DYN.LE00.IN ~ SH.H2O.SMDW.ZS + SP.DYN.IMRT.IN, data = df3)
plot(SH.H2O.SMDW.ZS + SP.DYN.IMRT.IN, SP.DYN.LE00.IN , col ="red")
abline(final_model2, lwd = 3, col="blue")
summary(final_model2)
confint(final_model2)

final_model3 <- lm(SP.DYN.LE00.IN ~ SH.H2O.SMDW.ZS + EG.ELC.ACCS.ZS, data = df3)
plot(SH.H2O.SMDW.ZS + EG.ELC.ACCS.ZS, SP.DYN.LE00.IN , col ="red")
abline(final_model3, lwd = 3, col="blue")
summary(final_model3)
confint(final_model3)

final_model4 <- lm(SP.DYN.LE00.IN ~ SH.H2O.SMDW.ZS + EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN
                   , data = df3)

plot(x=predict(final_model4), y=final_model4$SP.DYN.LE00.IN,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)
summary(final_model4)
confint(final_model4)

AIC(final_model1)
AIC(final_model2)
AIC(final_model3)
AIC(final_model4)

install.packages("olsrr")
library(olsrr)
ols_mallows_cp(final_model1, final_model2)

#using final_model 2 as best model from above then comparing it with 3
ols_mallows_cp(final_model2, final_model3)

#final model 2 is better than final model 3 hence comparing it with final model 4

ols_mallows_cp(final_model2, final_model4)
ols_mallows_cp(final_model4, final_model2)
summary(final_model4)

#from above we can infer as the Mallow's CP value is lesser for final model 4 then it
#better than final model 2 hence final model 4 is better than all the final model.

#SH.H2O.SMDW.ZS + EG.ELC.ACCS.ZS + SP.DYN.IMRT.IN

#task 4  

# added continent column from original data to our final data fram that is df3
df3$Continent = myData$Continent

#copied the data to a new data table df3
df4 <- df3
setDF(df4)

#taking out mean of life expectancy target response on the basis of continents and setting
#value to group_mean
group_mean <- tapply(SP.DYN.LE00.IN, df4$Continent,mean) 
group_mean

#made a box plot of life expectancy with continent to check the data distribution
boxplot(df4$SP.DYN.LE00.IN~df4$Continent,main='Comparing Life Expetancy per Continent',
        xlab='Continent', col="light gray", ylab = "Avg Life Expectancy",)

# used one way anova method to compare mean of life expectancy in df4 to analysis the variance
# if the data is spread equally among all the continents or not.
anova_one_way_model<-aov(SP.DYN.LE00.IN~as.factor(df4$Continent),data=df4)
summary(anova_one_way_model)

# from above summary we found out that significance level that is Pr(>F) is less than 0.005 we 
# can conclude that there is significant difference among the group data.

# further to verify the above result we use Bonferroni post-hoc test to check the multiple comparison
# as there might be pairs which have significant difference among them but might not have significant
#difference from other predictors. 

cat("Bonferroni post-hoc test","\n")

pairwise.t.test(SP.DYN.LE00.IN, df4$Continent, p.adj = "bonferroni")

# from above test we can find out that below pair has significant difference in data from each other
# as there significance value is less then 0.005
# Asia - Africa
# Australia - Africa
#Europe - Africa
# North America - Africa
# South America - Africa
# Europe - Asia
# Europe - Australia/Oceania
# North America - Europe

cat("\n","Tukey post-hoc test","\n")

# using the tukey post hoc test we can make pair and then plot the same to visualize the 
# siginificant difference among pairs.
tukey.life_expectancy<-TukeyHSD(anova1way)
plot(tukey.life_expectancy)

# added residual values in our Analysis of Variance Table
df4$residulas1 <- anova1way$residuals

#making grid to plot 2 graphs
par(mfrow=c(1,2))

#creating a histogram and qq plot to check for our normality of residuals and also to check the 
# if the variance is same or not in all the groups
hist(df4$residulas1, main="Standardised residuals-histogram",
xlab="Standardised resduals")

qqnorm(df4$residulas1,pch=19)
qqline(df4$residulas1)

# to check if data is normally distributed or not. as the p value is greater than 0.05
# we found out that data is skewed
shapiro.test(df4$residulas1)

# omitting the missing values in order to get correct result for variance. 
df4 <- na.omit(df4)

# we did levene test to check the significance level of our data and check if our null hypothesis
# that is if the variance of data is accepted or not. 
leveneTest(df4$SP.DYN.LE00.IN~factor(df4$Continent), center=mean, data=df4)

# as checked from the above statement we found that significance level is less than 0.005 which means
# our null hypothesis is rejected -> the degree of variance is not same in all the groups. which means
# data is not equally spread on all groups which makes sense as we dont have same number of countries in 
#each continents which will make our variance rejected as data wont be spread equally in all continents



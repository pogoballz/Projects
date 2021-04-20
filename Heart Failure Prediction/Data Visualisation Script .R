getwd()
setwd( "/Volumes/TRNC/NTU/Y2S2/AI")

library(data.table)
#Exploring data - Looking at the data we have itself
df1 = fread("heart_failure_clinical_records_dataset.csv")
summary(df1)

library(ggplot2)
library(GGally)
pairs(~ . , panel=panel.smooth, data = df1)

df1[,.N, by = DEATH_EVENT]

#Severe Hyponatremia ??? serum sodium concentration <120 mEq/L 
#Moderate Hyponatremia ??? serum sodium concentration between 120 to 129 mEq/L 
#Mild Hyponatremia ??? serum sodium concentration between 130 to 134 mEq/L 

df1$serum_sodiumdummy = df1$serum_sodium
df1$serum_sodiumdummy = ifelse(df1$serum_sodiumdummy<120,1,df1$serum_sodiumdummy)
df1$serum_sodiumdummy = ifelse(df1$serum_sodiumdummy >=130, 3, df1$serum_sodiumdummy)
df1$serum_sodiumdummy = ifelse(df1$serum_sodiumdummy>4,2,df1$serum_sodiumdummy)
summary(df1)

df1$high_blood_pressure = as.factor(df1$high_blood_pressure)
df1$DEATH_EVENT = as.factor(df1$DEATH_EVENT)
df1$anaemia = as.factor(df1$anaemia)
colnames(df1)[3] = "CPK"

ggplot(unique(df1[,.(.N), by = .(serum_sodiumdummy,DEATH_EVENT)]), aes(x = serum_sodiumdummy, y = N)) + geom_col(aes(fill = serum_sodiumdummy)) + facet_grid(~DEATH_EVENT) + theme_bw()

ggplot(df1[,.N, by = .(high_blood_pressure, DEATH_EVENT)], aes(x = high_blood_pressure, y = N)) + geom_col(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(sex, DEATH_EVENT)], aes(x = sex, y = N)) + geom_col(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(serum_sodium, DEATH_EVENT)], aes(y = serum_sodium, x = DEATH_EVENT)) + geom_boxplot(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(CPK, DEATH_EVENT)], aes(x = CPK)) + geom_histogram(aes(fill = DEATH_EVENT)) + theme_bw()
ggplot(df1[,.N, by = .(CPK, DEATH_EVENT)], aes(y = CPK, x = DEATH_EVENT)) + geom_boxplot(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(age, DEATH_EVENT)], aes(y = age, x = DEATH_EVENT)) + geom_boxplot(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(ejection_fraction, DEATH_EVENT)], aes(x = ejection_fraction, y = N)) + geom_col(aes(fill = DEATH_EVENT)) + theme_bw()
ggplot(df1[,.N, by = .(ejection_fraction, DEATH_EVENT)], aes(y = ejection_fraction, x = DEATH_EVENT)) + geom_boxplot(aes(fill = DEATH_EVENT)) + theme_bw()

ggplot(df1[,.N, by = .(serum_creatinine, DEATH_EVENT)], aes(x = serum_creatinine)) + geom_histogram(aes(fill = DEATH_EVENT)) + theme_bw()





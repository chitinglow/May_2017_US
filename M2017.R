setwd("~/Desktop/learning/Playground/oesm17nat")

library(scales)
library(readxl)
library(Amelia)
library(psych)
library(knitr)
library(ggplot2)
library(ggalluvial)

#loading file
M2017 <- read_excel('national_M2017_dl.xlsx')

#file structure
str(M2017)

describe(M2017)

#checking missing values
missmap(M2017)

# using mean 
M2017_1 <- M2017[,c(2,3,4,5,7,8)]

#remove 1st row
M2017_1 <- M2017_1[-1,]


#convert into relevant data type
M2017_1$OCC_TITLE <- as.character(M2017_1$OCC_TITLE)
M2017_1$OCC_GROUP <- as.factor(M2017_1$OCC_GROUP)
M2017_1$CATEGORY <- as.factor(M2017_1$CATEGORY)
M2017_1$TOT_EMP <- as.numeric(M2017_1$TOT_EMP)
M2017_1$H_MEAN <- as.numeric(M2017_1$H_MEAN)
M2017_1$A_MEAN <- as.numeric(M2017_1$A_MEAN)

missmap(M2017_1)

#remove missing value
M2017_1.na <- na.omit(M2017_1)

#total employment by group
ggplot(M2017_1.na, aes(x = OCC_GROUP, y = TOT_EMP, fill = OCC_GROUP)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(-0,30000000), labels = comma) +
  labs(title = "Total Employment by Occupation Group", x = 'Occupation Group', y = "Total Employment") +
  guides(fill = guide_legend(title = "Occupation Group"))


#total employment by category
ggplot(M2017_1.na, aes(x = CATEGORY, y = TOT_EMP, fill = CATEGORY)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(-0,30000000), labels = comma) +
  labs(title = "Total Employment by Occupation Category", x = 'Occupation Category', y = "Total Employment") +
  guides(fill = guide_legend(title = "Occupation Category"))

#hourly wage
summary(M2017_1.na$H_MEAN)

M2017_1.na$Hour_wage <- cut(M2017_1.na$H_MEAN,
                         breaks = c(-Inf, 10.21, 27.74, Inf),
                         labels = c("low", "medium", "high"))

#annual income
M2017_1.na$Income <- cut(M2017_1.na$A_MEAN,
                         breaks = c(-Inf, 49999, 99999, Inf),
                         labels = c("low", "medium", "high"))


#catgorize the data
df <- table(M2017_1.na$OCC_GROUP, M2017_1.na$Hour_wage)
df1 <- table(M2017_1.na$OCC_GROUP, M2017_1.na$Income)
df2 <- table(M2017_1.na$CATEGORY, M2017_1.na$Hour_wage)
df3 <-  table(M2017_1.na$CATEGORY, M2017_1.na$Income)

df <- as.data.frame(df)
df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)
df3 <- as.data.frame(df3)

colnames(df) <- c("Group", "Wage (per Hour)", "Freq")
colnames(df1) <- c("Category", "Income (Annual)", "Freq")
colnames(df2) <- c("Group", "Wage (per Hour)", "Freq")
colnames(df3) <- c("Category", "Income (Annual)", "Freq")

#plot
ggplot(df, aes(x = Group, y = Freq, fill = `Wage (per Hour)`)) +
  geom_bar(stat = "identity", position = 'dodge')

ggplot(df1, aes(x = Category, y = Freq, fill = `Income (Annual)`)) +
  geom_bar(stat = "identity", position = 'dodge')

ggplot(df2, aes(x = Group, y = Freq, fill = `Wage (per Hour)`)) +
  geom_bar(stat = "identity", position = 'dodge')

ggplot(df3, aes(x = Category, y = Freq, fill = `Income (Annual)`)) +
  geom_bar(stat = "identity", position = 'dodge')

#sankey diagram for Wage per hour
df4 <- table(M2017_1.na$OCC_GROUP, M2017_1.na$CATEGORY, M2017_1.na$Hour_wage)
df4 <- as.data.frame(df4)
colnames(df4) <- c("Group", "Category", "Wage (per Hour)", "Freq")

ggplot(df4,
       aes(weight = Freq, axis1 = Group, axis2 = Category, axis3 = `Wage (per Hour)`)) +
  geom_alluvium(aes(fill = `Wage (per Hour)`), width = 1/12) +
  geom_stratum(width = 1/12, fill = "green", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:3, labels = c("Group", "Category", "Wage (per Hour)")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Wage Per Hour By Occupation Group and Professional Category in May 2017 (USA)") +
  theme_bw()

#sankey diagram for annual income

df5 <- table(M2017_1.na$OCC_GROUP, M2017_1.na$CATEGORY, M2017_1.na$Income)
df5 <- as.data.frame(df5)
colnames(df5) <- c("Group", "Category", "Income (Annual)", "Freq")

ggplot(df5,
       aes(weight = Freq, axis1 = Group, axis2 = Category, axis3 = `Income (Annual)`)) +
  geom_alluvium(aes(fill = `Income (Annual)`), width = 1/12) +
  geom_stratum(width = 1/12, fill = "green", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:3, labels = c("Group", "Category", "Income (Annual)")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Annual Income By Occupation Group and Professional Category in May 2017 (USA)") +
  theme_bw()

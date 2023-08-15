## STEP 1: INITIAL EXPLORATORY

# 1.1 Import data
# I have used Import Dataset option to import the file using From Text (base).. and then changed the data frame name to dfHollywoods 

# To install a package
install.packages("tidyverse")

# 1.2 Importing library
# I will be using "tidyverse" and "ggplot2" library. "tidyverse" is not installed in my environment so I will be using following code to install the package first and then call it to use.

# To install tidyverse
install.packages("tidyverse")

# importing the necessary libraries using the following code
library(tidyverse)
library(ggplot2)

# 1.3 View and checking my imported data
View(dfHollywoods)

dim(dfHollywoods)

head(dfHollywoods)


#1.4 Checking data types
str(dfHollywoods)




## STEP 2.A: CLEAN DATA

# 2.A.1 Checking for missing values
colSums(is.na(dfHollywoods))

# 2.A.2 Drop missing values
dfHollywoods <- na.omit(dfHollywoods)
View (dfHollywoods)
# There is one row which has missing value in column, which is escaped by the previous comand, we will be using following comand to filter that row and remove
dfHollywoods<-dfHollywoods%>%  filter(!row_number() %in% c(38))
dim(dfHollywoods)
View (dfHollywoods)

# 2.A.3 Check for duplicates and remove them.
dfHollywoods <- dfHollywoods[!duplicated(dfHollywoods$Film), ]
dim(dfHollywoods)
# there is an alternative way of removing duplicates using the following code
# dfHollywoods <- dfHollywoods %>% distinct(Film, .keep_all=TRUE)


# 2.A.4 Rounding off values to 2 decimle places. I have identifed two column Profitability and Worlwide.gross, needs rounding off.
dfHollywoods$Profitability<- round(dfHollywoods$Profitability, digit=2)
dfHollywoods$Worldwide.Gross<- round(dfHollywoods$Worldwide.Gross, digit=2)
#checking values after rounding off
head(dfHollywoods)

## STEP 2.B : OUTLIER

# 2.B.1 Check for outliers using a boxplot
ggplot(dfHollywoods, aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

# From the plot created by using above code, I can see there is outlier. So I will be removing it from both profitability and Worldwide.Gross

# 2.B.2 Now remove outliers in Profitability
Q1 <- quantile(dfHollywoods$Profitability, .25)
Q3 <- quantile(dfHollywoods$Profitability, .75)
IQR <- IQR(dfHollywoods$Profitability)
no_outliers <- subset(dfHollywoods, dfHollywoods$Profitability> (Q1 - 1.5*IQR) & dfHollywoods$Profitability< (Q3 + 1.5*IQR))
dim(no_outliers)

# 2.B.3 Removing outlier from Worldwide.Gross
Q1 <- quantile(dfHollywoods$Worldwide.Gross, .25)
Q3 <- quantile(dfHollywoods$Worldwide.Gross, .75)
IQR <- IQR(dfHollywoods$Worldwide.Gross)
no_outliers <- subset(dfHollywoods, dfHollywoods$Worldwide.Gross> (Q1 - 1.5*IQR) & dfHollywoods$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(no_outliers)

## STEP 3: EXPLORATORY DATA ANALYSIS

# 3.1 Summary Statistics
summary(no_outliers)
View (no_outliers)

# 3.2 Bivariate Analysis

# Rotten Tomatoes Rating Per Studio in Scatter Plot
ggplot(no_outliers, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point(aes(color = factor(Rotten.Tomatoes..)))+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

# Genre VS Profitability in Scatter Plot
ggplot(no_outliers, aes(x=Genre, y=Profitability)) + geom_point(aes(color = factor(Profitability)))+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 80))+theme(axis.text.x = element_text(angle = 90))

# Genre VS Worldwide.Gross in Scatter Plot
ggplot(no_outliers, aes(x=Genre, y=Worldwide.Gross)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 500))+theme(axis.text.x = element_text(angle = 90))

# No. of film by Year using Bar Chart
ggplot(no_outliers, aes(x=Year)) + geom_bar(fill="darkgreen")

# No. of film per Studio using Bar Chart
ggplot(no_outliers, aes(x=Lead.Studio)) + geom_bar(fill="darkblue")

# No. of film per Genre using Bar Chart
ggplot(no_outliers, aes(x=Genre)) + geom_bar(fill="brown")

# Audience Score in Histogram, using two color
ggplot(no_outliers, aes(Audience..score..)) + geom_histogram(bins = 35, fill = "brown", color = "blue")

# Rotten Tomatoes Rating in Histogram, using two color
ggplot(no_outliers, aes(Rotten.Tomatoes..)) + geom_histogram(bins = 35, fill = "darkblue", color = "green")

# Worlwide Gross in Histogram, using two color
ggplot(no_outliers, aes(Worldwide.Gross)) + geom_histogram(bins = 35, fill = "darkgreen", color = "darkblue")

# Audience Score Density
ggplot(no_outliers, aes(x = Audience..score..)) +geom_density(fill = "darkblue", color = "green")

# Rotten Tomatoes Ratting Density
ggplot(no_outliers, aes(x = Rotten.Tomatoes..)) +geom_density(fill = "darkblue", color = "green")

# 3.3 Find Correlation

## Find if there is any correlation between Profitability and Audience Score of Films

cor(no_outliers$Profitability,no_outliers$Audience..score..)
# ANS: 0.02430116, which is less than 1 but more than 0. We can say there is a positive correlation between Profitability and Audience Score of Films, but not Perfect Correlation. 

## Find if there is any correlation between Worlwide Gross and Audience Score of Films

cor(no_outliers$Worldwide.Gross,no_outliers$Audience..score..)
# ANS: 0.3653465, which is less than 1 but more than 0. We can say there is a positive correlation between Worldwide Gross and Audience Score of Films, but not Perfect Correlation. 

## Find if there is any correlation between Profitability and Rotten Tomatoes Rating of Films

cor(no_outliers$Profitability,no_outliers$Rotten.Tomatoes..)
# ANS: 0.04563012, which is less than 1 but more than 0. We can say there is a positive correlation between Profitability and Rotten Tomatoes Rating of Films, but not Perfect Correlation. 

## Find if there is any correlation between Worlwide Gross and Rotten Tomatoes Rating of Films

cor(no_outliers$Worldwide.Gross,no_outliers$Rotten.Tomatoes..)
# ANS: 0.03240535, which is less than 1 but more than 0. We can say there is a positive correlation between Worldwide Gross and Rotten Tomatoes Rating of Films, but not Perfect Correlation. 

## Find if there is any correlation between Worlwide Gross and Profitability
cor(no_outliers$Worldwide.Gross,no_outliers$Profitability)




## Liner Regression to predict: We think 'Audience..Score..' is independent variable
model<-lm(no_outliers$Worldwide.Gross ~ no_outliers$Audience..score.., data = no_outliers)
summary(model)

#It means a change in one unit in Audience..Score,, will bring 2.8374 units to change in WorldWide.Gross.




## STEP 4:  EXPORT

# Using following code to export a clean file to be used in PowerBi
write.csv(no_outliers, "clean_dfHollywoods.csv")

# I will also be creating excel file of my clean Dataframe, using "xlsx" package
install.packages("xlsx")

library(xlsx)
write.xlsx(no_outliers, "clean_dfHollywoods.xlsx")

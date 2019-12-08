################################################################################
# FinalProjectScript_MathewJacob.R

# Created by Mathew Jacob
# Last edited 12/8/2019 by Mathew Jacob

# INPUT Files:
# worldbankdata.csv
# corrections.csv

################################################################################



#### SETUP #####################################################################

# Imports necessary libraries
library("gapminder")
library("ggplot2")
library("dplyr")

# Imports necessary data sources
data("gapminder")
gapminder <- data.frame(gapminder)
worldbankdata <- read.csv("worldbankdata.csv", as.is = T)



#### DATA CLEANING #############################################################

# Fixes country names in gapminder data to match the World Bank Country Names
corrections <- read.csv("corrections.csv", as.is = T)
countries = c()
for (country in gapminder$country){
  if(country %in% corrections$wrong){
    correct <- corrections[corrections$wrong == country,]$right
    countries <- c(countries, correct)
  }else{
    countries <- c(countries, country)
  }
}
gapminder$country <- countries

# Gets list of all country names and regions
countryinfo <- gapminder[!duplicated(gapminder$country),]
countryinfo <- countryinfo[ ,-c(3:6)]

# Creates GDP dataframe from Gapminder Data
gdpPC <- gapminder
gdpPC[, c(2,4,5)] <- NULL
gdpPC <- reshape(gdpPC, direction = "wide", idvar = "country", timevar = "year")
gdpPC <- merge(countryinfo, gdpPC, by = "country", all.x = T, all.y = T)
rownames(gdpPC) <- NULL

# Creates Life Expectancy dataframe from Gapminder Data 
lfExp <- gapminder
lfExp[, c(2,5,6)] <- NULL
lfExp <- reshape(lfExp, direction = "wide", idvar = "country", timevar = "year")
lfExp <- merge(countryinfo, lfExp, by = "country", all.x = T, all.y = T)
rownames(lfExp) <- NULL

# Creates PM 2.5 Exposure (microgram/m^3) dataframe from Worldbank Data
pollution <- worldbankdata[worldbankdata$Series.Code == "EN.ATM.PM25.MC.M3", ]
pollution[, c(2,3,4)] <- NULL
pollution <- data.frame(pollution[, 1], apply(pollution[,-1], 2, as.numeric))
colnames(pollution) <- c("country", as.character(1960:2019))

# Creates Sanitation Access dataframe from Worldbank Data
sanitation <- worldbankdata[worldbankdata$Series.Code == "SH.STA.BASS.ZS", ]
sanitation[, c(2,3,4)] <- NULL
sanitation <- data.frame(sanitation[, 1], apply(sanitation[,-1], 2, as.numeric))
colnames(sanitation) <- c("country", as.character(1960:2019))

# Creates Drinking Water Access dataframe from Worldbank Data
water <- worldbankdata[worldbankdata$Series.Code == "SH.H2O.BASW.ZS", ]
water[, c(2,3,4)] <- NULL
water <- data.frame(water[, 1], apply(water[,-1], 2, as.numeric))
colnames(water) <- c("country", as.character(1960:2019))



#### DATA MERGING ##############################################################

# Gets 2007 Sanitation Data
sanitation2007 <- data.frame(sanitation$country, as.numeric(sanitation$`2007`))
colnames(sanitation2007) <- c("country","sanitation2007")

# Gets 2010 Pollution Data
pollution2010 <- data.frame(pollution$country, pollution$`2010`)
colnames(pollution2010) <- c("country","pollution2010")

# Gets 2007 Water Data
water2007 <- data.frame(pollution$country, as.numeric(water$`2007`))
colnames(water2007) <- c("country","water2007")

# Combines all data
merged <- data.frame(lfExp$country, lfExp$continent, 
                     lfExp$lifeExp.2007, gdpPC$gdpPercap.2007)
colnames(merged) <- c("country","continent","lifeExp2007","gdpPerCap2007")
merged <- merge(merged, sanitation2007, by = "country")
merged <- merge(merged, pollution2010, by = "country")
merged <- merge(merged, water2007, by = "country")



#### VISUALIZATION #############################################################

# Gets China Gapminder Data
Chinagapminder <- gapminder[gapminder$country == "China", ]

# Gets China Pollution Data
Chinapollution <- pollution[pollution$country == "China", -c(2:41)]
Chinapollution <- reshape(Chinapollution, direction = "long", 
                       varying = list(names(Chinapollution)[2:21]),
                       v.names = "value", idvar = c("country"),
                       timevar = "year", times = 2000:2019)

# Gets China Sanitation Data
Chinasanitation <- sanitation[sanitation$country == "China", -c(2:41)]
Chinasanitation <- reshape(Chinasanitation, direction = "long", 
                       varying = list(names(Chinasanitation)[2:21]), 
                       v.names = "value", idvar = c("country"),
                       timevar = "year", times = 2000:2019)

# Gets China Water Data
Chinawater <- water[water$country == "China", -c(2:41)]
Chinawater <- reshape(Chinawater, direction = "long", 
                        varying = list(names(Chinawater)[2:21]),
                        v.names = "value", idvar = c("country"),
                        timevar = "year", times = 2000:2019)

# Plots China GDP Over Time
ggplot(Chinagapminder) + geom_line(aes(x = year, y = gdpPercap)) + 
  ggtitle("China GDP Over Time") + 
  xlab("Year") + ylab("GDP Per Capita")

# Plots China Life Expectancy Over Time
ggplot(Chinagapminder) + geom_line(aes(x = year, y = lifeExp)) + 
  ggtitle("Life Expectancy Over Time") + 
  xlab("Year") + ylab("Life Expectancy")

# Plots China PM 2.5 Exposure Over Time
ggplot(Chinapollution) + geom_line(aes(x = year, y = value)) + 
  ggtitle("China PM 2.5 Exposure Over Time") + 
  xlab("Year") + ylab("PM 2.5 Exposure (micrograms per cubic meter")

# Plots China Sanitation Access Over Time
ggplot(Chinasanitation) + geom_line(aes(x = year, y = value)) + 
  ggtitle("China Sanitation Access Over Time") + 
  xlab("Year") + ylab("% of Population")

# Plots China Drinking Water Access Over Time
ggplot(Chinawater) + geom_line(aes(x = year, y = value)) + 
  ggtitle("China Drinking Water Access Over Time") + 
  xlab("Year") + ylab("% of Population")

# Plot GDP vs LFE and log transform GDP
plot(gdpPC$gdpPercap.2007, lfExp$lifeExp.2007, 
     main = "GDP Per Capita vs Life Expectancy", 
     xlab = "GDP Per Capita", ylab = "Life Expectancy")
merged$gdpPerCap2007 <- log(merged$gdpPerCap2007)


#### REGRESSION ANALYSIS #######################################################

model1 <- lm(lifeExp2007 ~ pollution2010, data = merged)

model2 <- lm(lifeExp2007 ~ water2007, data = merged)

model3 <- lm(lifeExp2007 ~ sanitation2007, data = merged)

model4 <- lm(lifeExp2007 ~ pollution2010 + gdpPerCap2007 , data = merged)
model4$AIC <- AIC(model4)
model4$BIC <- BIC(model4)  

model5 <- lm(lifeExp2007 ~ water2007 + gdpPerCap2007 , data = merged)
model5$AIC <- AIC(model5)
model5$BIC <- BIC(model5)

model6 <- lm(lifeExp2007 ~ sanitation2007 + gdpPerCap2007 , data = merged)
model6$AIC <- AIC(model6)
model6$BIC <- BIC(model6)

model7 <- lm(lifeExp2007 ~ water2007 + sanitation2007 + gdpPerCap2007 , data = merged)
model7$AIC <- AIC(model7)
model7$BIC <- BIC(model7)

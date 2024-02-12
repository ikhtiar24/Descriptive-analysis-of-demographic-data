census2002_2022 <- na.omit(census2002_2022)

library(ggplot2)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(reshape2)

#................1: Frequency distribution of the variables

#Histogram of Life.Expectancy.at.Birth..Both.Sexes in 2022
Life.exp.rate_2022 <- census2002_2022$Life.Expectancy.at.Birth..Both.Sexes[census2002_2022$Year == 2022]


hist(Life.exp.rate_2022, col = 'orange', main = "", freq = TRUE, xlim =c(0,120) , ylim = c(0,35), xlab = "Life expectancy at birth in Both Sexes",  breaks = 15 )
abline(v = mean(Life.exp.rate_2022), col="red", lwd=2)
abline(v = median(Life.exp.rate_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=1.2)


#Histogram of Under.Age.5.Mortality..Both.Sexes in 2022
mortality.rate_2022 <- census2002_2022$Under.Age.5.Mortality..Both.Sexes[census2002_2022$Year == 2022]

hist(mortality.rate_2022,col = 'orange', main = "",
     freq = TRUE, xlim =c(0,120) , ylim = c(0,80), xlab = "Mortality rate in both sexes",  breaks = 15 )
abline(v = mean(mortality.rate_2022), col="red", lwd=2)
abline(v = median(mortality.rate_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.8)


#Histogram of Life.Expectancy.at.Birth..Males in 2022
life.exp.male_2022 <- census2002_2022$Life.Expectancy.at.Birth..Males[census2002_2022$Year == 2022]

hist(life.exp.male_2022,col = 'orange', main = "",
     freq = TRUE, xlim =c(0,120) , ylim = c(0,40), xlab = "Life expentancy at birth in males",  breaks = 15 )
abline(v = mean(life.exp.male_2022), col="red", lwd=2)
abline(v = median(life.exp.male_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)

#Histogram of Life.Expectancy.at.Birth..Females in 2022
life.exp.female_2022 <- census2002_2022$Life.Expectancy.at.Birth..Females[census2002_2022$Year == 2022]
hist(life.exp.female_2022,col = 'orange', main = "",
     freq = TRUE, xlim =c(0,120) , ylim = c(0,35), xlab = "Life expentancy at birth in females",  breaks = 15 )
abline(v = mean(life.exp.female_2022), col="red", lwd=2)
abline(v = median(life.exp.female_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)

#Histogram of Under.Age.5.Mortality..Males in 2022
mortality.male_2022 <- census2002_2022$Under.Age.5.Mortality..Males[census2002_2022$Year == 2022]

hist(mortality.male_2022,col = 'orange', main = "",
     freq = TRUE, xlim =c(0,120) , ylim = c(0,80), xlab = "Under age 5 mortality rate in males",  breaks = 15 )
abline(v = mean(mortality.male_2022), col="red", lwd=2)
abline(v = median(mortality.male_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)

#Histogram of Under.Age.5.Mortality..Females in 2022
mortality.female_2022 <- census2002_2022$Under.Age.5.Mortality..Females[census2002_2022$Year == 2022]

hist(mortality.female_2022,col = 'orange', main = "",
     freq = TRUE, xlim =c(0,120) , ylim = c(0,100), xlab = "Under age 5 mortality rate in females",  breaks = 15 )
abline(v = mean(mortality.female_2022), col="red", lwd=2)
abline(v = median(mortality.female_2022), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)

#--- 1: contiue : boxplot region wise


#Boxplot for Life.Expectancy.at.Birth..Both.Sexes
life.exp.both <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                       life.exp = census2002_2022[ census2002_2022$Year == 2022, 'Life.Expectancy.at.Birth..Both.Sexes'])

life.exp.both <- life.exp.both[order(life.exp.both$Region), ]
plot8 <- ggplot(life.exp.both, aes(life.exp, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Life expectancy at birth in both sexes")

#Boxplot for Under.Age.5.Mortality..Both.Sexes
mor.both <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                       Mortality = census2002_2022[ census2002_2022$Year == 2022, 'Under.Age.5.Mortality..Both.Sexes'])

mor.both<- mor.both[order(mor.both$Region), ]
plot7 <- ggplot(mor.both, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Under age 5 mortality in both sexes")

grid.arrange(plot8,plot7, nrow=1,ncol=2)

#--------------------------
#Boxplot for Life.Expectancy.at.Birth..Males
life.exp.males <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                            life.exp = census2002_2022[ census2002_2022$Year == 2022, 'Life.Expectancy.at.Birth..Males'])

life.exp.males <- life.exp.males[order(life.exp.males$Region), ]
plot9 <- ggplot(life.exp.males, aes(life.exp, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Life expectancy at birth in males")


#Boxplot for Life.Expectancy.at.Birth..Females
life.expectancy.females <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                       Mortality = census2002_2022[ census2002_2022$Year == 2022, 'Life.Expectancy.at.Birth..Females'])

life.expectancy.females<- life.expectancy.females[order(life.expectancy.females$Region), ]
plot10 <- ggplot(life.expectancy.females, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Life expectancy at birth in females")

grid.arrange(plot9,plot10, nrow=1,ncol=2)

#---------------
#Boxplot for Under.Age.5.Mortality..Males
mor.males <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                        Mortality = census2002_2022[ census2002_2022$Year == 2022, 'Under.Age.5.Mortality..Males'])

mor.males <- mor.males[order(mor.males$Region), ]
plot11 <- ggplot(mor.males, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Under age 5 mortality in males")


#Boxplot for Under.Age.5.Mortality..Females
mor.females <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022, 'Region'],
                                      Mortality = census2002_2022[ census2002_2022$Year == 2022, 'Under.Age.5.Mortality..Females'])

mor.females.females<- mor.females[order(mor.females$Region), ]
plot12 <- ggplot(mor.females, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Under age 5 mortality in females")

grid.arrange(plot11,plot12, nrow=1,ncol=2)




#.................2:homogeneous within the individual subregions and heterogeneous between different subregions

#Boxplot for Under.Age.5.Mortality..Both.Sexes
data_mor <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Region'],
                       Subregion = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Subregion'],
                       Under.Age.5.Mortality.Both.Sexes = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Under.Age.5.Mortality..Both.Sexes'])

data_mor <- data_mor[order(data_mor$Region, data_mor$Subregion), ]
data_mor$Subregion <- factor(data_mor$Subregion, levels = rev(unique(data_mor$Subregion)), ordered = TRUE)
ggplot(data_mor, aes(Under.Age.5.Mortality.Both.Sexes, Subregion, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Subregions")



#Boxplot for Life.Expectancy.at.Birth..Both.Sexes
data_exp <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Region'],
                       Subregion = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Subregion'],
                       Life.Expectancy.at.Birth.Both.Sexes = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Life.Expectancy.at.Birth..Both.Sexes'])

data_exp <- data_exp[order(data_exp$Region, data_exp$Subregion), ]
data_exp$Subregion <- factor(data_exp$Subregion, levels = rev(unique(data_exp$Subregion)), ordered = TRUE)
ggplot(data_exp, aes(Life.Expectancy.at.Birth.Both.Sexes, Subregion, fill=Region)) + geom_boxplot() + coord_flip()+ labs(y= "Subregions")



#..................3: Bivariate correlation



plot <- cor(census2002_2022[ census2002_2022$Year == 2022, 6:11], method = "pearson")
colnames(plot) <- c("Life expectancy at birth in both sexes", "Life expectancy at birth in males", "Life expectancy at birth in females", "Mortality rate in both sexes", "Mortality rate in males", "Mortality rate in females")
rownames(plot) <- c("Life expectancy at birth in both sexes", "Life expectancy at birth in males", "Life expectancy at birth in females", "Mortality rate in both sexes", "Mortality rate in males", "Mortality rate in females")
corrplot(plot, method = "number", type = "upper", mar=c(0,0,0,0), tl.col = "black", tl.srt = 45)





#.........4: comparing 2002 with 2022

Country <- census2002_2022$Country
Subregion <- census2002_2022$Subregion

#Under.Age.5.Mortality..Both.Sexes 2002 vs 2022
data_fert <- dcast(census2002_2022, Country + Subregion  + Region ~ factor(Year), value.var="Under.Age.5.Mortality..Both.Sexes")
data_fert <- data_fert[order(data_fert$Region, data_fert$Subregion), ]
data_fert$Subregion <- factor(data_fert$Subregion, levels = rev(unique(data_fert$Subregion)), ordered = TRUE)
ggplot(data=data_fert, mapping = aes(x=`2002`, y=`2022`)) +
  geom_point() + facet_wrap(. ~ Subregion) + 
  geom_abline()


# Life.Expectancy.at.Birth..Both.Sexes 2002 vs 2022
data_life_exp <- dcast(census2002_2022, Country + Subregion + Region ~ factor(Year), value.var="Life.Expectancy.at.Birth..Both.Sexes")
data_life_exp <- data_life_exp[order(data_life_exp$Region, data_life_exp$Subregion), ]
data_life_exp$Subregion <- factor(data_life_exp$Subregion, levels = rev(unique(data_life_exp$Subregion)), ordered = TRUE)
ggplot(data=data_life_exp, mapping = aes(x=`2002`,y=`2022`)) +
  geom_point() + facet_wrap(. ~ Subregion) + 
  geom_abline()























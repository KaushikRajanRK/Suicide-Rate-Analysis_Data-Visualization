
#Setting up working directory
setwd('D:/Education/IRELAND/NCI/Moodle Documents/SEM 2/DV/dataset')

# Importing the dataset
suicide<-read.csv('master.csv')
str(suicide)
colnames(suicide)<-c("country","year","sex","age", "suicide_count","population","suicides/100k pop","country-year","HDI for year","gdp_for_year ($)","gdp_per_capita ($)","generation")


suicide_corr<-suicide
suicide_corr$year<-as.numeric(suicide$year)
suicide_corr$suicide_count<-as.numeric(suicide$suicide_count)
suicide_corr$population<-as.numeric(suicide$population)
suicide_corr$`suicides/100k pop`<-as.numeric(suicide$`suicides/100k pop`)
suicide_corr$`HDI for year`<-as.numeric(suicide$`HDI for year`)
suicide_corr$`gdp_per_capita ($)`<-as.numeric(suicide$`gdp_per_capita ($)`)
suicide_corr<- suicide_corr[c(2,5,6,7,11)]
suicide_corr<-as.data.frame(suicide_corr)
str(suicide_corr)

// Correlation plot

library(corrplot)
forcorrplot<- cor(suicide_corr)
corrplot(forcorrplot,order = "AOE", method = "color",bg="green",addCoef.col = "gray")
?corrplot

write.csv(suicide_corr,"forcorrplot.csv")


countrydeathcount<-as.data.frame(read.csv("countrywisedeaths.csv"))

countrydeathcount<- as.data.frame(countrydeathcount)
str(countrydeathcount)
countrydeathcount$suicides_no<-as.numeric(countrydeathcount$suicides_no)
// countrydeathcount$country<-as.list(countrydeathcount$country)

agg = aggregate(countrydeathcount$suicides_no,
                by = list(countrydeathcount$country),FUN = sum)
?aggregate
rm(by)
aggregate()
write.csv(agg,"agg.csv")
plot(suicide$population,suicide$`gdp_for_year ($)`)
?plot
scaling<- read.csv("fr scaling.csv")
Scaling$Suicide.Count<-scale(scaling$Suicide.Count)
scale(scaling$Life.Ladder)

scale(scaling$gdp_per_capita....)


library(dplyr)
library(reshape2)
Summary <- suicide %>%
  group_by(year,age) %>%
  summarise(Net = sum(suicide_count))
write.csv(Summary,"summary.csv")

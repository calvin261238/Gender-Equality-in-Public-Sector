#1. Data & Library Load
#dplyr, tidyr, ggplot2
library(dplyr)
library(tidyr)
library(ggplot2)

#Public Sector data 
data <- read.csv("data.csv",header=T, sep=",")

data <- data[,-5:-7]
data_2000_2017 <- na.omit(data)

test <- data_2000_2017%>%
  mutate(change_Female_15_00 = X2015-X2000)%>%
  mutate(change_Female_17_15 = X2017-X2015)%>%
  mutate(MX2000 = 100-X2000)%>%
  mutate(MX2015 = 100-X2015)%>%
  mutate(MX2017 = 100-X2017)%>%
  mutate(change_Male_15_00 = MX2015-MX2000)%>%
  mutate(change_Male_17_15 = MX2017-MX2015)


#Which countries value women equal rughts the most & the least from 2000~2015
Worst_improve_MDG <- test%>%
  arrange(desc(change_Male_15_00))%>%
  head(3)

Top_improve_MDG <- test%>%
  arrange(desc(change_Female_15_00))%>%
  head(3)

#Visualizing data from 2000~2015
ggplot(test, aes(x=change_Female_15_00,y=change_Male_15_00,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=-17.5,linetype=3)+
  geom_vline(xintercept=17.5,linetype=3)+
  scale_x_continuous(limits=c(-10,45))+
  scale_y_continuous(limits=c(-45,10))+
  geom_text(data=Top_improve_MDG,size=3,check_overlap = T)+
  geom_text(data=Worst_improve_MDG,size=3, check_overlap = T)+
  labs(title="Gender Inequality in Parliaments",
       subtitle="Proportion changes from 2000 to 2015",
       caption="Source: World Bank",
       x="Changes of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()

#Which countries value women equal rughts the most & the least from 2015~2017
Worst_improve_SDG <- test%>%
  arrange(desc(change_Male_17_15))%>%
  head(3)

Top_improve_SDG <- test%>%
  arrange(desc(change_Female_17_15))%>%
  head(3)
  
#Visualizing data from 2015~2017
ggplot(test, aes(x=change_Female_17_15,y=change_Male_17_15,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=5,linetype=3)+
  geom_vline(xintercept=-5,linetype=3)+
  scale_x_continuous(limits=c(-25,15))+
  scale_y_continuous(limits=c(-15,25))+
  geom_text(data=Top_improve_SDG,size=3,check_overlap = T)+
  geom_text(data=Worst_improve_SDG,size=3, check_overlap = T)+
  labs(title="Gender Inequality in Parliaments",
       subtitle="Proportion changes from 2015 to 2017",
       caption="Source: World Bank",
       x="Chnages of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()

#Now let's examine each districts gender equality

Districts <- test%>%
  slice(c(4,19,34,36,37,38,39,76,80,93,100,111,127,128,129))

#Which districts value women equal rughts the most & the least from 2000~2015
Worst_improve_D15 <- Districts%>%
  arrange(desc(change_Male_15_00))%>%
  head(3)

Top_improve_D15 <- Districts%>%
  arrange(desc(change_Female_15_00))%>%
  head(3)

#Visualizing data from 2000~2015
ggplot(Districts, aes(x=change_Female_15_00,y=change_Male_15_00,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=-7.5,linetype=3)+
  geom_vline(xintercept=7.5,linetype=3)+
  scale_x_continuous(limits=c(-10,25))+
  scale_y_continuous(limits=c(-25,10))+
  geom_text(data=Top_improve_D15,size=3,check_overlap = T)+
  geom_text(data=Worst_improve_D15,size=3, check_overlap = T)+
  labs(title="Gender Inequality in Parliaments by Districts",
       subtitle="Proportion changes from 2000 to 2015",
       caption="Source: World Bank",
       x="Changes of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()
  
#Which districts value women equal rughts the most & the least from 2015~2017
Worst_improve_D17 <- Districts%>%
  arrange(desc(change_Male_17_15))%>%
  head(3)

Top_improve_D17 <- Districts%>%
  arrange(desc(change_Female_17_15))%>%
  head(3)

#Visualizing data from 2015~2017
ggplot(Districts, aes(x=change_Female_17_15,y=change_Male_17_15,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=0,linetype=3)+
  geom_vline(xintercept=0,linetype=3)+
  scale_x_continuous(limits=c(-5.5,5.5))+
  scale_y_continuous(limits=c(-5.5,5.5))+
  geom_text(data=Top_improve_D17,size=3,check_overlap = T)+
  geom_text(data=Worst_improve_D17,size=3, check_overlap = T)+
  labs(title="Gender Inequality in Parliaments by Districts",
       subtitle="Proportion changes from 2015 to 2017",
       caption="Source: World Bank",
       x="Changes of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()

#Now let's see gender improvement of countries if define by Income
Income <- test%>%
  slice(c(81,83,84,96,146))

#From 2000-2015
Income_15_00 <- Income%>%
  arrange(desc(change_Female_15_00))

#Visualizing data from 2000~2015
ggplot(Income, aes(x=change_Female_15_00,y=change_Male_15_00,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=-10,linetype=3)+
  geom_vline(xintercept=10,linetype=3)+
  scale_x_continuous(limits=c(7.5,12.5))+
  scale_y_continuous(limits=c(-12.5,-7.5))+
  geom_text(data=Income_15_00,size=3,check_overlap = T)+
  labs(title="Gender Inequality in Parliaments by Income",
       subtitle="Proportion changes from 2000 to 2015",
       caption="Source: World Bank",
       x="Changes of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()

#From 2015~2017
Income_17_15 <- Income%>%
  arrange(desc(change_Female_17_15))

#Visualizing data from 2015~2017
ggplot(Income_17_15, aes(x=change_Female_17_15,y=change_Male_17_15,label=Country.Name))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5)+
  geom_hline(yintercept=-0.625,linetype=3)+
  geom_vline(xintercept=0.625,linetype=3)+
  scale_x_continuous(limits=c(0,1.2))+
  scale_y_continuous(limits=c(-1.2,0))+
  geom_text(data=Income_17_15,size=3,check_overlap = T)+
  labs(title="Gender Inequality in Parliaments by Income",
       subtitle="Proportion changes from 2015 to 2017",
       caption="Source: World Bank",
       x="Changes of Proportion of seats held by Women",
       y="Changes of Proportion of seats held by Men")+
  theme_bw()

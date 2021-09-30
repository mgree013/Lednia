#Project: Lednia Review Paper Analysis
#Title:Stoneflies in the genus Lednia (Plecoptera: Nemouridae): sentinels of climate change impacts on mountain stream biodiversity
#journal: Biodiversity and Conservation
#Date: Sept 30,2021

######################################################
library(tidyverse)
library(ggplot2)
library(cowplot)
######################################################

data<-read.csv("Data/Lednia.analysis.csv")
newer_dataz<-read.csv("Data/new_dataz.csv")
new_data<-read.csv("Data/lednia_new_data.csv")

######################################################

#Figure 4:
elev.sp<-data%>%
  ggplot(aes(x=Species,y=Elev.m, fill=Species))+
  geom_boxplot()+
  ylab("Elevation (m)")+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

dist.sp<-data%>%
  filter(D.From.Source.m< 5000)%>%
  ggplot(aes(x=Species,y=D.From.Source.m, fill=Species))+
  geom_boxplot()+
  ylab("Distance from Source (m)")+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.box.background = element_rect(colour = "black", linetype='solid'))+
  theme(legend.position = c(.6,.8))

dist.genus<-data%>%
  ggplot(aes(x=Genus,y=D.From.Source.m, fill=Genus))+
  geom_boxplot()+
  ylab("Distance from Source (m)")

elev.genus<-data%>%
  ggplot(aes(x=Genus,y=Elev.m, fill=Genus))+
  geom_boxplot()+
  ylab("Elevation (m)")+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

#plot both figs
plot_grid(elev.sp,dist.sp)

#Stats
elev.anova<-aov(Elev.m~Species, data=data)
summary(elev.anova)
TukeyHSD(elev.anova)

sd(data$D.From.Source.m)
stderr(data$D.From.Source.m)
dist.anova<-aov(D.From.Source.m~Species, data=data)
summary(dist.anova)
TukeyHSD(dist.anova)

##########################################################################################
#Figure 3: Population discovery
#by species

new_dataz<-new_data%>%
  add_column(number=1)%>%
  group_by(Species)%>%
  mutate(total.number.pops=sum(number))%>%
  ungroup()%>%
  group_by(Species, Year,total.number.pops)%>%
  summarise(number.pops=sum(number))%>%
  ungroup()%>%
  drop_na()

new_dataz%>%
  ggplot(aes(x=Year, y=number.pops, colour=Species))+
  geom_point()+
  geom_line()

#write.csv(new_dataz, "new_dataz.csv")  

newer_dataz%>%
  ggplot(aes(x=Year, y=new.pops, colour=Species))+
  geom_point()+
  geom_line()+
  ylab("Cumulative Number of Popualtions Discovered")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"))



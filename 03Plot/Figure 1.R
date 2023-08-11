#Augest 2017, Yansen Xu (ysxu_st@rcees.ac.cn)-----------
rm(list=ls())
library(tidyverse)
library(rio)
library(lubridate)
library(agricolae)
library(ez)
library(car)
library(ggforce)
library(psych)
library(corrplot)
library(RColorBrewer)
library(showtext)
library(sysfonts)
library(showtextdb)
library(lme4)
library(nlme)
library(emmeans)
library(ggsci)

windowsFonts(Times=windowsFont("Times New Roman"))
#input folder-------
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/O3浓度和日期数据/Plot O3"
figure.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"

#ozone data---------
China_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=5)
China_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='China')->China_ozone

India_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=6)
#India_ozone %>% select(-Ref.50,-Ref.26)  %>% pivot_longer(cols = 3:7) %>% mutate(Area='India')->India_ozone
India_ozone %>% pivot_longer(cols = 3:9) %>% mutate(Area='India')->India_ozone


Europe_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=7)
Europe_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='Europe')->Europe_ozone

America_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=8)
America_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='North America')->America_ozone

DF.ozone<-rbind(China_ozone,India_ozone,Europe_ozone,America_ozone)



dodge <- position_dodge(width=0.9)
#China-------
ggplot(DF.ozone %>% filter(Area=="China"),aes(x=Date,y=value))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-3-19"), xmax=as.POSIXct("2023-5-23"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = F)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_npg()+
  geom_hline(yintercept =40,linetype="dashed",size=1, colour="black")+
  expand_limits(x=as.POSIXct(c("2022-10-1","2023-8-30")))+
  expand_limits(y=c(0,100))+
  scale_x_datetime(limits = as.POSIXct(c("2022-10-1","2023-8-30")),date_breaks = "29 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks=seq(0,100,20))+
  labs(y=as.expression('Mean'~O[3]~'concentration'~"("~"ppb"~")"))+
  labs(x=c("Date"))+

  geom_segment(aes(x = as.POSIXct("2022-11-9"), y =2 , xend = as.POSIXct("2022-11-9"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2022-11-9"),y = 4),label='sowing',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-5-1"), y =2 , xend = as.POSIXct("2023-5-1"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-5-1"),y = 4),label='mid-anthesis',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-6-2"), y =2 , xend = as.POSIXct("2023-6-2"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-6-2"),y = 4),label='harvest',size = 3,color = "black",family="Times",position = dodge)+
  
  #add the marked of the 90 days in the figures 
  geom_segment(aes(x = as.POSIXct("2023-6-2"), y =15 , xend = as.POSIXct("2023-3-5"), yend = 15),arrow = arrow(length = unit(3, "mm"),ends="both"),size = 1,  colour = "black")+
  geom_text(aes(x = as.POSIXct("2023-4-18"),y = 20),label='90 d',size = 4,color = "black",family="Times",position = dodge)+
 
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    #legend.justification=c(1,1), 
    #legend.position=c(0.25,0.2),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.4,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(size=7,hjust = 0,colour="black", face = "bold"),
    #图底板折被子
    panel.background = element_rect(fill = 'white'),
    panel.border=element_rect(fill = NA,colour = "black",size = 0.5),
    panel.spacing.y = unit(.5,"cm"),
    panel.grid = element_blank(),
    #坐标轴
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black",size = 0),
    #坐标轴文字
    axis.title.y=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    axis.title.x=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    #轴刻度
    axis.text.y=element_text(colour='black',size=9),
    axis.text.x=element_text(colour = "black",size = 9,angle = 0,hjust = 0.5,vjust = 0),
    #分面标题
    strip.text = element_text(colour = "black",size = 13,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.8,color="black"))
ggsave(file.path(figure.folder,"China_O3.png"),width = 20, height = 14,units = "cm",dpi=400)
  
  
  
#India-----
ggplot(DF.ozone %>% filter(Area=="India"),aes(x=Date,y=value))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2022-12-10"), xmax=as.POSIXct("2023-4-11"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = F)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_npg()+
  geom_hline(yintercept =40,linetype="dashed",size=1, colour="black")+
  expand_limits(x=as.POSIXct(c("2022-10-1","2023-8-30")))+
  expand_limits(y=c(0,100))+
  scale_x_datetime(limits = as.POSIXct(c("2022-10-1","2023-8-30")),date_breaks = "29 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks=seq(0,100,20))+
  labs(y=as.expression('Mean'~O[3]~'concentration'~"("~"ppb"~")"))+
  labs(x=c("Date"))+
  
  geom_segment(aes(x = as.POSIXct("2022-12-1"), y =2 , xend = as.POSIXct("2022-12-1"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2022-12-1"),y = 4),label='sowing',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-2-18"), y =2 , xend = as.POSIXct("2023-2-18"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-2-18"),y = 4),label='mid-anthesis',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-4-13"), y =2 , xend = as.POSIXct("2023-4-13"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-4-13"),y = 4),label='harvest',size = 3,color = "black",family="Times",position = dodge)+
  
  #add the marked of the 90 days in the figures 
  geom_segment(aes(x = as.POSIXct("2023-1-14"), y =15 , xend = as.POSIXct("2023-4-13"), yend = 15),arrow = arrow(length = unit(3, "mm"),ends="both"),size = 1,  colour = "black")+
  geom_text(aes(x = as.POSIXct("2023-2-27"),y = 20),label='90 d',size = 4,color = "black",family="Times",position = dodge)+
  
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    #legend.justification=c(1,1), 
    #legend.position=c(0.25,0.2),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.4,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(size=7,hjust = 0,colour="black", face = "bold"),
    #图底板折被子
    panel.background = element_rect(fill = 'white'),
    panel.border=element_rect(fill = NA,colour = "black",size = 0.5),
    panel.spacing.y = unit(.5,"cm"),
    panel.grid = element_blank(),
    #坐标轴
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black",size = 0),
    #坐标轴文字
    axis.title.y=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    axis.title.x=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    #轴刻度
    axis.text.y=element_text(colour='black',size=9),
    axis.text.x=element_text(colour = "black",size = 9,angle = 0,hjust = 0.5,vjust = 0),
    #分面标题
    strip.text = element_text(colour = "black",size = 13,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.8,color="black"))
ggsave(file.path(figure.folder,"India_O3.png"),width = 20, height = 14,units = "cm",dpi=400)


#Europe------
ggplot(DF.ozone %>% filter(Area=="Europe"),aes(x=Date,y=value))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-5-28"), xmax=as.POSIXct("2023-8-18"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = F)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_npg()+
  geom_hline(yintercept =40,linetype="dashed",size=1, colour="black")+
  expand_limits(x=as.POSIXct(c("2022-10-1","2023-8-30")))+
  expand_limits(y=c(0,100))+
  scale_x_datetime(limits = as.POSIXct(c("2022-10-1","2023-8-30")),date_breaks = "29 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks=seq(0,100,20))+
  labs(y=as.expression('Mean'~O[3]~'concentration'~"("~"ppb"~")"))+
  labs(x=c("Date"))+
  
  geom_segment(aes(x = as.POSIXct("2023-4-19"), y =2 , xend = as.POSIXct("2023-4-19"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-4-19"),y = 4),label='sowing',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-7-2"), y =2 , xend = as.POSIXct("2023-7-2"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-7-2"),y = 4),label='mid-anthesis',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-8-21"), y =2 , xend = as.POSIXct("2023-8-21"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-8-21"),y = 4),label='harvest',size = 3,color = "black",family="Times",position = dodge)+
  #add the marked of the 90 days in the figures 
  geom_segment(aes(x = as.POSIXct("2023-5-24"), y =15 , xend = as.POSIXct("2023-8-21"), yend = 15),arrow = arrow(length = unit(3, "mm"),ends="both"),size = 1,  colour = "black")+
  geom_text(aes(x = as.POSIXct("2023-7-7"),y = 20),label='90 d',size = 4,color = "black",family="Times",position = dodge)+
  
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    #legend.justification=c(1,1), 
    #legend.position=c(0.25,0.2),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.4,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(size=7,hjust = 0,colour="black", face = "bold"),
    #图底板折被子
    panel.background = element_rect(fill = 'white'),
    panel.border=element_rect(fill = NA,colour = "black",size = 0.5),
    panel.spacing.y = unit(.5,"cm"),
    panel.grid = element_blank(),
    #坐标轴
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black",size = 0),
    #坐标轴文字
    axis.title.y=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    axis.title.x=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    #轴刻度
    axis.text.y=element_text(colour='black',size=9),
    axis.text.x=element_text(colour = "black",size = 9,angle = 0,hjust = 0.5,vjust = 0),
    #分面标题
    strip.text = element_text(colour = "black",size = 13,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.8,color="black"))
ggsave(file.path(figure.folder,"Europe_O3.png"),width = 20, height = 14,units = "cm",dpi=400)

#America----
ggplot(DF.ozone %>% filter(Area=="North America"),aes(x=Date,y=value))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-4-24"), xmax=as.POSIXct("2023-6-18"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = F)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_npg()+
  geom_hline(yintercept =40,linetype="dashed",size=1, colour="black")+
  expand_limits(x=as.POSIXct(c("2022-10-1","2023-8-30")))+
  expand_limits(y=c(0,100))+
  scale_x_datetime(limits = as.POSIXct(c("2022-10-1","2023-8-30")),date_breaks = "29 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks=seq(0,100,20))+
  labs(y=as.expression('Mean'~O[3]~'concentration'~"("~"ppb"~")"))+
  labs(x=c("Date"))+
  
  geom_segment(aes(x = as.POSIXct("2022-10-22"), y =2 , xend = as.POSIXct("2022-10-22"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2022-10-22"),y = 4),label='sowing',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-5-17"), y =2 , xend = as.POSIXct("2023-5-17"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-5-17"),y = 4),label='mid-anthesis',size = 3,color = "black",family="Times",position = dodge)+
  
  geom_segment(aes(x = as.POSIXct("2023-6-24"), y =2 , xend = as.POSIXct("2023-6-24"), yend = -8),arrow = arrow(length = unit(3, "mm"),type="closed"),size = 0.8,  colour = "#EC7014")+
  geom_text(aes(x = as.POSIXct("2023-6-24"),y = 4),label='harvest',size = 3,color = "black",family="Times",position = dodge)+
  
  #add the marked of the 90 days in the figures 
  geom_segment(aes(x = as.POSIXct("2023-3-27"), y =15 , xend = as.POSIXct("2023-6-24"), yend = 15),arrow = arrow(length = unit(3, "mm"),ends="both"),size = 1,  colour = "black")+
  geom_text(aes(x = as.POSIXct("2023-5-10"),y = 20),label='90 d',size = 4,color = "black",family="Times",position = dodge)+
  
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    #legend.justification=c(1,1), 
    #legend.position=c(0.25,0.2),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.4,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(size=7,hjust = 0,colour="black", face = "bold"),
    #图底板折被子
    panel.background = element_rect(fill = 'white'),
    panel.border=element_rect(fill = NA,colour = "black",size = 0.5),
    panel.spacing.y = unit(.5,"cm"),
    panel.grid = element_blank(),
    #坐标轴
    axis.ticks.length = unit(0.4,"lines"), 
    axis.ticks = element_line(color='black'),
    axis.line = element_line(colour = "black",size = 0),
    #坐标轴文字
    axis.title.y=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    axis.title.x=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    #轴刻度
    axis.text.y=element_text(colour='black',size=9),
    axis.text.x=element_text(colour = "black",size = 9,angle = 0,hjust = 0.5,vjust = 0),
    #分面标题
    strip.text = element_text(colour = "black",size = 13,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.8,color="black"))
ggsave(file.path(figure.folder,"America_O3.png"),width = 20, height = 14,units = "cm",dpi=400)



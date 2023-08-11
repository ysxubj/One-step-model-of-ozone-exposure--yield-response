# Author Yansen Xu  e-mail:yansenxu@nuist.edu.cn
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
library(ggbreak) 
library(ggplot2)
library(cli)
library(ggpattern)

windowsFonts(Times=windowsFont("Times New Roman"))
#input folder-------
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Output result/wheat"
figure.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"

#Yield----------
China<-"Aisa_wheat_result_China.xlsx"
India<-"Aisa_wheat_result_India.xlsx"
Europe<-"Europe_wheat_result.xlsx"
America<-"N_America_withoutKohut_wheat_result_estY.xlsx"

China_yield<-import(file.path(input.folder,China))
China_yield$Area<-"China"
India_yield<-import(file.path(input.folder,India))
India_yield$Area<-"India"
Europe_yield<-import(file.path(input.folder,Europe))
Europe_yield$Area<-"Europe"
America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
DF.line<-rbind(China_yield,India_yield,Europe_yield,America_yield)

DF.line %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(data.frame(`country of experiment`='China',import(file.path(input.folder,China),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='India',import(file.path(input.folder,India),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='Europe',import(file.path(input.folder,Europe),sheet=2)),
               data.frame(`country of experiment`='North America',import(file.path(input.folder,America),sheet=2)))

DF.plot$country.of.experiment<-factor(DF.plot$country.of.experiment, levels=c("China", "India","Europe",'North America'), ordered=TRUE)


#AOT40
ggplot()+
  #geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.3),label='(a)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain yield")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  xlim(0,40)+
  ylim(0,1.3)+
  #expand_limits(x=c(0,40),y=c(0,1.3))
  #scale_x_continuous(breaks=seq(0,30,10))+
  scale_y_continuous(breaks=seq(0,1.3,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_color_npg()
#scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.32,0.26),
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-Yield.png"),width = 11, height = 10,units = "cm",dpi=400)


DF.line$Area<-factor(DF.line$Area, levels=c("China", "India","Europe",'North America'), ordered=TRUE)

#AOT40
ggplot()+
  geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  #geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  #geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.3),label='(b)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain yield")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  scale_y_continuous(breaks=seq(0,1.3,0.2),limits =c(0,1.3))+
  scale_x_continuous(breaks=seq(0,40,10),limits =c(0,40))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    #legend.position=c(0.32,0.26),
    legend.position = 'none',
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-Yield_b.png"),width = 11, height = 10,units = "cm",dpi=400)

#M12
ggplot()+
  #geom_ribbon(data=DF.line,aes(x=M12,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(M12),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  geom_point(data=DF.plot,aes(x=as.numeric(M12),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(M12,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(M12  ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(M12  ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 80, y =1.3),label='(a)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain yield")))+
  labs(x=expression(atop("M12 (ppb)",paste(''))))+
  ylim(0,1.3)+
  xlim(20,80)+
  #expand_limits(x=c(20,90),y=c(0,1))+
  #scale_x_continuous(breaks=seq(20,80,10))+
  scale_y_continuous(breaks=seq(0,1.3,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.32,0.26),
    #legend.position="none",
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"M12-Yield.png"),width = 11, height = 10,units = "cm",dpi=400)

#M12
ggplot()+
  geom_ribbon(data=DF.line,aes(x=M12,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=F)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  #geom_point(data=DF.plot,aes(x=as.numeric(M12),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  #geom_point(data=DF.plot,aes(x=as.numeric(M12),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(M12,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(M12  ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(M12  ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 80, y =1.3),label='(b)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain yield")))+
  labs(x=expression(atop("M12 (ppb)",paste(''))))+
  scale_y_continuous(breaks=seq(0,1.3,0.2),limits =c(0,1.3))+
  scale_x_continuous(breaks=seq(20,80,20),limits =c(20,80))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    #legend.position=c(0.32,0.26),
    legend.position = "none",
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"M12-Yield_b.png"),width = 11, height = 10,units = "cm",dpi=400)

#1000 grain----------
China<-"Aisa_wheat_result_1000_China.xlsx"
India<-"Aisa_wheat_result_1000_India.xlsx"
Europe<-"Europe_wheat_result_1000.xlsx"
America<-"N_America_withoutKohut_wheat_result_estY_1000.xlsx"

China_yield<-import(file.path(input.folder,China))
China_yield$Area<-"China"
India_yield<-import(file.path(input.folder,India))
India_yield$Area<-"India"
Europe_yield<-import(file.path(input.folder,Europe))
Europe_yield$Area<-"Europe"
America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
DF.line<-rbind(China_yield,India_yield,Europe_yield,America_yield)
DF.line %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(data.frame(`country of experiment`='China',import(file.path(input.folder,China),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='India',import(file.path(input.folder,India),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='Europe',import(file.path(input.folder,Europe),sheet=2)),
               data.frame(`country of experiment`='North America',import(file.path(input.folder,America),sheet=2)))

DF.plot$country.of.experiment<-factor(DF.plot$country.of.experiment, levels=c("China", "India","Europe",'North America'), ordered=TRUE)


#AOT40
ggplot()+
  #geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.6),label='(a)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative single grain weight")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  xlim(0,40)+
  ylim(0.4,1.6)+
  #expand_limits(x=c(0,12),y=c(0,1))+
  #scale_x_continuous(breaks=seq(0,50,10))+
  scale_y_continuous(limits = c(0.4,1.6),breaks=seq(0.4,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.32,0.26),
    #legend.position = "none",
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-1000.png"),width = 11, height = 10,units = "cm",dpi=400)


#grain number----------
China<-"Aisa_wheat_result_grain number_China.xlsx"
India<-"Aisa_wheat_result_grain number_India.xlsx"
Europe<-"Europe_wheat_result_grain number.xlsx"
America<-"N_America_withoutKohut_wheat_result_estY_grain number.xlsx"

China_yield<-import(file.path(input.folder,China))
China_yield$Area<-"China"
India_yield<-import(file.path(input.folder,India))
India_yield$Area<-"India"
Europe_yield<-import(file.path(input.folder,Europe))
Europe_yield$Area<-"Europe"
America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
DF.line<-rbind(China_yield,India_yield,Europe_yield,America_yield)
DF.line %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(data.frame(`country of experiment`='China',import(file.path(input.folder,China),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='India',import(file.path(input.folder,India),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='Europe',import(file.path(input.folder,Europe),sheet=2)),
               data.frame(`country of experiment`='North America',import(file.path(input.folder,America),sheet=2)))

DF.plot$country.of.experiment<-factor(DF.plot$country.of.experiment, levels=c("China", "India","Europe",'North America'), ordered=TRUE)


#AOT40
ggplot()+
  #geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  #scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3,show.legend=T)+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.6),label='(d)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain number per area")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  xlim(0,40)+
  ylim(0.4,1.6)+
  #expand_limits(x=c(0,12),y=c(0,1))+
  #scale_x_continuous(breaks=seq(0,50,10))+
  scale_y_continuous(limits = c(0.4,1.6),breaks=seq(0.4,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    #legend.position=c(0.32,0.26),
    legend.position="none",
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-number.png"),width = 11, height = 10,units = "cm",dpi=400)


#N-America data compare with Kohut---------- 
America<-"N_America_withoutKohut_wheat_result_estY.xlsx"

America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
America_yield %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(import(file.path(input.folder,America),sheet=2))

DF.Kohut<-import(file.path(input.folder,"N_America_wheat_result_estY.xlsx"),sheet=2)
DF.Kohut %>% filter(Author=="Kohut")->DF.Kohut

#AOT40
ggplot(DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY)))+
  geom_point(aes(colour=Author),size=3)+
  geom_point(aes(colour=Author),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated),size=1,lty='solid',color="#4dbbd5")+
  geom_line(data=DF.line,aes(AOT40 ,RY_upper),size=1,lty='dashed',color="#4dbbd5")+
  geom_line(data=DF.line,aes(AOT40 ,RY_lower),size=1,lty='dashed',color="#4dbbd5")+
  geom_point(data=DF.Kohut,aes(colour=Author),show.legend=FALSE,size=3)+
  geom_line(data=DF.Kohut,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),shape=Author,colour=Author),show.legend=FALSE,size=1)+
  geom_text(aes(x = 50, y =1.6),label='(a)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative yield")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  expand_limits(x=c(0,12),y=c(0,1))+
  scale_x_continuous(breaks=seq(0,50,10))+
  #scale_y_continuous(limits = c(0.4,1.6),breaks=seq(0.4,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#4dbbd5","#e64b35", "#00a087","#3c5488"))+
  #scale_colour_manual(values=c("#82be48", "#fcb26a", "#981962"),labels = c(expression(Asia), expression(Europe), expression(America)))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression(America)))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.25,0.25),
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black", face = "bold"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-America.png"),width = 11, height = 10,units = "cm",dpi=400)

#M12
ggplot(DF.plot,aes(x=as.numeric(M12),y=as.numeric(RY)))+
  geom_point(aes(colour=Author),size=3)+
  geom_point(aes(colour=Author),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(M12 ,RY_estimated),size=1,lty='solid',color="#4dbbd5")+
  geom_line(data=DF.line,aes(M12 ,RY_upper),size=1,lty='dashed',color="#4dbbd5")+
  geom_line(data=DF.line,aes(M12 ,RY_lower),size=1,lty='dashed',color="#4dbbd5")+
  geom_point(data=DF.Kohut,aes(colour=Author),show.legend=FALSE,size=3)+
  geom_line(data=DF.Kohut,aes(x=as.numeric(M12),y=as.numeric(RY),colour=Author),show.legend=FALSE,size=1)+
  geom_text(aes(x = 90, y =1.6),label='(b)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative yield")))+
  labs(x=expression(atop("M12 (ppb)",paste(''))))+
  expand_limits(x=c(20,90),y=c(0,1))+
  scale_x_continuous(breaks=seq(20,90,10))+
  scale_y_continuous(breaks=seq(0,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  #scale_colour_manual(values=c("#82be48", "#fcb26a", "#981962"),labels = c(expression(Asia), expression(Europe), expression(America)))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression(America)))+
  scale_colour_manual(values=c("#4dbbd5","#e64b35", "#00a087","#3c5488"))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.25,0.25),
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black", face = "bold"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"M12-America.png"),width = 11, height = 10,units = "cm",dpi=400)


#grain number per ear----------
China<-"Aisa_wheat_result_grain number per ear_China.xlsx"
India<-"Aisa_wheat_result_grain number per ear_India.xlsx"
Europe<-"Europe_wheat_result_grain number per ear.xlsx"
America<-"N_America_withoutKohut_wheat_result_estY_grain number per ear.xlsx"

China_yield<-import(file.path(input.folder,China))
China_yield$Area<-"China"
India_yield<-import(file.path(input.folder,India))
India_yield$Area<-"India"
Europe_yield<-import(file.path(input.folder,Europe))
Europe_yield$Area<-"Europe"
America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
DF.line<-rbind(China_yield,India_yield,Europe_yield,America_yield)
DF.line %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(data.frame(`country of experiment`='China',import(file.path(input.folder,China),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='India',import(file.path(input.folder,India),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='Europe',import(file.path(input.folder,Europe),sheet=2)),
               data.frame(`country of experiment`='North America',import(file.path(input.folder,America),sheet=2)))

DF.plot$country.of.experiment<-factor(DF.plot$country.of.experiment, levels=c("China", "India","Europe",'North America'), ordered=TRUE)


#AOT40
ggplot(DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY)))+
  #geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.6),label='(b)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative grain number per ear")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  ylim(0.4,1.6)+
  xlim(0,40)+
  #expand_limits(x=c(0,12),y=c(0,1))+
  #scale_x_continuous(breaks=seq(0,50,10))+
  scale_y_continuous(limits = c(0.4,1.6),breaks=seq(0.4,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    #legend.position=c(0.32,0.26),
    legend.position='none',
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-number per ear.png"),width = 11, height = 10,units = "cm",dpi=400)


#ear number per area----------
China<-"Aisa_wheat_result_ear number per plant_China_fix 40.xlsx"
India<-"Aisa_wheat_result_ear number per plant_India_fix 40.xlsx"
Europe<-"Europe_wheat_result_ear number per plant_fix40.xlsx"
America<-"N_America_withoutKohut_wheat_result_estY_ear number per plant_fix40.xlsx"

China_yield<-import(file.path(input.folder,China))
China_yield$Area<-"China"
India_yield<-import(file.path(input.folder,India))
India_yield$Area<-"India"
Europe_yield<-import(file.path(input.folder,Europe))
Europe_yield$Area<-"Europe"
America_yield<-import(file.path(input.folder,America))
America_yield$Area<-"North America"
DF.line<-rbind(China_yield,India_yield,Europe_yield,America_yield)
DF.line %>% filter(AOT40<=50)->DF.line
DF.plot<-rbind(data.frame(`country of experiment`='China',import(file.path(input.folder,China),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='India',import(file.path(input.folder,India),sheet=2) %>% select(-`country of experiment`)),
               data.frame(`country of experiment`='Europe',import(file.path(input.folder,Europe),sheet=2)),
               data.frame(`country of experiment`='North America',import(file.path(input.folder,America),sheet=2)))

DF.plot$country.of.experiment<-factor(DF.plot$country.of.experiment, levels=c("China", "India","Europe",'North America'), ordered=TRUE)


#AOT40
ggplot()+
  #geom_ribbon(data=DF.line,aes(x=AOT40,ymin = RY_upper, ymax = RY_lower,fill=Area),alpha=0.5,linetype = 0,show.legend=FALSE)+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),size=3)+
  geom_point(data=DF.plot,aes(x=as.numeric(AOT40_Adjusted),y=as.numeric(RY),colour=country.of.experiment),show.legend=FALSE,size=3)+
  geom_line(data=DF.line,aes(AOT40 ,RY_estimated,colour=Area),size=1,lty='solid')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_upper,colour=Area),size=1,lty='dashed')+
  #geom_line(data=DF.line,aes(AOT40 ,RY_lower,colour=Area),size=1,lty='dashed')+
  geom_text(aes(x = 40, y =1.6),label='(c)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative ear number per area")))+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  xlim(0,40)+
  ylim(0.4,1.6)+
  #expand_limits(x=c(0,12),y=c(0,1.6))+
  #scale_x_continuous(breaks=seq(0,50,10))+
  scale_y_continuous(limits = c(0.4,1.6),breaks=seq(0.4,1.6,0.2))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression(China), expression(India),expression(Europe), expression('North America')))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression('North America')))+
  theme_classic()+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    #legend.position=c(0.32,0.26),
    legend.position="none",
    legend.key.size = unit(.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-ear per area.png"),width = 11, height = 10,units = "cm",dpi=400)


#contributions of yield components---------
China_yield<-import(file.path(input.folder,"Aisa_wheat_result_China.xlsx"))
China_yield$variable<-"yield"
China_1000<-import(file.path(input.folder,"Aisa_wheat_result_1000_China.xlsx"))
China_1000$variable<-"1000"
China_grain<-import(file.path(input.folder,"Aisa_wheat_result_grain number per ear_China.xlsx"))
China_grain$variable<-"grain"
China_ear<-import(file.path(input.folder,"Aisa_wheat_result_ear number per plant_China_fix 40.xlsx"))
China_ear$variable<-"ear"
China<-rbind(China_yield,China_1000,China_grain,China_ear)
China$area<-"China"

India_yield<-import(file.path(input.folder,"Aisa_wheat_result_India.xlsx"))
India_yield$variable<-"yield"
India_1000<-import(file.path(input.folder,"Aisa_wheat_result_1000_India.xlsx"))
India_1000$variable<-"1000"
India_grain<-import(file.path(input.folder,"Aisa_wheat_result_grain number per ear_India.xlsx"))
India_grain$variable<-"grain"
India_ear<-import(file.path(input.folder,"Aisa_wheat_result_ear number per plant_India_fix 40.xlsx"))
India_ear$variable<-"ear"
India<-rbind(India_yield,India_1000,India_grain,India_ear)
India$area<-"India"

Europe_yield<-import(file.path(input.folder,"Europe_wheat_result.xlsx"))
Europe_yield$variable<-"yield"
Europe_1000<-import(file.path(input.folder,"Europe_wheat_result_1000.xlsx"))
Europe_1000$variable<-"1000"
Europe_grain<-import(file.path(input.folder,"Europe_wheat_result_grain number per ear.xlsx"))
Europe_grain$variable<-"grain"
Europe_ear<-import(file.path(input.folder,"Europe_wheat_result_ear number per plant_fix40.xlsx"))
Europe_ear$variable<-"ear"
Europe<-rbind(Europe_yield,Europe_1000,Europe_grain,Europe_ear)
Europe$area<-"Europe"

America_yield<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY.xlsx"))
America_yield$variable<-"yield"
America_1000<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY_1000.xlsx"))
America_1000$variable<-"1000"
America_grain<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY_grain number per ear.xlsx"))
America_grain$variable<-"grain"
America_ear<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY_ear number per plant_fix40.xlsx"))
America_ear$variable<-"ear"
America<-rbind(America_yield,America_1000,America_grain,America_ear)
America$area<-"North America"
DF.contri<-rbind(China,India,Europe,America)

DF.contri %>% select(-RY_upper,-RY_lower)%>% pivot_wider(names_from = variable, values_from = RY_estimated)->DF.contri

DF.contri %>% dplyr::mutate(Yield_calculated=`1000`*grain*ear) %>% dplyr::mutate(g1000_contri=log(`1000`)/log(Yield_calculated)*100*(1-yield),grain_contri=log(grain)/log(Yield_calculated)*100*(1-yield),ear_contri=log(ear)/log(Yield_calculated)*100*(1-yield))->DF.contributions

#set value<0  =0
#DF.contributions$ear_contri[which(DF.contributions$ear_contri<0)]<-1
#remove yield<0
DF.contributions %>% filter(AOT40<=40)->DF.contributions
DF.contributions %>% select(M12,AOT40,area,g1000_contri,grain_contri,ear_contri) %>% pivot_longer(cols=4:6)->DF.plot
DF.plot$name<- factor(DF.plot$name, levels=c("ear_contri", "grain_contri","g1000_contri"), ordered=TRUE)
DF.plot$area  <- factor(DF.plot$area  , levels=c("China","India", "Europe","North America"), ordered=TRUE)
DF.plot %>% filter(AOT40==15) %>% select(-name) %>% group_by(M12,AOT40,area) %>% summarise_all(sum)->DF.line

#plot species plot
DF.plot %>% filter(area=='China') %>% select(-name) %>% group_by(M12,AOT40,area) %>% filter(value<0) %>% mutate(ear.value=value) %>% select(-value)->DF.china.ear
DF.plot %>% filter(area=='China') %>% select(-name) %>% group_by(M12,AOT40,area) %>% summarise_all(sum) %>% right_join(DF.china.ear)->DF.china.ear
DF.china.ear %>% mutate(value.low=value-ear.value,name="ear number per area (China)")->DF.china.ear



ggplot(DF.plot %>% filter(value>0),aes(x=AOT40,y = value))+
  facet_wrap(.~area)+
  geom_area(aes(fill=name),size=0.1,alpha=1)+
  geom_ribbon_pattern(data=DF.china.ear,aes(x=AOT40,ymin = value.low, ymax = value,fill=name),
                      colour          = '#cf3e3e', 
                      pattern_density = 0.03,
                      pattern_spacing = 0.02,
                      pattern_fill    = '#00b050',
                      pattern_colour  = '#00b050',
                      pattern_angle = 90,
                      alpha=1)+
  #geom_text(aes(x = 50, y =1.6),label='(A)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative yield (%)")),family="Times")+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  geom_segment(data=DF.line,aes(x=15,y=0,xend=15,yend=value),color = "blue", size=1)+
  geom_segment(data=DF.line,aes(x=0,y=value,xend=15,yend=value),color = "blue", size=1)+
  #xlim(0,40)
  expand_limits(x=c(0,40),y=c(0,100))+
  #scale_x_continuous(breaks=seq(0,40,10))+
  scale_y_continuous(breaks=seq(0,100,20))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_fill_manual(values=c("#cf3e3e","#cf3e3e", "#fe8c4d", "#00b050"),labels = c(expression("ear number per area (China)"),expression("ear number per area"), expression("single grain weight"), expression("grain number per ear")))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression(America)))+
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-contribution_RE.pdf"),width = 20, height = 14,units = "cm",dpi=400)


ggplot(DF.plot,aes(x=M12 ,y = value))+
  facet_wrap(.~area)+
  geom_area(aes(fill=name),size=.01,colour="white",alpha=1)+
  #geom_text(aes(x = 50, y =1.6),label='(A)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative yield (%)")))+
  labs(x=expression(atop("M12 (ppb)",paste(''))))+
  expand_limits(x=c(20,80),y=c(0,100))+
  scale_x_continuous(breaks=seq(20,80,20))+
  scale_y_continuous(breaks=seq(0,100,20))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_fill_manual(values=c(  "#981962","#fcb26a","#82be48"),labels = c(expression("ear number per area"), expression("grain number per ear"), expression("thousand grain weight")))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression(America)))+
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
#ggsave(file.path(figure.folder,"M12-contribution.png"),width = 20, height = 14,units = "cm",dpi=400)

export(DF.contributions,'Contribution.xlsx')


#contributions of grain number---------
China_yield<-import(file.path(input.folder,"Aisa_wheat_result_China.xlsx"))
China_yield$variable<-"yield"
China_1000<-import(file.path(input.folder,"Aisa_wheat_result_1000_China.xlsx"))
China_1000$variable<-"1000"
China_grain<-import(file.path(input.folder,"Aisa_wheat_result_grain number_China.xlsx"))
China_grain$variable<-"grain"
China<-rbind(China_yield,China_1000,China_grain)
China$area<-"China"

India_yield<-import(file.path(input.folder,"Aisa_wheat_result_India.xlsx"))
India_yield$variable<-"yield"
India_1000<-import(file.path(input.folder,"Aisa_wheat_result_1000_India.xlsx"))
India_1000$variable<-"1000"
India_grain<-import(file.path(input.folder,"Aisa_wheat_result_grain number_India.xlsx"))
India_grain$variable<-"grain"
India<-rbind(India_yield,India_1000,India_grain)
India$area<-"India"

Europe_yield<-import(file.path(input.folder,"Europe_wheat_result.xlsx"))
Europe_yield$variable<-"yield"
Europe_1000<-import(file.path(input.folder,"Europe_wheat_result_1000.xlsx"))
Europe_1000$variable<-"1000"
Europe_grain<-import(file.path(input.folder,"Europe_wheat_result_grain number.xlsx"))
Europe_grain$variable<-"grain"

Europe<-rbind(Europe_yield,Europe_1000,Europe_grain)
Europe$area<-"Europe"

America_yield<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY.xlsx"))
America_yield$variable<-"yield"
America_1000<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY_1000.xlsx"))
America_1000$variable<-"1000"
America_grain<-import(file.path(input.folder,"N_America_withoutKohut_wheat_result_estY_grain number.xlsx"))
America_grain$variable<-"grain"

America<-rbind(America_yield,America_1000,America_grain)
America$area<-"North America"
DF.contri<-rbind(China,India,Europe,America)

DF.contri %>% select(-RY_upper,-RY_lower)%>% pivot_wider(names_from = variable, values_from = RY_estimated)->DF.contri
DF.contri %>% mutate(Yield_calculated=`1000`*grain) %>% mutate(g1000_contri=log(`1000`)/log(Yield_calculated)*100*(1-yield),grain_contri=log(grain)/log(Yield_calculated)*100*(1-yield))->DF.contributions

#set value<0  =0
#DF.contributions$ear_contri[which(DF.contributions$ear_contri<0)]<-1
#remove yield<0
DF.contributions %>% filter(AOT40<=40)->DF.contributions
DF.contributions %>% select(M12,AOT40,area,g1000_contri,grain_contri) %>% pivot_longer(cols=4:5)->DF.plot
DF.plot$name<- factor(DF.plot$name, levels=c("grain_contri","g1000_contri"), ordered=TRUE)
DF.plot$area  <- factor(DF.plot$area  , levels=c("China","India", "Europe","North America"), ordered=TRUE)
DF.plot %>% filter(AOT40==15) %>% select(-name) %>% group_by(M12,AOT40,area) %>% summarise_all(sum)->DF.line

ggplot(DF.plot %>% filter(value>0),aes(x=AOT40,y = value))+
  facet_wrap(.~area)+
  geom_area(aes(fill=name),size=0.1,alpha=1)+
  #geom_ribbon_pattern(data=DF.china.ear,aes(x=AOT40,ymin = value.low, ymax = value,fill=name),
  #                    colour          = '#cf3e3e', 
  #                    pattern_density = 0.03,
  #                    pattern_spacing = 0.02,
  #                    pattern_fill    = '#00b050',
  #                    pattern_colour  = '#00b050',
  #                    pattern_angle = 90,
  #                    alpha=1)
  #geom_text(aes(x = 50, y =1.6),label='(A)',size = 4,color = "black",family="Times")+
  labs(y=expression(atop(paste(''),"Relative yield (%)")),family="Times")+
  labs(x=expression(atop("AOT40 (ppm h)",paste(''))))+
  geom_segment(data=DF.line,aes(x=15,y=0,xend=15,yend=value),color = "blue", size=1)+
  geom_segment(data=DF.line,aes(x=0,y=value,xend=15,yend=value),color = "blue", size=1)+
  #xlim(0,40)
  expand_limits(x=c(0,40),y=c(0,100))+
  #scale_x_continuous(breaks=seq(0,40,10))+
  scale_y_continuous(breaks=seq(0,100,20))+
  #annotate("text",label=eqn_1,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=3)+
  #annotate("text",label=eqn_2,parse=TRUE,x=0,y=0.15,parse=F, hjust=0,size=4)+
  scale_fill_manual(values=c("#00b050","#fe8c4d"),labels = c(expression("grain number per area"),expression("single grain weight")))+
  #scale_shape(labels = c(expression(Asia), expression(Europe), expression(America)))+
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
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"AOT40-contribution_grain number_RE.png"),width = 20, height = 14,units = "cm",dpi=400)


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
library(ggbreak) 
library(ggplot2)
library(cli)
library(ggpattern)
library(ggpmisc)
library(car)
library(PupillometryR)
library(ggpubr)



#remotes::install_github("coolbutuseless/ggpattern")

windowsFonts(Times=windowsFont("Times New Roman"))
#input folder-------
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Input result"
figure.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"

yield.RE<-import(file.path(input.folder,"Global wheat dataset_meta analysis.xlsx"))

yield.RE$Area<-factor(yield.RE$Area, levels=c("China", "India","Europe",'North America'), ordered=TRUE)

ggplot(data=yield.RE,aes(x=-(`ears no. plant-1.RE`)*100,y=-(`Total grain no. ear-1.RE`)*100,colour=Area))+
  geom_point(size=3)+
  stat_cor(aes(color = Area), label.x = 3)+
  labs(y=expression(atop(paste(''),"RL of grain number per ear")))+
  labs(x=expression(atop("RL of ear number per area",paste(''))))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))

#ear no. and grain no ear-----------
ggplot(data=yield.RE,aes(x=-(`ears no. plant-1.RE`)*100,y=-(`Total grain no. ear-1.RE`)*100,colour=Area))+
  geom_point(size=3)+
  geom_point(show.legend=FALSE,size=3)+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_smooth(data=yield.RE,aes(x=-`ears no. plant-1.RE`*100,y=-`Total grain no. ear-1.RE`*100,colour=Area),method = 'lm', se = F, show.legend=FALSE)+
  stat_poly_eq(method = 'lm',aes(label=paste(after_stat(rr.label),after_stat(p.value.label),sep = "*\", \"*")),size=3)+
  geom_hline(yintercept = 0,linetype="dashed",color="grey")+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  labs(y=expression(atop(paste(''),"RL of grain number per ear")))+
  labs(x=expression(atop("RL of ear number per area",paste(''))))+
  geom_text(aes(x = 50, y =50),label='(a)',size = 4,color = "black",family="Times")+
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
ggsave(file.path(figure.folder,"garin.no.ear-ears.no.plant.png"),width = 11, height = 10,units = "cm",dpi=400)


ggplot(data=yield.RE,aes(x=-`1000 grain weight.RE`*100,y=-`Total grain no.RE`*100,colour=Area))+
  geom_point(size=3)+
  stat_cor(aes(color = Area), label.x = 3)+
  labs(y=expression(atop(paste(''),"RL of grain number per area")))+
  labs(x=expression(atop("RL of single grain weight",paste(''))))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))

#grain number and 1000 grain weight-----------
ggplot(data=yield.RE,aes(x=-`1000 grain weight.RE`*100,y=-`Total grain no.RE`*100,colour=Area))+
  geom_point(size=3)+
  geom_point(show.legend=FALSE,size=3)+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  geom_smooth(method = 'lm', se = F, show.legend=FALSE)+
  stat_poly_eq(method = 'lm',aes(label=paste(after_stat(rr.label),after_stat(p.value.label),sep = "*\", \"*")),size=3)+
  geom_hline(yintercept = 0,linetype="dashed",color="grey")+
  geom_vline(xintercept = 0,linetype="dashed",color="grey")+
  labs(y=expression(atop(paste(''),"RL of grain number per area")))+
  labs(x=expression(atop("RL of single grain weight",paste(''))))+
  geom_text(aes(x = 50, y =50),label='(b)',size = 4,color = "black",family="Times")+
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
ggsave(file.path(figure.folder,"garin.no-1000.png"),width = 11, height = 10,units = "cm",dpi=400)



#plot relative losses of grain number per area and grain yield-------
REL_Y2 <- function(REL_Z,REL_Y1) {
  (REL_Z-REL_Y1)/(1-REL_Y1)
}
Y1<-seq(-0.3,0.9,0.001)
Y2<-REL_Y2(0,Y1)
plot.data.0<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.1,Y1)
plot.data.1<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.2,Y1)
plot.data.2<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.3,Y1)
plot.data.3<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.4,Y1)
plot.data.4<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.5,Y1)
plot.data.5<-data.frame(Y1,Y2)
plot.data.6<-data.frame(Y1,Y1)

Point<-c("A","B","C","D","E")
Y1<-c(0.4,0.2254,0.2,0.1,0)
Y2<-c(0.1,0.2254,0,0.2,0.4)
plot.data.7<-data.frame(Point,Y1,Y2)

ggplot()+
  geom_line(data=plot.data.0,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.07, y =  -0.07526882),label='0 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.1,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.12, y =  -0.02272727),label='10 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.2,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.18 , y =  2.439024e-02),label='20 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.3,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.24, y = 7.894737e-02),label='30 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.4,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.30, y = 0.14285714),label='40 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.5,aes(x=Y1,y=Y2),size=0.5,color="grey")+
  geom_text(aes(x = 0.37, y = 0.20634921),label='50 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_text(aes(x = 0.37, y = 0.20634921),label=expression(RL['y1 x y2']),size = 4,color = "black",family="Times",angle =-45,vjust = -1.5)+
  
  geom_line(data=plot.data.6,aes(x=Y1,y=Y1),size=0.5,linetype="dashed",color="black")+
  geom_text(aes(x = 0.4, y = 0.4),label=expression(RL[y1]*"="*RL[y2]),size = 4,color = "black",family="Times",angle =45,vjust = -0.2)+
  geom_point(data=plot.data.7,aes(x=Y1,y=Y2,color=Point),size=3)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(labels = scales::percent,limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,0.1))+
  scale_x_continuous(labels = scales::percent,limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,0.1))+
  labs(y=expression(RL[Y2]))+
  labs(x=expression(RL[Y1]))+
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
    #legend.position=c(0.32,0.26),
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
ggsave(file.path(figure.folder,"relationship-figure.png"),width = 13, height = 10,units = "cm",dpi=400)
  
#grain number per area--------
REL_Y2 <- function(REL_Z,REL_Y1) {
  (REL_Z-REL_Y1)/(1-REL_Y1)
}
Y1<-seq(-0.5,0.9,0.001)
Y2<-REL_Y2(0,Y1)
plot.data.0<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.1,Y1)
plot.data.1<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.2,Y1)
plot.data.2<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.3,Y1)
plot.data.3<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.4,Y1)
plot.data.4<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.5,Y1)
plot.data.5<-data.frame(Y1,Y2)
plot.data.6<-data.frame(Y1,Y1)

Point<-c("China","India","Europe","North America")
Y1<-c(0.2360,0.0071,0.1019,0.0547)
Y2<-c(-0.0881,0.2151,0.0223,0.0036)
Y2up<-c(0.12,-0.249,0.059,0.01)
Y2down<-c(0.301,0.119,0.163,0.093)
Y1up<-c(-0.341,0.138,-0.013,-0.052)
Y1down<-c(0.055,0.404,0.055,0.061)
plot.data.7<-data.frame(Point,Y1,Y2,Y2up,Y2down,Y1up,Y1down)

plot.data.7$Point<-factor(plot.data.7$Point, levels=c("China", "India","Europe",'North America'), ordered=TRUE)

#circle
yield.RE %>% select(Area,`ears no. plant-1.RE`,`Total grain no. ear-1.RE`) %>% filter(Area=="China")->yield.RE.China
yield.RE.China<-na.omit(yield.RE.China)
DF.China<-dataEllipse(-yield.RE.China$`ears no. plant-1.RE`,-yield.RE.China$`Total grain no. ear-1.RE`,levels=c(0.95))

yield.RE %>% select(Area,`ears no. plant-1.RE`,`Total grain no. ear-1.RE`) %>% filter(Area=="India")->yield.RE.India
yield.RE.India<-na.omit(yield.RE.India)
DF.India<-dataEllipse(-yield.RE.India$`ears no. plant-1.RE`,-yield.RE.India$`Total grain no. ear-1.RE`,levels=c(0.95))

yield.RE %>% select(Area,`ears no. plant-1.RE`,`Total grain no. ear-1.RE`) %>% filter(Area=="Europe")->yield.RE.Europe
yield.RE.Europe<-na.omit(yield.RE.Europe)
DF.Europe<-dataEllipse(-yield.RE.Europe$`ears no. plant-1.RE`,-yield.RE.Europe$`Total grain no. ear-1.RE`,levels=c(0.95))

yield.RE %>% select(Area,`ears no. plant-1.RE`,`Total grain no. ear-1.RE`) %>% filter(Area=="North America")->yield.RE.America
yield.RE.America<-na.omit(yield.RE.America)
DF.America<-dataEllipse(-yield.RE.America$`ears no. plant-1.RE`,-yield.RE.America$`Total grain no. ear-1.RE`,levels=c(0.95))


ggplot()+
  geom_point(data=yield.RE,aes(x=-(`ears no. plant-1.RE`),y=-(`Total grain no. ear-1.RE`),colour=Area),size=3)+
  geom_path(data=data.frame(DF.China),aes(x,y),color="#cf3e3e")+
  geom_polygon(data=data.frame(DF.China),aes(x,y),fill="#cf3e3e",alpha=0.4)+
  
  geom_path(data=data.frame(DF.India),aes(x,y),color="#fe8c4d")+
  geom_polygon(data=data.frame(DF.India),aes(x,y),fill="#fe8c4d",alpha=0.4)+
  
  geom_path(data=data.frame(DF.Europe),aes(x,y),color="#00b050")+
  geom_polygon(data=data.frame(DF.Europe),aes(x,y),fill="#00b050",alpha=0.4)+
  
  geom_path(data=data.frame(DF.America),aes(x,y),color="#3a61dd")+
  geom_polygon(data=data.frame(DF.America),aes(x,y),fill="#3a61dd",alpha=0.4)+

  geom_text(aes(x = 0.45, y =0.5),label='(a)',size = 4,color = "black",family="Times")+
  geom_hline(yintercept = 0,color = "#969595")+
  geom_vline(xintercept = 0,color = "#969595")+ 
  labs(y=expression("RL of grain number per ear"))+
  labs(x=expression("RL of ear number per area"))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  theme_classic()+
  scale_y_continuous(labels = scales::percent,limits=c(-0.4,0.5),breaks=seq(-0.4,0.5,0.1))+
  scale_x_continuous(labels = scales::percent,limits=c(-0.6,0.5),breaks=seq(-0.6,0.5,0.1))+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1),
    legend.position=c(0.26,0.26),
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
ggsave(file.path(figure.folder,"Grain number per area_point.png"),width = 14, height = 14,units = "cm",dpi=400)



ggplot()+
  #geom_point(data=yield.RE,aes(x=-(`ears no. plant-1.RE`),y=-(`Total grain no. ear-1.RE`),colour=Area),size=3)+
  geom_path(data=data.frame(DF.China),aes(x,y),color="#cf3e3e")+
  geom_polygon(data=data.frame(DF.China),aes(x,y),fill="#cf3e3e",alpha=0.4)+
  
  geom_path(data=data.frame(DF.India),aes(x,y),color="#fe8c4d")+
  geom_polygon(data=data.frame(DF.India),aes(x,y),fill="#fe8c4d",alpha=0.4)+
  
  geom_path(data=data.frame(DF.Europe),aes(x,y),color="#00b050")+
  geom_polygon(data=data.frame(DF.Europe),aes(x,y),fill="#00b050",alpha=0.4)+
  
  geom_path(data=data.frame(DF.America),aes(x,y),color="#3a61dd")+
  geom_polygon(data=data.frame(DF.America),aes(x,y),fill="#3a61dd",alpha=0.4)+
  
  #stat_ellipse(data=yield.RE,aes(x=(-`ears no. plant-1.RE`),y=(-`Total grain no. ear-1.RE`),fill=Area),size=1,level = 0.8,type = "norm",geom = "polygon",alpha=0.4,na.rm=T)+
  #stat_ellipse(data=yield.RE,aes(x=(-`ears no. plant-1.RE`),y=(-`Total grain no. ear-1.RE`),color=Area),size=0.8,type = "norm",level = 0.8,na.rm=T)+
  geom_line(data=plot.data.0,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.15, y =  -0.1764706),label='0 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.1,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.21, y =   -0.1392405),label='10 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.2,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.27 , y =  -0.09589041),label='20 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.3,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.33, y = -0.04477612),label='30 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.4,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.39, y = 0.01639344),label='40 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  #geom_line(data=plot.data.5,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  #geom_text(aes(x = 0.5, y = 0.09090909),label='50 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_text(aes(x = 0.17, y = 0.17),label=expression(atop("RL of grain", "number per area")),size = 4,color = "#969595",family="Times",angle =-45,vjust = -1)+
  
  geom_line(data=plot.data.6,aes(x=Y1,y=Y1),size=0.5,linetype="dashed",color="#969595")+
  geom_text(aes(x = 0.38, y = 0.38),label=expression(x*"="*y),size = 4,color = "#969595",family="Times",angle =45,vjust = -0.2)+
  geom_point(data=plot.data.7,aes(x=Y2,y=Y1,color=Point),size=3)+
  geom_errorbar(data=plot.data.7,aes(x=Y2,ymin=Y2up, ymax=Y2down,color=Point),width=.0,size=1)+
  geom_errorbar(data=plot.data.7,aes(y=Y1,xmin=Y1up, xmax=Y1down,color=Point),width=.0,size=1)+
  geom_text(aes(x = 0.45, y =0.5),label='(c)',size = 4,color = "black",family="Times")+
  geom_hline(yintercept = 0,color = "#969595")+
  geom_vline(xintercept = 0,color = "#969595")+ 
  labs(y=expression("RL of grain number per ear"))+
  labs(x=expression("RL of ear number per area"))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  theme_classic()+
  scale_y_continuous(labels = scales::percent,limits=c(-0.4,0.5),breaks=seq(-0.4,0.5,0.1))+
  scale_x_continuous(labels = scales::percent,limits=c(-0.4,0.5),breaks=seq(-0.4,0.5,0.1))+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #图例
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    #legend.justification=c(1,1),
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
ggsave(file.path(figure.folder,"Grain number per area.png"),width = 14, height = 14,units = "cm",dpi=400)

#grain yield--------
REL_Y2 <- function(REL_Z,REL_Y1) {
  (REL_Z-REL_Y1)/(1-REL_Y1)
}
Y1<-seq(-0.3,0.9,0.001)
Y2<-REL_Y2(0,Y1)
plot.data.0<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.1,Y1)
plot.data.1<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.2,Y1)
plot.data.2<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.3,Y1)
plot.data.3<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.4,Y1)
plot.data.4<-data.frame(Y1,Y2)
Y2<-REL_Y2(0.5,Y1)
plot.data.5<-data.frame(Y1,Y2)
plot.data.6<-data.frame(Y1,Y1)

Point<-c("China","India","Europe","North America")
Y1<-c(0.3076,0.1483,0.2162,0.1649)
Y2<-c(0.1425,0.2267,0.1348,0.0650)
Y2up<-c(0.031,0.138,0.08,0.037)
Y2down<-c(0.219,0.323,0.177,0.088)
Y1up<-c(0.225,0.093,0.155,0.115)
Y1down<-c(0.397,0.215,0.274,0.264)

plot.data.7<-data.frame(Point,Y1,Y2)

plot.data.7$Point<-factor(plot.data.7$Point, levels=c("China", "India","Europe",'North America'), ordered=TRUE)

#circle
yield.RE %>% select(Area,`1000 grain weight.RE`,`Total grain no.RE`) %>% filter(Area=="China")->yield.RE.China
yield.RE.China<-na.omit(yield.RE.China)
DF.China<-dataEllipse(-yield.RE.China$`1000 grain weight.RE`,-yield.RE.China$`Total grain no.RE`,levels=c(0.95))

yield.RE %>% select(Area,`1000 grain weight.RE`,`Total grain no.RE`) %>% filter(Area=="India")->yield.RE.India
yield.RE.India<-na.omit(yield.RE.India)
DF.India<-dataEllipse(-yield.RE.India$`1000 grain weight.RE`,-yield.RE.India$`Total grain no.RE`,levels=c(0.95))

yield.RE %>% select(Area,`1000 grain weight.RE`,`Total grain no.RE`) %>% filter(Area=="Europe")->yield.RE.Europe
yield.RE.Europe<-na.omit(yield.RE.Europe)
DF.Europe<-dataEllipse(-yield.RE.Europe$`1000 grain weight.RE`,-yield.RE.Europe$`Total grain no.RE`,levels=c(0.95))

yield.RE %>% select(Area,`1000 grain weight.RE`,`Total grain no.RE`) %>% filter(Area=="North America")->yield.RE.America
yield.RE.America<-na.omit(yield.RE.America)
DF.America<-dataEllipse(-yield.RE.America$`1000 grain weight.RE`,-yield.RE.America$`Total grain no.RE`,levels=c(0.95))


ggplot()+
  geom_point(data=yield.RE,aes(x=-(`1000 grain weight.RE`),y=-(`Total grain no.RE`),colour=Area),size=3)+
  geom_path(data=data.frame(DF.China),aes(x,y),color="#cf3e3e")+
  geom_polygon(data=data.frame(DF.China),aes(x,y),fill="#cf3e3e",alpha=0.4)+
  
  geom_path(data=data.frame(DF.India),aes(x,y),color="#fe8c4d")+
  geom_polygon(data=data.frame(DF.India),aes(x,y),fill="#fe8c4d",alpha=0.4)+
  
  geom_path(data=data.frame(DF.Europe),aes(x,y),color="#00b050")+
  geom_polygon(data=data.frame(DF.Europe),aes(x,y),fill="#00b050",alpha=0.4)+
  
  geom_path(data=data.frame(DF.America),aes(x,y),color="#3a61dd")+
  geom_polygon(data=data.frame(DF.America),aes(x,y),fill="#3a61dd",alpha=0.4)+
  
  geom_hline(yintercept = 0,color="#969595")+
  geom_vline(xintercept = 0,color="#969595")+  
  scale_y_continuous(labels = scales::percent,limits=c(-0.3,0.5),breaks=seq(-0.3,0.5,0.1))+
  scale_x_continuous(labels = scales::percent,limits=c(-0.3,0.5),breaks=seq(-0.3,0.5,0.1))+
  geom_text(aes(x = 0.45, y =0.5),label='(b)',size = 4,color = "black",family="Times")+
  
  
  labs(y=expression("RL of grain number per area"))+
  labs(x=expression("RL of single grain number"))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
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
ggsave(file.path(figure.folder,"Grain yield_point.png"),width = 14, height = 14,units = "cm",dpi=400)




ggplot()+
  #geom_point(data=yield.RE,aes(x=-(`1000 grain weight.RE`),y=-(`Total grain no.RE`),colour=Area),size=3)+
  geom_path(data=data.frame(DF.China),aes(x,y),color="#cf3e3e")+
  geom_polygon(data=data.frame(DF.China),aes(x,y),fill="#cf3e3e",alpha=0.4)+
  
  geom_path(data=data.frame(DF.India),aes(x,y),color="#fe8c4d")+
  geom_polygon(data=data.frame(DF.India),aes(x,y),fill="#fe8c4d",alpha=0.4)+
  
  geom_path(data=data.frame(DF.Europe),aes(x,y),color="#00b050")+
  geom_polygon(data=data.frame(DF.Europe),aes(x,y),fill="#00b050",alpha=0.4)+
  
  geom_path(data=data.frame(DF.America),aes(x,y),color="#3a61dd")+
  geom_polygon(data=data.frame(DF.America),aes(x,y),fill="#3a61dd",alpha=0.4)+
  
  
  
  #stat_ellipse(data=yield.RE,aes(x=(-`1000 grain weight.RE`),y=(-`Total grain no.RE`),fill=Area),size=1,level = 0.8,geom = "polygon",alpha=0.4,type = "norm")+
  #stat_ellipse(data=yield.RE,aes(x=(-`1000 grain weight.RE`),y=(-`Total grain no.RE`),color=Area),size=0.8,level = 0.8,type = "norm")+
  geom_line(data=plot.data.0,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = -0.2, y =  0.1666667),label='0 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.1,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = -0.14, y =  0.2105263),label='10 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.2,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = -0.08 , y = 0.2592593),label='20 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.3,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = -0.02, y = 0.3137255),label='30 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  geom_line(data=plot.data.4,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  geom_text(aes(x = 0.04, y = 0.375),label='40 %',size = 4,color = "#969595",family="Times",angle =-45,vjust = -0.2)+
  #geom_line(data=plot.data.5,aes(x=Y1,y=Y2),size=0.5,color="#969595")+
  #geom_text(aes(x = 0.37, y = 0.20634921),label='50 %',size = 4,color = "black",family="Times",angle =-45,vjust = -0.2)+
  geom_text(aes(x = 0.3, y = 0.3),label=expression("RL of grain yield"),size = 4,color = "#969595",family="Times",angle =-45,vjust = -2)+
  
  geom_line(data=plot.data.6,aes(x=Y1,y=Y1),size=0.5,linetype="dashed",color="#969595")+
  geom_text(aes(x = 0.4, y = 0.4),label=expression(x*"="*y),size = 4,color = "#969595",family="Times",angle =45,vjust = -0.25)+
  geom_point(data=plot.data.7,aes(x=Y1,y=Y2,color=Point),size=3)+
  geom_errorbar(data=plot.data.7,aes(x=Y1,ymin=Y2up, ymax=Y2down,color=Point),width=.0,size=1)+
  geom_errorbar(data=plot.data.7,aes(y=Y2,xmin=Y1up, xmax=Y1down,color=Point),width=.0,size=1)+
  
  
  geom_hline(yintercept = 0,color="#969595")+
  geom_vline(xintercept = 0,color="#969595")+  
  scale_y_continuous(labels = scales::percent,limits=c(-0.3,0.5),breaks=seq(-0.3,0.5,0.1))+
  scale_x_continuous(labels = scales::percent,limits=c(-0.3,0.5),breaks=seq(-0.3,0.5,0.1))+
  geom_text(aes(x = 0.45, y =0.5),label='(d)',size = 4,color = "black",family="Times")+
  
  
  labs(y=expression("RL of grain number per area"))+
  labs(x=expression("RL of single grain number"))+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
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
ggsave(file.path(figure.folder,"Grain yield.png"),width = 14, height = 14,units = "cm",dpi=400)

#plot release year-----------------
yield.release<-import(file.path(input.folder,"Datasets of the Global Wheat data-release year.xlsx"))
yield.release$Area<-factor(yield.release$Area, levels=c("China", "India","Europe",'North America'), ordered=TRUE)

ggplot(yield.release,aes(x=Area,y=`CV release year`))+
  geom_hline(yintercept = 1990,color = "#969595")+
  geom_flat_violin(aes(fill=Area),position = position_nudge(x = .2),width=1)+
  geom_jitter(aes(color = Area),width=0.1, alpha = 0.6)+
  geom_boxplot(position = position_nudge(x = .1),size=0.1,width=0.15)+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_y_continuous(limits=c(1960,2020),breaks=seq(1960,2020,10))+
  geom_text(aes(x = 4.5, y =2020),label='(b)',size = 4,color = "black",family="Times")+
  labs(y=expression("Year"))+
  labs(x=expression(""))+
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
    #axis.title.y = element_blank(),
    axis.title.x=element_text(colour='black', size=12,face = "bold",vjust = 1.5),
    #轴刻度
    axis.text.y=element_text(colour='black',size=9),
    axis.text.x=element_text(colour = "black",size = 9,angle = 0,hjust = 0.5,vjust = 0),
    #axis.text.y = element_blank(),
    #分面标题
    strip.text = element_text(colour = "black",size = 9,face = "bold"),
    strip.background = element_rect(fill=NA,size=0.5,color="black"))
ggsave(file.path(figure.folder,"release year_b.png"),width = 10, height = 14,units = "cm",dpi=400)


ggplot(yield.release,aes(x=Area,y=exp.Year))+
  geom_hline(yintercept = 1990,color = "#969595")+
  geom_flat_violin(aes(fill=Area),position = position_nudge(x = .2),width=1)+
  geom_jitter(aes(color = Area),width=0.1, alpha = 0.6)+
  geom_boxplot(position = position_nudge(x = .1),size=0.1,width=0.15)+
  scale_color_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_fill_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"))+
  scale_y_continuous(limits=c(1960,2020),breaks=seq(1960,2020,10))+
  geom_text(aes(x = 4.5, y =2020),label='(a)',size = 4,color = "black",family="Times")+
  labs(y=expression("Year"))+
  labs(x=expression(""))+
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
ggsave(file.path(figure.folder,"release year_a.png"),width = 10, height = 14,units = "cm",dpi=400)




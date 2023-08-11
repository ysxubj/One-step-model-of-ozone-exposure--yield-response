#May 2022, Yansen Xu (yansenxu@nuist.edu.cn)
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
library(plyr)
library(nls2)
library(minpack.lm)
windowsFonts(Times=windowsFont("Times New Roman"))
#input folder
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/O3浓度和日期数据/Plot O3/Days AOT40"
output.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/O3浓度和日期数据/Plot O3/Days AOT40"


#test<-DF.split[[1]]
###define the function------------
AOT<-function(test){
  test %>% filter(Hour<17&Hour>8) %>% select(Note,Date,index,Ozone) %>% group_by(Note,Date,index) %>% summarise_all(mean)->M8
  names(M8)<-c( "Note",  "Date",  "index", "M8")
  test %>% filter(Hour<20&Hour>7) %>% select(Note,Date,index,Ozone) %>% group_by(Note,Date,index) %>% summarise_all(mean)->M12
  names(M12)<-c( "Note",  "Date",  "index", "M12")
  test$AOT40<-test$Ozone-40
  test$AOT40[which(test$AOT40<0)]<-0
  test %>% filter(Hour<20&Hour>7) %>% select(Note,Date,index,AOT40) %>% group_by(Note,Date,index) %>% summarise_all(sum)->AOT
  M8 %>% left_join(AOT)%>% left_join(M12)->output
  return(output)
}

#main prog--------
DF<-import(file.path(input.folder,"AOT40 and M7 relationship.xlsx"))
DF$Date<-ymd(DF$Date)
DF$Hour<-hms(DF$Hour)
DF$Hour<-hour(DF$Hour)
DF %>% group_by(Note,Date,Hour) %>%summarise_all(mean,na.rm=T)->DF
DF$index<-paste0(DF$Note,DF$Date)
DF.split<-split(DF,DF$index)
DF.M8.AOT<-lapply(DF.split, AOT)
Result <- ldply(DF.M8.AOT,data.frame)

plot(Result$AOT40, Result$M8)
plot(Result$AOT40, Result$M12)
plot(Result$M8, Result$M12)

fit<-lm(formula = M12 ~M8 +0,data=Result)
summary(fit)
#y=0.906918*M8


#define function of AOT40 and AOTx using P1 and P2-----
AOTX.fun<-function(AOT40,x,P1,P2){
  AOT40+(40-x)*(12/1000)-1/(1/((40-P1)*(12/1000)*(1-(x/40)^2))+P2*AOT40)
}
Result$AOT0<-Result$M12*12/1000
Result$AOT40<-Result$AOT40/1000


#fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=Result,start=list(P1=15,P2=-0.003),trace=TRUE) 
#coef(fit)
#P1<-coef(fit)[[1]]
#P2<-coef(fit)[[2]]

P1<- 27.31163
P2<-206.03696

ggplot(Result,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)


#estimate AOT0
AOT40.fun<-function(x,AOT0)  x+(40-0)*(12/1000)-1/(1/((40-P1)*(12/1000)*(1-(0/40)^2))+P2*x)-AOT0
root.value<-function(x){
  result<-uniroot(AOT40.fun, c(0,1000),AOT0=x)
  return(result$root)
}

#root.value(1) #测试任何值
#root.value(0.33)



#calculated AOT40 with different area---------
windowsFonts(Times=windowsFont("Times New Roman"))
#input folder-------
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/O3浓度和日期数据/Plot O3"
figure.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"

#ozone data---------
China_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=5)
China_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='China')->China_ozone

India_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=6)
India_ozone  %>% select(-Ref.50,-Ref.26)%>% pivot_longer(cols = 3:7) %>% mutate(Area='India')->India_ozone

Europe_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=7)
Europe_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='Europe')->Europe_ozone

America_ozone<-import(file.path(input.folder,"O3 concentration_global.xlsx"),sheet=8)
America_ozone  %>% pivot_longer(cols = 3:5) %>% mutate(Area='North America')->America_ozone

DF.ozone<-rbind(China_ozone,India_ozone,Europe_ozone,America_ozone)


#China-------
DF.ozone %>% filter(Area=="China") %>% select(-name)%>% group_by(Area,Date,DOY) %>% summarise_all(mean,na.rm=T)->DF.ozone.china
plot(DF.ozone.china$DOY,DF.ozone.china$value)
DF.ozone.china$AOT0<-DF.ozone.china$value*0.906918*12/1000
DF.ozone.china$AOT40<-NA
for (i in 1:length(DF.ozone.china$AOT0)) {
  if (DF.ozone.china$AOT0[i]>=0.33) {
    AOT40<-root.value(DF.ozone.china$AOT0[i])
  }else{
  AOT40<-0  
  }
  DF.ozone.china$AOT40[i]<-AOT40
}
plot(DF.ozone.china$DOY,DF.ozone.china$AOT0)
plot(DF.ozone.china$DOY,DF.ozone.china$AOT40)


#O3 fumigation
DF.ozone.china %>% filter(Date<=ymd("2023-5-23")&Date>=ymd("2023-3-19")) %>% mutate(value_EO3=1.5*value)->DF.ozone.china.EO3.1
DF.ozone.china %>% filter(Date>ymd("2023-5-23")|Date<ymd("2023-3-19")) %>% mutate(value_EO3=1*value)->DF.ozone.china.EO3.2
rbind(DF.ozone.china.EO3.1,DF.ozone.china.EO3.2)->DF.ozone.china.EO3
DF.ozone.china.EO3$AOT0_EO3<-DF.ozone.china.EO3$value_EO3*0.906918*12/1000
DF.ozone.china.EO3$AOT40_EO3<-NA
for (i in 1:length(DF.ozone.china.EO3$value_EO3)) {
  if (DF.ozone.china.EO3$AOT0_EO3[i]>=0.33) {
    AOT40<-root.value(DF.ozone.china.EO3$AOT0_EO3[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.china.EO3$AOT40_EO3[i]<-AOT40
}


DF.ozone.china.EO3 %>% filter(Area=="China") %>% pivot_longer(cols=c(4,7),values_to = "plot_name")->plot.china
plot.china%>% filter(name=="value_EO3") %>% filter(Date>=as.POSIXct("2023-3-19")&Date<=as.POSIXct("2023-5-23")) ->plot.CN
rbind(plot.china %>% filter(name=="value"),plot.CN)->plot.CN

dodge <- position_dodge(width=0.9)
#China plot-------
ggplot(plot.china ,aes(x=Date,y=plot_name))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-3-19"), xmax=as.POSIXct("2023-5-23"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = T)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F,show.legend = F)+
  scale_color_manual(values=c("#70c9dd", "#eb6f5d"),labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
  #scale_color_npg(labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
  scale_shape(labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
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
ggsave(file.path(figure.folder,"China_O3_AOT40.png"),width = 20, height = 14,units = "cm",dpi=400)


#India-----
DF.ozone %>% filter(Area=="India") %>% select(-name)%>% group_by(Area,Date,DOY) %>% summarise_all(mean,na.rm=T)->DF.ozone.india
plot(DF.ozone.india$DOY,DF.ozone.india$value)
DF.ozone.india$AOT0<-DF.ozone.india$value*0.906918*12/1000
DF.ozone.india$AOT40<-NA
for (i in 1:length(DF.ozone.india$AOT0)) {
  if (DF.ozone.india$AOT0[i]>=0.33) {
    AOT40<-root.value(DF.ozone.india$AOT0[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.india$AOT40[i]<-AOT40
}
plot(DF.ozone.india$DOY,DF.ozone.india$AOT0)
plot(DF.ozone.india$DOY,DF.ozone.india$AOT40)


#O3 fumigation
DF.ozone.india %>% filter(Date<=ymd("2023-4-11")&Date>=ymd("2022-12-10")) %>% mutate(value_EO3=1.5*value)->DF.ozone.india.EO3.1
DF.ozone.india %>% filter(Date>ymd("2023-4-11")|Date<ymd("2022-12-10")) %>% mutate(value_EO3=1*value)->DF.ozone.india.EO3.2
rbind(DF.ozone.india.EO3.1,DF.ozone.india.EO3.2)->DF.ozone.india.EO3
DF.ozone.india.EO3$AOT0_EO3<-DF.ozone.india.EO3$value_EO3*0.906918*12/1000
DF.ozone.india.EO3$AOT40_EO3<-NA
for (i in 1:length(DF.ozone.india.EO3$value_EO3)) {
  if (DF.ozone.india.EO3$AOT0_EO3[i]>=0.33) {
    AOT40<-root.value(DF.ozone.india.EO3$AOT0_EO3[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.india.EO3$AOT40_EO3[i]<-AOT40
}


dodge <- position_dodge(width=0.9)
#India plot-------
ggplot(DF.ozone.india.EO3 %>% filter(Area=="India") %>% pivot_longer(cols=c(4,7),values_to = "plot_name") ,aes(x=Date,y=plot_name))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2022-12-10"), xmax=as.POSIXct("2023-4-11"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = T)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_manual(values=c("#70c9dd", "#eb6f5d"),labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
  #scale_color_npg(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
  scale_shape(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
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
ggsave(file.path(figure.folder,"India_O3_AOT40.png"),width = 20, height = 14,units = "cm",dpi=400)



#Europe------
DF.ozone %>% filter(Area=="Europe") %>% select(-name)%>% group_by(Area,Date,DOY) %>% summarise_all(mean,na.rm=T)->DF.ozone.Europe
plot(DF.ozone.Europe$DOY,DF.ozone.Europe$value)
DF.ozone.Europe$AOT0<-DF.ozone.Europe$value*0.906918*12/1000
DF.ozone.Europe$AOT40<-NA
for (i in 1:length(DF.ozone.Europe$AOT0)) {
  if (DF.ozone.Europe$AOT0[i]>=0.33) {
    AOT40<-root.value(DF.ozone.Europe$AOT0[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.Europe$AOT40[i]<-AOT40
}
plot(DF.ozone.Europe$DOY,DF.ozone.Europe$AOT0)
plot(DF.ozone.Europe$DOY,DF.ozone.Europe$AOT40)


#O3 fumigation
DF.ozone.Europe %>% filter(Date<=ymd("2023-8-18")&Date>=ymd("2023-5-28")) %>% mutate(value_EO3=1.5*value)->DF.ozone.Europe.EO3.1
DF.ozone.Europe %>% filter(Date>ymd("2023-8-18")|Date<ymd("2023-5-28")) %>% mutate(value_EO3=1*value)->DF.ozone.Europe.EO3.2
rbind(DF.ozone.Europe.EO3.1,DF.ozone.Europe.EO3.2)->DF.ozone.Europe.EO3
DF.ozone.Europe.EO3$AOT0_EO3<-DF.ozone.Europe.EO3$value_EO3*0.906918*12/1000
DF.ozone.Europe.EO3$AOT40_EO3<-NA
for (i in 1:length(DF.ozone.Europe.EO3$value_EO3)) {
  if (DF.ozone.Europe.EO3$AOT0_EO3[i]>=0.33) {
    AOT40<-root.value(DF.ozone.Europe.EO3$AOT0_EO3[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.Europe.EO3$AOT40_EO3[i]<-AOT40
}


dodge <- position_dodge(width=0.9)
#Europe plot-------
ggplot(DF.ozone.Europe.EO3 %>% filter(Area=="Europe") %>%  pivot_longer(cols=c(4,7),values_to = "plot_name") ,aes(x=Date,y=plot_name))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-5-28"), xmax=as.POSIXct("2023-8-18"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = T)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  scale_color_manual(values=c("#70c9dd", "#eb6f5d"),labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
  #scale_color_npg(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
  scale_shape(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
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
ggsave(file.path(figure.folder,"Europe_O3_AOT40.png"),width = 20, height = 14,units = "cm",dpi=400)


#America----
DF.ozone %>% filter(Area=="North America") %>% select(-name)%>% group_by(Area,Date,DOY) %>% summarise_all(mean,na.rm=T)->DF.ozone.America
plot(DF.ozone.America$DOY,DF.ozone.America$value)
DF.ozone.America$AOT0<-DF.ozone.America$value*0.906918*12/1000
DF.ozone.America$AOT40<-NA
for (i in 1:length(DF.ozone.America$AOT0)) {
  if (DF.ozone.America$AOT0[i]>=0.33) {
    AOT40<-root.value(DF.ozone.America$AOT0[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.America$AOT40[i]<-AOT40
}
plot(DF.ozone.America$DOY,DF.ozone.America$AOT0)
plot(DF.ozone.America$DOY,DF.ozone.America$AOT40)


#O3 fumigation
DF.ozone.America %>% filter(Date<=ymd("2023-6-18")&Date>=ymd("2023-4-24")) %>% mutate(value_EO3=1.5*value)->DF.ozone.America.EO3.1
DF.ozone.America %>% filter(Date>ymd("2023-6-18")|Date<ymd("2023-4-24")) %>% mutate(value_EO3=1*value)->DF.ozone.America.EO3.2
rbind(DF.ozone.America.EO3.1,DF.ozone.America.EO3.2)->DF.ozone.America.EO3
DF.ozone.America.EO3$AOT0_EO3<-DF.ozone.America.EO3$value_EO3*0.906918*12/1000
DF.ozone.America.EO3$AOT40_EO3<-NA
for (i in 1:length(DF.ozone.America.EO3$value_EO3)) {
  if (DF.ozone.America.EO3$AOT0_EO3[i]>=0.33) {
    AOT40<-root.value(DF.ozone.America.EO3$AOT0_EO3[i])
  }else{
    AOT40<-0  
  }
  DF.ozone.America.EO3$AOT40_EO3[i]<-AOT40
}


dodge <- position_dodge(width=0.9)
#America plot-------
ggplot(DF.ozone.America.EO3 %>% filter(Area=="North America") %>%  pivot_longer(cols=c(4,7),values_to = "plot_name") ,aes(x=Date,y=plot_name))+
  facet_wrap(.~Area)+
  geom_rect(aes(xmin=as.POSIXct("2023-4-24"), xmax=as.POSIXct("2023-6-18"), ymin=-Inf, ymax=Inf),fill='#E6E6E6',alpha = .5)+
  geom_point(aes(colour=name),size=3,alpha=0.8,show.legend = T)+
  geom_smooth(aes(shape=name),color='grey',size=1,span = 20,se = F)+
  
  
  scale_color_manual(values=c("#70c9dd", "#eb6f5d"),labels = c( expression(''*AA[ ]),expression(''*E*"-"*O[3])))+
  
  
  #scale_color_npg(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
  scale_shape(labels = c(expression(''*AA[ ]), expression(''*E*"-"*O[3])))+
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
ggsave(file.path(figure.folder,"America_O3_AOT40.png"),width = 20, height = 14,units = "cm",dpi=400)

DF.ozone.china.EO3 %>%  filter(Date<=ymd("2023-6-2")&Date>=(ymd("2022-11-9")))%>% mutate(day=DOY-min(DOY))->DF.ozone.china.EO3
DF.ozone.india.EO3 %>%  filter(Date<=ymd("2023-4-13")&Date>=(ymd("2022-12-1")))%>% mutate(day=DOY-min(DOY))->DF.ozone.india.EO3
DF.ozone.America.EO3 %>%  filter(Date<=ymd("2023-6-24")&Date>=(ymd("2022-10-22")))%>% mutate(day=DOY-min(DOY))->DF.ozone.America.EO3
DF.ozone.Europe.EO3 %>%  filter(Date<=ymd("2023-8-21")&Date>=(ymd("2023-4-19")))%>% mutate(day=DOY-min(DOY))->DF.ozone.Europe.EO3



DF.ozone.china.EO3 %>%  filter(Date<=ymd("2023-5-23")&Date>=(ymd("2023-5-23")-89))%>% mutate(day=DOY-min(DOY))->DF.ozone.china.EO3
DF.ozone.india.EO3 %>%  filter(Date<=ymd("2023-4-11")&Date>=(ymd("2023-4-11")-89))%>% mutate(day=DOY-min(DOY))->DF.ozone.india.EO3
DF.ozone.America.EO3 %>%  filter(Date<=ymd("2023-6-18")&Date>=(ymd("2023-6-18")-89))%>% mutate(day=DOY-min(DOY))->DF.ozone.America.EO3
DF.ozone.Europe.EO3 %>%  filter(Date<=ymd("2023-8-18")&Date>=(ymd("2023-8-18")-89))%>% mutate(day=DOY-min(DOY))->DF.ozone.Europe.EO3


DF.ozone<-rbind(DF.ozone.china.EO3,DF.ozone.india.EO3,DF.ozone.America.EO3,DF.ozone.Europe.EO3)


DF<-list(DF.ozone.china.EO3,DF.ozone.india.EO3,DF.ozone.America.EO3,DF.ozone.Europe.EO3,DF.ozone)

export(DF,file.path(output.folder,'AOT40 for different days.xlsx'))


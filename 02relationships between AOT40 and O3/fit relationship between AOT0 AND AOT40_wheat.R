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
library(minpack.lm)
library(nls2)
windowsFonts(Times=windowsFont("Times New Roman"))

#input folder
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/06数据计算分析/Inputdata"
output.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/06数据计算分析ata"

#define function of AOT40 and AOTx using P1 and P2-----
AOTX.fun<-function(AOT40,x,P1,P2){
  AOT40+(40-x)*(1080/1000)-1/(1/((40-P1)*(1080/1000)*(1-(x/40)^2))+P2*AOT40)
}

#input data
TOAR<-import(file.path(input.folder,'TOAR_sfc_ozone_wheat_growing_season_global_2008-2015_aggregated_v1_1.xlsx'))

#TOAR data-----
TOAR %>% select(aot40,daytime_avg) %>% filter(aot40>0) %>% filter(daytime_avg>0) %>% mutate(AOT40=aot40/1000,AOT0=daytime_avg*1.08)->TOAR.filter
fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=TOAR.filter,start=list(P1=21,P2=0.02527),trace=TRUE) 
coef(fit)
ggplot(TOAR.filter,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))
#ggsave(file.path(output.folder,"TOAR.png"),width = 10,height = 8,units = 'cm',dpi = 400)

#point to polygons---------
library("sf")
library(spData)
library(spDataLarge)
#devtools::install_github("geocompr/geocompkg")
#install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")
#install.packages('spDataLarge',repos='https://nowosad.github.io/drat/', type='source')
CN.geo.crs<-"+proj=longlat +ellps=krass +no_defs"
data(world)
world<-st_transform(world, CN.geo.crs)
globe<-st_union(world)
TOAR  %>% filter(aot40>0) %>% filter(daytime_avg>0) %>% filter(station_lon<200)%>%  mutate(AOT40=aot40/1000,AOT0=daytime_avg*1.08)->TOAR.point


ggplot() +
  geom_sf(data = world, fill = "white", size=0.5, color="grey")+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA), 
        plot.margin = margin(0.2, 0.3, 0, 0, "cm"))+
  geom_point(data = TOAR.point, aes(x = station_lon, y = station_lat, color = station_country), size = 2,show.legend = F)+
  ggtitle("Wheat data point")  
#ggsave(file.path(output.folder,'Wheat.png'),width = 15, height = 7,units = "in",dpi=400)

# convert the points to an sf object
points_sf <- st_as_sf(TOAR.point, coords = c("station_lon", "station_lat"))
# set CRS for the points to be the same as shapefile
st_crs(points_sf) <- st_crs(world)
world$iso_a2<-world$region_un
#extreme
points_sf_joined <-st_join(points_sf, world) %>%filter(!is.na(iso_a2)) 
#plot
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = points_sf_joined,aes(color=iso_a2)) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA), 
        plot.margin = margin(0.2, 0.3, 0, 0, "cm"))
ggsave(file.path(output.folder,'Wheat.png'),width = 15, height = 7,units = "in",dpi=400)

export(points_sf_joined,file.path(input.folder,"TOAR.wheat.differentarea.csv"))

#EU-------
points_sf_joined<-import(file.path(input.folder,"TOAR.wheat.differentarea.csv"))

points_sf_joined %>% filter(iso_a2=="Europe")->TOAR.EU

#TOAR data
fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=TOAR.EU,start=list(P1=19.949474396,P2=0.028324),trace=TRUE) 
coef(fit)
ggplot(TOAR.EU,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))
#ggsave(file.path(output.folder,"TOAR.EU.png"),width = 10,height = 8,units = 'cm',dpi = 400)


#Asia-------
points_sf_joined %>% filter(iso_a2=="Asia")->TOAR.Asia

#TOAR data
fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=TOAR.Asia,start=list(P1=21,P2=0.02527),trace=TRUE) 
coef(fit)
ggplot(TOAR.Asia,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))
#ggsave(file.path(output.folder,"TOAR.EU.png"),width = 10,height = 8,units = 'cm',dpi = 400)

#Americas-------
points_sf_joined %>% filter(iso_a2=="Americas")->TOAR.Americas

#TOAR data
fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=TOAR.Americas,start=list(P1=21,P2=0.02527),trace=TRUE) 
coef(fit)
ggplot(TOAR.Americas,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))
#ggsave(file.path(output.folder,"TOAR.EU.png"),width = 10,height = 8,units = 'cm',dpi = 400)

ggplot(TOAR.Americas,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =30, label=paste0('TOAR: P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))
ggsave(file.path(output.folder,"TOAR and wheat.Americas.png"),width = 10,height = 8,units = 'cm',dpi = 400)




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
output.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/06数据计算分析/Outputdata"

#define function of AOT40 and AOTx using P1 and P2-----
AOTX.fun<-function(AOT40,x,P1,P2){
  AOT40+(40-x)*(1080/1000)-1/(1/((40-P1)*(1080/1000)*(1-(x/40)^2))+P2*AOT40)
}

#input data
China.AOT.0.9<-import(file.path(input.folder,'China_RYL_0.9.xlsx'))
China.AOT<-import(file.path(input.folder,'China_RYL.xlsx'))
JNP.AOT.0.9<-import(file.path(input.folder,'JNP_RYL_0.9.xlsx'))
JNP.AOT<-import(file.path(input.folder,'JNP_RYL.xlsx'))
KOR.AOT.0.9<-import(file.path(input.folder,'KOR_RYL_0.9.xlsx'))
KOR.AOT<-import(file.path(input.folder,'KOR_RYL.xlsx'))
TOAR<-import(file.path(input.folder,'TOAR_sfc_ozone_wheat_growing_season_global_2008-2015_aggregated_v1_1.xlsx'))

#TOAR data-----
TOAR %>% select(aot40,daytime_avg) %>% filter(aot40>0) %>% filter(daytime_avg>0) %>% mutate(AOT40=aot40/1000,AOT0=daytime_avg*1.08)->TOAR.filter
fit<-nlsLM(AOT0~AOTX.fun(AOT40,0,P1,P2),data=TOAR.filter,start=list(P1=21,P2=0.02527),trace=TRUE) 
coef(fit)
ggplot(TOAR.filter,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))


#wheat data---------
Wheat.DF<-import(file.path('D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量','Global-crop-dataset.xlsx'),sheet=2)
Wheat.DF %>% filter(!is.na(AOT40)&!is.na(`O3 con`)&!is.na(`Est AOT0`))->wheat.DF.filter
wheat.DF.filter$AOT0<-wheat.DF.filter$`Est AOT0`
fit.wheat<-nlsLM(AOT0~AOTX.fun(AOT40_Adjusted,0,P1,P2),data=wheat.DF.filter,start=list(P1=21,P2=0.02527),trace=TRUE) 
coef(fit.wheat)
ggplot(TOAR.filter,aes(AOT40,AOT0))+
  geom_point(color='black',size=0.5,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit)[[1]],coef(fit)[[2]]),color='red',size=1)+
  geom_text(x = 10, y =76, label=paste0('TOAR: P1=',round(coef(fit)[[1]],3),'  P2=',round(coef(fit)[[2]],3)))+
  geom_point(data = wheat.DF.filter,aes(x=AOT40_Adjusted,y=`Est AOT0`),color='Blue',size=1,show.legend=FALSE)+
  geom_function(fun = function(x) AOTX.fun(x,0,coef(fit.wheat)[[1]],coef(fit.wheat)[[2]]),color='Blue',size=1)+
  geom_text(x = 20, y =25, label=paste0('wheat DB: P1=',round(coef(fit.wheat)[[1]],3),'  P2=',round(coef(fit.wheat)[[2]],3)))


P1=21.28
P2=0.01023

#estimated
AOT40.fun<-function(x,AOT0)  x+(40-0)*(1080/1000)-1/(1/((40-P1)*(1080/1000)*(1-(0/40)^2))+P2*x)-AOT0
root.value<-function(x){
  result<-uniroot(AOT40.fun, c(0,1000),AOT0=x)
  return(result$root)
}

root.value(90.12463762) #测试任何值
root.value(74.128932)

#est
Wheat.DF %>% filter(!is.na(`Est AOT0`)&is.na(AOT40))->wheat.DF.filter
result<-data.frame()
for (i in wheat.DF.filter$`Est AOT0`) {
  if (i>21.1) {
    value<-root.value(i)
  }else{
    value<-0
  }
  result<-rbind(result,value)
}
wheat.DF.filter$AOT40_Adjusted_value<-result[,1]
Wheat.DF %>% left_join(wheat.DF.filter)->wheat.DF

data.frame(wheat.DF[,1:21],wheat.DF$AOT40_Adjusted_value)->wheat.DF
export(wheat.DF,file.path('D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量ataset-change.xlsx'),overwrite=T)



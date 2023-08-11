rm(list=ls())
#load necessary libraries----
library(rio)
library(tidyverse)
library(minpack.lm)
library(fastDummies)
library(nls2)
library(tidymodels)

#set path of folder----
main.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量"
inputfolder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Input result"
Outputfolder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Output result/wheat"


#define function of AOT40 and AOTx using P1 and P2-----
AOTX.fun<-function(AOT40){
  AOT40+(40-40)*(1080/1000)-1/(1/((40-P1)*(1080/1000)*(1-(40/40)^2))+P2*AOT40)
}

#define function of AOT40 and AOTx using P1 and P2-----
AOTX.fun.0<-function(AOT40,z){
  AOT40+(40-(19.999*(sin(z)+1)))*(1080/1000)-1/(1/((40-P1)*(1080/1000)*(1-((19.999*(sin(z)+1))/40)^2))+P2*AOT40)
}


#defind RY function------------
RY_fun<-function(S,AOT40){
  (1-S*AOTX.fun(AOT40))/(1-S*AOTX.fun(0))
}

# residual function-----
resids <- function(pars,Totaldata) {  
  resid<-(log(Totaldata$value) - Y.fun(pars,Totaldata))
  return(resid)
}

#Totaldata<-DF.long.split[["yield"]]#for test
Y.fun<-function(pars,Totaldata){
  number.ID<-length(unique(Totaldata$EXP_ID))
  Totaldata.class <- dummy_cols(Totaldata, select_columns = 'EXP_ID')
  #par input
  S<-pars[1]
  Y0.ID<-as.matrix((data.frame(pars[2:(number.ID+1)])))
  #obser input
  AOT40<-Totaldata$AOT40_Adjusted 
  ID<-t(as.matrix(Totaldata.class[(length(Totaldata)+1):length(Totaldata.class)]))
  Y0<-crossprod(Y0.ID,ID)
  Y.model<-Y0+log(1-S*AOTX.fun(AOT40))
  return(Y.model)
}

#Y0 estimated function----
RY.fun<-function(pars,Totaldata){
  number.ID<-length(unique(Totaldata$EXP_ID))
  Totaldata.class <- dummy_cols(Totaldata, select_columns = 'EXP_ID')
  #par input
  S<-pars[1]
  Y0.ID<-as.matrix((data.frame(pars[2:(number.ID+1)])))
  #obser input
  AOT40<-Totaldata$AOT40_Adjusted 
  ID<-t(as.matrix((Totaldata.class[(length(Totaldata)+1):length(Totaldata.class)])))
  Y0<-t(exp(crossprod(Y0.ID,ID)))
  Totaldata$Y0<-Y0
  Totaldata$RY<-Totaldata$value/(Y0*(1-S*AOTX.fun(0)))
  return(Totaldata)
}

#model RY-------
Model_output_fun<-function(S,Totaldata){
  #S=0.001
  Totaldata %>% select(EXP_ID,value)%>% group_by(EXP_ID) %>% summarise_all(c(max))->Y.max
  Y.max<-Y.max$value
  pars<-c(S,log(Y.max))
  Y.fun(pars,Totaldata)
  resids(pars,Totaldata)
  #fit
  nls.out <- nls.lm(par=pars, 
                    fn = resids, Totaldata = Totaldata,
                    control=nls.lm.control(ftol = sqrt(.Machine$double.eps),ptol = sqrt(.Machine$double.eps)))
  #model result
  pars.est<-nls.out$par
  Model<-data.frame(Y.fun(pars.est,Totaldata))
  sum((Model-log(Totaldata$value))^2)
  #RY estimated------
  Totaldata.plot<-RY.fun(pars.est,Totaldata)
  #Totaldata.plot$RY.model<-1-S*AOTX.fun(Totaldata$AOT40_Adjusted,z)
  Totaldata.plot$RY.model<-RY_fun(pars.est[1],Totaldata$AOT40_Adjusted)
  
  SSE=sum((Model-log(Totaldata$value))^2)
  output<-list(Totaldata.plot,pars.est,data.frame(SSE))
  return(output)
}



#main prog---------
DF<-import(file.path(inputfolder,"Global wheat dataset.xlsx"),sheet=1)
#filter data
DF %>%filter(Accept!=0) %>% select("country of experiment","EXP_ID","PDFnumber","Author","Area","species","cultivar/genotype","exp.Year","experimental facility","rooting","O3Trt","AOT40","O3 con","O3 moniter hour","AOT40 Days","N", "AOT40_Adjusted","yield","1000 grain weight","Total grain no. ear-1","Total grain no.","ears no. plant-1")->DF.filter


#filter
DF.filter %>% pivot_longer(cols = 18:22)->DF.class.long

DF.class.long %>% filter(!is.na(value))->DF.class.long.NA

split(DF.class.long.NA,DF.class.long.NA$name)->DF.long.split


#Total grain no.----------
names(DF.long.split)
var.name<-"ears no. plant-1"
Totaldata<-DF.long.split[[var.name]]

#Aisa---------
P1<-21.28
P2<-0.01023

Totaldata %>% filter(`country of experiment` =="India")->Totaldata_area

DF.plot<-Model_output_fun(0.011,Totaldata_area)

S<-DF.plot[[2]][1]
pars<-DF.plot[[2]]
#plot
ggplot(DF.plot[[1]], aes(AOT40_Adjusted, RY)) +
  geom_point()+
  geom_line(aes(AOT40_Adjusted,RY.model))+
  geom_text(aes(x=5,y=0.4),label=paste0("S=",round(S,4)))+
  geom_text(aes(x=5,y=0.5),label=paste0("y=",round(40,4)))
#ggsave(file.path(main.folder,paste0('05estimated Dose relationship/Output result/wheat','/wheat-Asian',var.name,'.png')),width = 12, height = 8,units = "cm",dpi=400)

modelresult<-DF.plot[[1]]
AOT40<-c(0,0.25,0.5,1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50,75)
AOT0<-AOTX.fun.0(AOT40,-pi/2)
M12<-AOT0*1000/1080
RY<-RY_fun(S,c(0,0.25,0.5,1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50,75))

#boot-strap resampling estimated CI
set.seed(27)
boots <- bootstraps(Totaldata_area, times = 25000, apparent = T)
par.start<-pars#set start value

#define function
fit_nls_on_bootstrap <- function(split) {
  fit<-nls.lm(par=par.start, 
              fn = resids, Totaldata = analysis(split),
              control=nls.lm.control(ftol = sqrt(.Machine$double.eps),ptol = sqrt(.Machine$double.eps)))
  fit_result<-data.frame(term=c("S"),estimate=c(fit$par[1]))
  RY_estimated<-RY_fun(fit$par[1],AOT40)
  RY_estimated<-c(fit$par[1],
                  RY_estimated)
  return(data.frame(term=c('s','AOT0','AOT0.25','AOT0.5','AOT1','AOT2','AOT3','AOT4','AOT5','AOT6','AOT7','AOT8','AOT9','AOT10','AOT15','AOT20','AOT25','AOT30','AOT35','AOT40','AOT45','AOT50','AOT75'),estimate=RY_estimated))
}

#estimated paramters
boot_models <-
  boots %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap))

boot_coefs <- 
  boot_models %>% 
  unnest(model)

#95%CI
percentile_intervals <- int_pctl(boot_models,model)
percentile_intervals

#plot CI
ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")
#ggsave(file.path(main.folder,paste0('05estimated Dose relationship/Output result/wheat','/wheat-Asian',var.name,'.png')),width = 12, height = 8,units = "cm",dpi=400)


#estimated Value
AOT40<-as.numeric(substr(percentile_intervals[1:22,]$term,4,10))
RY_estimated<-RY_fun(pars[1],AOT40)
RY_upper<-percentile_intervals[1:22,]$.upper
RY_lower<-percentile_intervals[1:22,]$.lower


result<-data.frame(AOT40,RY_estimated,RY_upper,RY_lower)
M12<-data.frame(M12,AOT40=c(0,0.25,0.5,1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50,75))
M12 %>% left_join(result)->result


#绘图输出-----
modelresult %>% mutate(M12=AOTX.fun.0(AOT40_Adjusted,-pi/2)*1000/1080)->modelresult
ggplot()+
  geom_point(data=modelresult,aes(x=AOT40_Adjusted,y=RY,color=Author))+
  geom_line(data=result,aes(x=AOT40,y=RY_estimated))+
  geom_line(data=result,aes(x=AOT40,y=RY_upper),color='red')+
  geom_line(data=result,aes(x=AOT40,y=RY_lower),color='blue')+
  geom_text(aes(x=5,y=0.1),label=paste0("S=",round(S,4)))+
  geom_text(aes(x=5,y=0),label=paste0("y=",round(40,4)))
#ggsave(file.path(Outputfolder,paste0('Figure','/Aisa_grain number_AOT40.png')),width = 12, height = 8,units = "cm",dpi=400)


ggplot()+
  geom_point(data=modelresult,aes(x=M12,y=RY,color=Author))+
  geom_line(data=result,aes(x=M12 ,y=RY_estimated))+
  geom_line(data=result,aes(x=M12 ,y=RY_upper),color='red')+
  geom_line(data=result,aes(x=M12 ,y=RY_lower),color='blue')
#ggsave(file.path(Outputfolder,paste0('Figure','/Aisa_grain number_M12.png')),width = 12, height = 8,units = "cm",dpi=400)


RY_result<-list(result,modelresult,pars,percentile_intervals)
export(RY_result,file.path(Outputfolder,"Aisa_wheat_result_ear number per plant_India_fix 40.xlsx"),overwrite=T)


windowsFonts(Times=windowsFont("Times New Roman"))
#plot function define------
plot_discrete_cbar <- function(
  breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
  palette = "Greys", # RColorBrewer palette to use
  colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
  direction = 1, # Flip colors? Can be 1 or -1
  spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
  border_color = NA, # NA = no border color
  legend_title = NULL,
  legend_direction = "horizontal", # Can be "horizontal" or "vertical"
  font_size = 5,
  expand_size = 1, # Controls spacing around legend plot
  spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
  width = 0.1, # Thickness of color bar
  triangle_size = 0.1, # Relative width of +-Inf triangles
  myalpha = 1, 
  trick_color ="grey"
) {
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
  
  if (direction == -1) colors = rev(colors)
  
  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors
  
  n_breaks = length(breaks)
  
  labels = breaks
  
  if (spacing == "constant") {
    breaks = 1:n_breaks
  }
  
  r_breaks = range(breaks)
  
  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]
  
  xmin = 1 - width/2
  xmax = 1 + width/2
  
  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color, alpha=myalpha)
  
  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }
  
  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    legend_position = xmax + 0.1 * spacing_scaling
  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }
  
  cbar_plot = cbar_plot +
    geom_segment(data=data.frame(y = breaks, yend = breaks),
                 aes(y=y, yend=yend),
                 x = x - 0.05 * mul * spacing_scaling, xend = xend,
                 inherit.aes = FALSE, colour=trick_color) +
    annotate(geom = 'text', x = x - 0.1 * mul * spacing_scaling, y = breaks,
             label = labels,
             size = font_size) +
    scale_x_continuous(expand = c(expand_size,expand_size)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()
  
  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = mean(r_breaks),
               label = legend_title,
               angle = angle,
               size = font_size)
  }
  
  return (cbar_plot)
}

#break function define----
breakLabel<- function (vct, mybreaks) {
  
  temp<-mybreaks[order(mybreaks)]
  if (identical(temp, mybreaks)==FALSE) {stop ("breaks are not in right order")}
  
  lab<-as.character(mybreaks)
  lab<-lab[-1]
  
  rsl<-cut(include.lowest = T,vct,breaks=mybreaks, 
           labels=lab
  )
  return (rsl)
}

#plot experimental site----
if (TRUE) {
  
  library("ggplot2")
  library("sf")
  library(cowplot)
  library(gridExtra)
  library(grid)
  library(dplyr)
  library(ggpubr)
  library(png)
  library(ggsci)
  library(RColorBrewer)
  library(spData)
  library(dplyr)
  library(raster)
  library(rio)
  library(rgdal)


#input study site
  inputfolder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Input result"
  DF<-import(file.path(inputfolder,"Global wheat dataset.xlsx"),sheet=1)
  
  #background plot
  CN.geo.crs<-"+proj=longlat +ellps=krass +no_defs"
  
  data(world)
  world<-st_transform(world, CN.geo.crs)
  globe<-st_union(world)
  Asianbase<-
    ggplot() +
    geom_sf(data = world, fill = "white", size=0.2, color="grey")+ 
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA), 
          plot.margin = margin(0.2, 0.3, 0, 0, "cm"))+
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) #裁剪中国地区
  
  #break raster
  mybreaks<-c(-Inf,2, 4, 6, 8, 10, Inf)
  mybreaks.Lab<-as.character(mybreaks)[-1]
  mypalette<-"YlOrRd"
  mycolor.Lab<-brewer.pal(length(mybreaks) - 1, mypalette)
  mycolPal<-data.frame(breaks=mybreaks.Lab, cols=mycolor.Lab)
  mycolPal$cols<-(mycolPal$cols)
  
  #input wheat yield raster
  main.folder <- "D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/05estimated Dose relationship/Output result/wheat"
  ds.path <- file.path (main.folder, "whe_2010_yld.tif")
  ds <- raster(ds.path)
  crs(ds)<-  crs(globe)
  ds.df<-as.data.frame(ds, xy=TRUE)
  ds.df$disLabel<-breakLabel(ds.df$whe_2010_yld, mybreaks)
  mycolPal.p<-subset(mycolPal, mycolPal$breaks %in% unique(ds.df$disLabel))
  
  DF$Area<-factor(DF$Area, levels=c("China", "India","Europe",'North America'), ordered=TRUE)
  
  #绘制底图
  p.wheat.yield<-
    Asianbase+
    geom_raster(data = ds.df, aes(x=x,y=y, fill=disLabel),alpha=0.6, show.legend = FALSE)+
    scale_fill_manual(breaks=as.character(mycolPal.p$breaks),values = as.character(mycolPal.p$cols),na.value=NA)+
    geom_point(data=DF,aes(x=Lon,y=Lat,color=Area),size=2)+
    
    
    scale_colour_manual(values=c("#cf3e3e", "#fe8c4d", "#00b050","#3a61dd"),labels = c(expression('China (n=3)'), expression('India (n=9)'),expression('Europe (n=12)'), expression('North America (n=5)')))+
    xlab("Longitude")+
    ylab("Latitude")+
    #geom_text(x = 75, y =54, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")
    #ggtitle("(b) Hybrid rice")+
    theme(
      text=element_text(family="Times"),
      plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
      plot.subtitle =element_text(hjust = .6),
      #legend.background = element_rect(linetype="solid", size=0, color="black"),
      legend.background = element_blank(),
      legend.title=element_blank(),
      legend.key = element_blank(),
      legend.justification=c(1,1), 
      legend.position=c(0.2,0.32),
      legend.key.size = unit(.7, "cm"),
      legend.key.width = unit(0.5,"cm"),
      legend.spacing.y = unit(-0.2, "cm"),
      legend.text = element_text(hjust = 0,colour="black"),
      #axis.title.y = element_text(hjust = .5,face = "bold",size = 1.5),
      #axis.title.x = element_text(hjust = .5,face = "bold",size = 1.5)
      )
  #p.wheat.yield
  
  #绘制比例尺
  p.cbar<-plot_discrete_cbar(breaks=mybreaks, colors=as.character(mycolPal$cols), border_color="grey", 
                             font_size = 3,trick_color="grey50",
                             myalpha = 0.6, triangle_size = 0.07)
  
  #绘制文字
  #p.text<-
  p.cbar+
    geom_text(x = 0.3, y =-12, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")
  
  
  #图片合并
  pic.wheat<-ggdraw()+draw_plot(p.wheat.yield,x=0,y=0.1,width=0.9,height = 0.9) +
    draw_plot(p.cbar,x=0.07, y=-0.15, width=0.80, height = 0.7)
  
  
  pic.wheat
  out.path <- "D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"

  ggsave(file.path(out.path,'RY_GLB.pdf'),width = 10, height = 7,units = "in",dpi=1000)
  ggsave(file.path(out.path,'RY_GLB.png'),width = 10, height = 7,units = "in",dpi=1000)

}

#plot Europe and north American site-------
#input folder
input.folder<-"D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/06数据计算分析/Inputdata"
#input data
TOAR<-import(file.path(input.folder,'TOAR_sfc_ozone_wheat_growing_season_global_2008-2015_aggregated_v1_1.xlsx'))
TOAR  %>% filter(aot40>0) %>% filter(daytime_avg>0) %>% filter(station_lon<200)%>%  mutate(AOT40=aot40/1000,AOT0=daytime_avg*1.08)->TOAR.point

points_sf <- st_as_sf(TOAR.point, coords = c("station_lon", "station_lat"))
points_sf$station_lat<-TOAR.point$station_lat
points_sf$station_lon<-TOAR.point$station_lon

st_crs(points_sf) <- st_crs(world)
world$iso_a2<-world$region_un
points_sf_joined <-st_join(points_sf, world) %>%filter(!is.na(iso_a2)) 

#North America-----------
CN.geo.crs<-"+proj=longlat +ellps=krass +no_defs"
data(world)
world<-st_transform(world, CN.geo.crs)
globe<-st_union(world)
Asianbase<-
  ggplot() +
  geom_sf(data = world, fill = "white", size=0.2, color="grey")+ 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA), 
        plot.margin = margin(0.2, 0.3, 0, 0, "cm"))+
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) #裁剪中国地区

#break raster
mybreaks<-c(-Inf,5, 10, 15, 20, 25, Inf)
mybreaks.Lab<-as.character(mybreaks)[-1]
mypalette<-"YlOrRd"
mycolor.Lab<-brewer.pal(length(mybreaks) - 1, mypalette)
mycolPal<-data.frame(breaks=mybreaks.Lab, cols=mycolor.Lab)
mycolPal$cols<-(mycolPal$cols)
TOAR.point$disLabel<-breakLabel(TOAR.point$AOT40, mybreaks)
mycolPal.p<-subset(mycolPal, mycolPal$breaks %in% unique(TOAR.point$disLabel))


#绘制底图
P.AOT.point<-
  Asianbase+
  geom_point(data = TOAR.point, aes(x = station_lon, y = station_lat, fill= disLabel), alpha=1,size = 3,show.legend = F,shape=21,color="grey")+
  scale_fill_manual(breaks=as.character(mycolPal.p$breaks),values = as.character(mycolPal.p$cols),na.value=NA)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_sf(xlim = c(-150, -50), ylim = c(10, 70), expand = FALSE)+ #裁剪中国地区
  #geom_text(x = 75, y =54, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")
  #ggtitle("(b) Hybrid rice")+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #legend.background = element_rect(linetype="solid", size=0, color="black"),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.2,0.32),
    legend.key.size = unit(.7, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
    #axis.title.y = element_text(hjust = .5,face = "bold",size = 1.5),
    #axis.title.x = element_text(hjust = .5,face = "bold",size = 1.5)
  )
#P.AOT.point

#绘制比例尺
p.cbar<-plot_discrete_cbar(breaks=mybreaks, colors=as.character(mycolPal$cols), border_color="grey", 
                           font_size = 3,trick_color="grey50",
                           myalpha = 0.8, triangle_size = 0.07)

#绘制文字
p.text<-
  p.cbar+
  geom_text(x = 10, y =-10, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")

#图片合并
pic.North.America<-ggdraw()+draw_plot(P.AOT.point,x=0,y=0.1,width=0.9,height = 0.9) +
  draw_plot(p.text,x=0.07, y=-0.3, width=0.80, height = 0.7)

pic.North.America
out.path <- "D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"
ggsave(file.path(out.path,'North Amercia.pdf'),width = 10, height = 7,units = "in",dpi=1000)
ggsave(file.path(out.path,'North Amercia.png'),width = 10, height = 7,units = "in",dpi=1000)


#Europe-----------
points_sf_joined %>% filter(iso_a2=="Europe")->TOAR.EU

CN.geo.crs<-"+proj=longlat +ellps=krass +no_defs"
data(world)
world<-st_transform(world, CN.geo.crs)
globe<-st_union(world)
Asianbase<-
  ggplot() +
  geom_sf(data = world, fill = "white", size=0.2, color="grey")+ 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA), 
        plot.margin = margin(0.2, 0.3, 0, 0, "cm"))+
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) #裁剪中国地区

#break raster
mybreaks<-c(-Inf,5, 10, 15, 20, 25, Inf)
mybreaks.Lab<-as.character(mybreaks)[-1]
mypalette<-"YlOrRd"
mycolor.Lab<-brewer.pal(length(mybreaks) - 1, mypalette)
mycolPal<-data.frame(breaks=mybreaks.Lab, cols=mycolor.Lab)
mycolPal$cols<-(mycolPal$cols)
TOAR.EU$disLabel<-breakLabel(TOAR.EU$AOT40, mybreaks)
mycolPal.p<-subset(mycolPal, mycolPal$breaks %in% unique(TOAR.EU$disLabel))


#绘制底图
P.AOT.point<-
  Asianbase+
  geom_point(data = TOAR.EU, aes(x = station_lon, y = station_lat, fill= disLabel), alpha=1,size = 3,show.legend = F,shape=21,color="grey")+
  scale_fill_manual(breaks=as.character(mycolPal.p$breaks),values = as.character(mycolPal.p$cols),na.value=NA)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_sf(xlim = c(-25, 50), ylim = c(30, 70), expand = FALSE)+ #裁剪中国地区
  #geom_text(x = 75, y =54, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")
  #ggtitle("(b) Hybrid rice")+
  theme(
    text=element_text(family="Times"),
    plot.title =element_text(hjust = .5,face = "bold",size = 1.5),
    plot.subtitle =element_text(hjust = .6),
    #legend.background = element_rect(linetype="solid", size=0, color="black"),
    legend.background = element_blank(),
    legend.title=element_blank(),
    legend.key = element_blank(),
    legend.justification=c(1,1), 
    legend.position=c(0.2,0.32),
    legend.key.size = unit(.7, "cm"),
    legend.key.width = unit(0.5,"cm"),
    legend.spacing.y = unit(-0.2, "cm"),
    legend.text = element_text(hjust = 0,colour="black"),
    #axis.title.y = element_text(hjust = .5,face = "bold",size = 1.5),
    #axis.title.x = element_text(hjust = .5,face = "bold",size = 1.5)
  )
#p.wheat.yield

#绘制比例尺
p.cbar<-plot_discrete_cbar(breaks=mybreaks, colors=as.character(mycolPal$cols), border_color="grey", 
                           font_size = 3,trick_color="grey50",
                           myalpha = 0.8, triangle_size = 0.07)

#绘制文字
#p.text<-
p.cbar+
  geom_text(x = 0.3, y =-12, aes(label = '(b)'),size = 5,color = "black",fontface = "bold",family="Times")

#图片合并
pic.Europe<-ggdraw()+draw_plot(P.AOT.point,x=0,y=0.1,width=0.9,height = 0.9) +
  draw_plot(p.cbar,x=0.07, y=-0.3, width=0.80, height = 0.7)

pic.Europe
out.path <- "D:/OneDrive - 南京信息工程大学/2022年/00实验数据处理/meta分析O3对作物产量/07plot figures"
ggsave(file.path(out.path,'Europe.pdf'),width = 10, height = 7,units = "in",dpi=1000)
ggsave(file.path(out.path,'Europe.png'),width = 10, height = 7,units = "in",dpi=1000)



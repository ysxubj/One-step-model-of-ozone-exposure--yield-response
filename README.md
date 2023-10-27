# Wheat yield loss to ozone differs among regions
This is a repository for compare wheat yield and yield components response to elevated O3 around the word (China, India, Europe and North America)
The code and data were used in the manuscript "Wheat yield response to elevated O3 concentrations differs between the world's major producing regions".

Three files were included in this repository.
01 estimated Dose relationship:
We develop a method to fit the O3 dose relationship with crop yield and yield components here. Relative yield or yield components were calculated as follows: 
RY_y={1-S ̂  f(AOT40;x ̂)}/{1-S ̂  f(0;x ̂)}
The 95% CI of RY was estimated using bootstrap on 25,000 time. The details of the equations could be find in the manuscript

02relationships between AOT40 and O3
We explore the relationships betwen AOT40 and hourly O3 concentration using Chinese, European and American O3 observation. The values of AOT40 and AOT0 thus obtained can be converted to each other by using a semi-empirical model of 90 days accumulated daytime hourly O3 over a threshold of x (AOTx) in the range from 0 to 40 ppb as follows:
AOTx = f(AOT40; x) = AOT40 + (40 – x) (1080/1000) – 1/{1/[(40-p1) (1080/1000) (1 – (0.025 x)2)] + p2 AOT40}
where p1 and p2 are empirical coefficients that are determined by fitting equation 10 to observed values of AOTx and AOT40. In this study, the values of p1 and p2 for China and India were taken from a previous study, whereas those for Europe and North America were estimated by using the reported values of AOT0 and AOT40 in TOAR (Tropospheric Ozone Assessment Report) database. In the TOAR database, the O3 doses (M7 and AOT40) are calculated for the wheat growing seasons from the year 2008 to 2015 at the monitoring sites across the respective regions. It is noteworthy that equation 10 can be solved for AOT40 analytically in case of conversion from AOTx to AOT40.

03Wheat_meta.analysis_ref
The references used in this manuscript.

This method was developed by Prof. Kazuhiko Kobayashi, Graduate School of Agricultural and Life Sciences, The University of Tokyo, Tokyo, Japan. It was implemented using R by Dr. Yansen Xu, University of Information Science & Technology, Nanjing, China. These studies were supervised by Prof. Zhaozhong Feng, University of Information Science & Technology, Nanjing, China.
Please leave your comments
Any questions please connect YansenXu via: yansenxu@nuist.edu.cn

References:
Feng Zhaozhong#*, Xu Yansen#, Kobayashi Kazuhiko#*, Dai Lulu, Zhang Tianyi, Agathokleous Evgenios, Calatayud Vicent, Paoletti Elena, Mukherjee Arideep, Agrawal Madhoolika, Park Rokjin J., Oak Yujin J.,Yue Xu*. (2022). Ozone pollution threatens the production of major staple crops in East Asia. Nature Food, 3(1), 47-56. https://doi.org/10.1038/s43016-021-00422-6 

Xu Yansen, Kobayashi Kazuhiko,Feng Zhaozhong*. (2023). Wheat yield response to elevated O3 concentrations differs between the world's major producing regions. Science of the Total Environment, 168103. https://doi.org/10.1016/j.scitotenv.2023.168103 

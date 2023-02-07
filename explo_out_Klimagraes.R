library(tidyverse)
library(lme4)
library(nlme)
library(agricolae)
library(ggpubr)
library(gridExtra)

#### output ----
out_tbl_hal <- readRDS("out_tbl_hal.RDS")

#head(out_tbl)

out_tbl_a <-out_tbl_hal %>% separate(
  ID, 
  c('id', 'soiltype','soilCinit','hal', 'croprot', 'inittime', 'initC'),
  sep = "_") %>% rename(
    totalC_topsoil='total(1,1)',
    totalC_subsoil='total(2,1)'
  ) %>% mutate(
    SoilC_tot=totalC_topsoil+totalC_subsoil,
    CO2_tot= Foml1+Foml2+Huml1+Huml2+Roml1+Roml2,
    transport_tot=Fom+Hum+Rom
  )%>% separate(
    croprot, c('croprot_man', 'croprot_field'), sep = "F",remove = FALSE)

##### Long term scatter -----

out_tbl_a %>% ggplot(aes(y=SoilC_tot, x=year))+
  geom_point()+
  scale_x_continuous(breaks=seq(1866,2020,14))+
  scale_y_continuous(breaks =seq(40,180,20))+
  theme_bw()+
  ylab("Total soil C [ Mg/ha m] (C topsoil + C subsoil)")

out_tbl_a %>% ggplot(aes(y=totalC_topsoil, x=year))+
  geom_point()+
  scale_x_continuous(breaks=seq(1866,2020,14))+
  scale_y_continuous(breaks =seq(40,180,20))+
  theme_bw()+
  
  ylab("Topsoil Total C [Mg/ha m]")

##### filter from 1996 to 2020 ----

out_a <- out_tbl_a %>% mutate(year2=strptime(year,"%Y")) %>% 
  filter(year2 > "1996-05-02" & year2 <"2020-06-02")

saveRDS(out_a,"out_a_hal.RDS")

write.table(out_a,"out_a_hal.txt", sep = "\t", dec=".")

#### Soil C -----
out_a %>% ggplot(aes(y=SoilC_tot, x=year))+
  geom_point()+
  scale_x_continuous(breaks=seq(1866,2020,14))+
  scale_y_continuous(breaks =seq(40,180,20))+
  theme_bw()+ylab("Total soil C [ Mg/ha m] (C topsoil + C subsoil)")

#### top vs sub
out_a %>% ggplot(aes(y=totalC_topsoil, x=totalC_subsoil, group= soilCinit))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_cor(method = "pearson")+
  theme_bw()

out_a %>% group_by(croprot_man,soilCinit,initC) %>% filter(
  year2 > "2015-05-02" & year2 <"2020-06-02") %>% summarise(
    mean.a_C_top=mean(totalC_topsoil),
    mean_C_sub=mean(totalC_subsoil)
  ) %>% pivot_longer(
    c(mean_C_sub,mean.a_C_top)) %>% ggplot() + 
  geom_col(aes(x = croprot_man, y=value, fill = name))+
  scale_fill_manual(name="Stock",labels=c("Topsoil", "Subsoil"),
                    values=c("#9C5712","#E49D56" ))+
  labs(y="Total soil C [ Mg/ha m]", x="Crop rotation")+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(initC~soilCinit) +
  theme_bw() 

out_a %>% filter(!hal=='mod.unfert')%>% 
  ggplot(aes(y=totalC_topsoil, x=year, 
             group=interaction(croprot_man, hal),
             col=croprot_man,
             linetype=hal))+
  geom_point(size=0.2, alpha =.3)+
  geom_smooth()+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  scale_x_continuous(breaks=seq(1996,2020,4))+
  scale_y_continuous(breaks =seq(40,180,10))+
  scale_linetype_manual(name="Allometric",
                        values = c("dotted",#"longdash",
                                   "solid"))+
  facet_grid(initC~soilCinit)+
  theme(legend.position="bottom",
        panel.background = NULL,
        text = element_text(size=12))+
  ylab("Topsoil Total C [Mg/ha m]")

cor(out_a$totalC_topsoil, out_a$totalC_subsoil)


##### VCA----
# Total amount of C (Mg ha-1m-1) in topsoil out_tbl$`total(1,1)`

SOC_vca<-lme4::lmer(
  totalC_topsoil~1+year+
    (1|hal)+(1|soilCinit)+(1|soiltype)+(1|croprot_man)+(1|inittime)+(1|initC)
  ,na.action=na.omit
  ,REML=T
  ,control=lmerControl(optimizer="bobyqa")
  ,data=out_a)

summary(SOC_vca)

vca <- as.data.frame(VarCorr(SOC_vca)) 

vca %>% group_by(grp) %>% summarise(
  varprop = vcov / sum(vca$vcov) * 100) %>% arrange(
    varprop, grp)

options(contrasts=c("contr.sum", "contr.poly"))


##### Linear model SOC----

SoilC_reg <- lm(
  totalC_topsoil~+year+soiltype+soilCinit+croprot_man+inittime+initC+hal+
    hal*year+year*initC+year*initC*hal+year*soilCinit+year*croprot_man, #+year*inittime+year*soiltype,
  data = out_a)

anova(SoilC_reg)
summary(SoilC_reg)



LSD.test(SoilC_reg, c("year:croprot_man"), 
         console = TRUE, 
         p.adj="bonferroni", 
         alpha = 0.01)

library(emmeans)#https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

emtrends(SoilC_reg, pairwise ~ initC:hal, adjust="tukey",var = "year")


# SoilC_reg <- lme4::lmer(
#   SoilC_tot~soiltype+croprot_man+inittime+initC+
#     soilCinit*croprot_man+soiltype*soilCinit+year*croprot_man+(1|soilCinit),
#   data = out_a)
# 
# anova(SoilC_reg)
# summary(SoilC_reg)


##### time serie decompose ----

ts_SOC <- out_a %>% filter(croprot_man=="Organic")
#group_by(id)

y <- ts_SOC %>% filter(id==" 37") 

ts_soc <-ts(y$SoilC_tot,  frequency = 5)

plot(ts_soc)

ts_soc_d <- decompose(ts_soc)

plot(ts_soc_d)


##### Difference final and initial situation ----

#obs_val <- out_a %>% filter(id=="  1")

dif <- out_tbl_a %>%  filter(
  year == 1995 | year== 1996 |year == 2020) %>% pivot_wider(
    names_from = year, 
    values_from = c(totalC_topsoil,CO2_tot,transport_tot)) %>% 
  group_by(id) %>% 
  mutate(dif_topC1995 =
           mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1995`,na.rm = T),
         dif_top1996=
           mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1996`,na.rm = T),
         change_SoilC=
           (mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1996`,na.rm = T))/(1996-2020),
         dif_CO2 =
           mean(`CO2_tot_2020`,na.rm = T)-mean(`CO2_tot_1995`,na.rm =T ),
         dif_trans =
           mean(transport_tot_2020,na.rm = T)-mean(transport_tot_1995,na.rm = T),
         Cinputsum=
           sum(`Carbon deposited in the topsoil (t/ha)`)+
           sum(`C deposited in the subsoil (t/ha)`)+
           sum(`C deposited in the topsoil from manure (tC ha-1)`)
  ) %>% filter(!is.na(totalC_topsoil_2020)
  )

#write.table(dif,"dif.txt", sep="\t", dec=".")



dif %>% group_by(soilCinit,croprot_man,initC,hal) %>% 
  summarize(dif_C=mean(dif_topC1995),
            sd=sd(dif_topC1995)) %>% 
  ggplot(aes(y = dif_C, x = hal ,
             colour = soilCinit,
             fill = soilCinit
  )) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
  facet_grid(croprot_man ~ initC) +
  geom_errorbar(aes(ymin=dif_C-sd, ymax=dif_C+sd), 
                position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y="Diference in Topsoil C", x="Allometric")


dif %>% ggplot(aes(y=dif_topC1995, x=initC, 
                   fill=croprot_man)) +
  geom_boxplot()+
  facet_grid(hal ~ soilCinit) +
  scale_fill_manual(name="Crop rotation", 
                    values=c("#15863E","#55E88B", "#EF8D64" ))+
  theme(panel.grid = element_blank())+
  labs(y="Diference in Topsoil C")

DifC_reg <- lm(
  dif_topC1995~soiltype+soilCinit+croprot_man+initC+hal+initC*hal,
  data = dif)

anova(DifC_reg)

LSD.test(DifC_reg, c("hal","initC"), 
         console = TRUE, 
         p.adj="bonferroni", 
         alpha = 0.01)


LSD.test(SoilC_reg,  c("hal","initC"), 
         console = TRUE, 
         p.adj="bonferroni", 
         alpha = 0.01)





difC_vca<-lme4::lmer(
  dif_topC1995~1+(1|hal)+(1|soilCinit)+(1|soiltype)+(1|croprot_man)+(1|inittime)+(1|initC)
  ,na.action=na.omit
  ,REML=T
  ,control=lmerControl(optimizer="bobyqa")
  ,data=dif)

summary(difC_vca)

vca <- as.data.frame(VarCorr(difC_vca)) 

vca %>% group_by(grp) %>% summarise(
  varprop = vcov / sum(vca$vcov)*100) %>% arrange(
    varprop, grp)


# dif %>% ggplot(aes(dif_CO2, fill=croprot)) +geom_histogram()+theme_bw()
# dif %>% ggplot(aes(dif_trans, fill=soiltype)) +geom_histogram()+theme_bw()




# stl(ts_soc, s.window ="periodic", t.window=5)
# 
# ts_SOC %>% filter(id=="  8") %>% select(year,SoilC_tot)%>% ets() 
#   forecast() %>%
#   autoplot()
# 
# 
# ts_SOC %>% group_by(id) %>%  time_decompose(SoilC_tot, method = "stl")%>% autoplot()
# 
# autoplot(decompose(ts_SOC, type = "additive"))+
#   labs(y=expression(Chl[a]~(µg/L)), x="Year") + 
#   ggtitle(expression(Decomposed~Chl[a]~Time~Series~BB1)) +
#   theme(
#     plot.title=element_text(hjust=0.5),
#     text = element_text(family = "Times New Roman", size = 15)
#   )
# 


#LSD.test(SoilC_reg, "croprot_man",console = TRUE)


out_a %>% dplyr::filter(soilCinit=="Mediuminit") %>% ggplot(
  aes(y=SoilC_tot, x=croprot_man, fill= soiltype))+
  geom_boxplot()+theme_bw()


#### CO2 -----

### temperature

temp_yr <- readRDS("temperature/temp_yr25.RDS")

out_a_co <- cbind(out_a,temp_yr[,-1], times=1458)

temp_plot <- out_a_co %>% 
  pivot_longer(c('yr.mean','yr.min','yr.max','yr.amp'), 
               names_to = 't_variable', values_to = 'temp') %>% 
  group_by(t_variable,year) %>% 
  ggplot(aes(y=temp, x=year, col=t_variable))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_grid(vars(t_variable),scales="free_y",)+
  theme(panel.background = NULL,
        legend.position="none")


tr_em_plot <- out_a_co %>% 
  pivot_longer(c('CO2_tot','transport_tot'), 
               names_to = 'out_variable', values_to = 'out') %>% 
  group_by(out_variable,year,croprot_man) %>% 
  ggplot(aes(y=out, x=year, col=croprot_man))+
  geom_point()+
  geom_smooth(se=FALSE)+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  facet_grid(vars(out_variable),scales="free_y",)+
  theme(panel.background = NULL,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.position="none"
  )+
  labs(x=NULL)  

ggarrange(
  tr_em_plot,temp_plot,
  heights = c(2, 1.2),
  ncol=1)



# other_plot <- out_a_co %>% 
#   pivot_longer(c('CO2_tot','transport_tot'), 
#                names_to = 'out_variable', values_to = 'out') %>% 
#   group_by(out_variable,year,croprot_man) %>% 
#   ggplot(aes(y=out, x=year, col=croprot_man))+
#   geom_point()+
#   geom_smooth(se=FALSE)+
#   geom_line(aes(y=(yr.mean/100)+0.2), col="black")+
#   geom_line(aes(y=(yr.amp/100)+0.2), col="black")+
#   scale_y_continuous(#breaks =seq(0,0.5,0.06)#,
#                      sec.axis = sec_axis(~(.*20)-0.2, name="Temp")
#   )+
#   scale_color_manual(name="Crop rotation", 
#                      values=c("#15863E","#55E88B", "#EF8D64" ))+
#   facet_grid(vars(out_variable),scales="free_y",)+
#   theme(panel.background = NULL,
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         #legend.position="none"
#   )#+
#   #labs(x=NULL)  




#out_a %>% mutate(tem_cat= kmeans(temp,25*4))


tm_cl <- kmeans(temp_yr[,-1],10)
out_a_co$tm_cl <- tm_cl$cluster

co2_plot <- out_a_co %>% ggplot(aes(y=CO2_tot, x=year, 
                                    #group=croprot_man, soilCinit,
                                    col=croprot_man))+
  geom_point(size=0.5)+
  geom_smooth(se=FALSE)+
  geom_smooth(aes(y=(yr.mean/35)), 
              col="black",se=FALSE)+
  geom_smooth(aes(y=(yr.amp/35)), 
              col="black", linetype="dashed",se=FALSE)+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  scale_x_continuous(breaks=seq(1996,2020,4))+
  scale_y_continuous(breaks =seq(0,0.5,0.06),
                     sec.axis = sec_axis(~(.*35), name="Temperature (°C)")
  )+
  #facet_grid(initC~soilCinit)+
  theme(panel.background = NULL)+
  ylab(expression(CO[2]*'(Mg/ha m)'))


tr_plot <- out_a_co %>% ggplot(aes(y=transport_tot, x=year, 
                                   #group=croprot_man, soilCinit,
                                   col=croprot_man))+
  geom_point(size=0.5)+
  geom_smooth(se=FALSE)+
  # geom_smooth(aes(y=(yr.mean/35)), 
  #             col="black",se=FALSE)+
  # geom_smooth(aes(y=(yr.amp/35)), 
  #             col="black", linetype="dashed",se=FALSE)+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  scale_x_continuous(breaks=seq(1996,2020,4))+
  # scale_y_continuous(breaks =seq(0,0.5,0.06),
  #                    #sec.axis = sec_axis(~(.*35), name="Temperature (°C)")
  # )+
  #facet_grid(initC~soilCinit)+
  theme(panel.background = NULL)+
  ylab('Topsoil Vertical \n transported C (Mg/ha m)')


ggarrange(
  co2_plot + 
    theme(plot.margin = margin( l = 9), 
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank()),
  tr_plot + 
    theme(plot.margin = margin( r = 29)),
  heights = c(2, 1.2),
  legend = "bottom",
  common.legend = TRUE,
  ncol=1)


CO2_vca<-lme4::lmer(
  transport_tot~1+year+(1|tm_cl)+(1|soilCinit)+(1|soiltype)+
    (1|croprot_man)+(1|inittime)+(1|initC)+(1|hal)
  ,na.action=na.omit
  ,REML=T
  ,control=lmerControl(optimizer="bobyqa")
  ,data=out_a_co)

summary(CO2_vca)

vca <- as.data.frame(VarCorr(CO2_vca)) 

vca %>% group_by(grp) %>% summarise(
  varprop = vcov / sum(vca$vcov)*100) %>% arrange(
    varprop, grp)


#### Transport ----




out_a %>% ggplot(aes(y=transport_tot, x=year, 
                     group=croprot_man, soilCinit,
                     col=croprot_man))+
  geom_point(size=0.5)+
  geom_smooth()+
  geom_line(aes(y=(temp/1000)+0.02), col="black")+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  scale_x_continuous(breaks=seq(1996,2020,4))+
  #scale_y_continuous(sec.axis = sec_axis(~(.*1000)-0.02, name="Temp"))+
  facet_grid(initC~soilCinit)+
  theme_bw()+ylab("Transport C")



trans_vca<-lme4::lmer(
  transport_tot~1+year+
    +(1|temp)+(1|soilCinit)+(1|soiltype)+(1|croprot)+(1|inittime)+(1|initC)
  ,na.action=na.omit
  ,REML=T
  ,control=lmerControl(optimizer="bobyqa")
  ,data=out_a)

summary(trans_vca)

vca <- as.data.frame(VarCorr(trans_vca)) 

vca %>% group_by(grp) %>% summarise(
  varprop = vcov / sum(vca$vcov)*100) %>% arrange(
    varprop, grp)

out_a %>% ggplot(
  aes(y=transport_tot, x=croprot_man, fill= initC))+
  geom_boxplot() + theme_bw()




#### Relations between outputs ----

pluto <- dif %>% 
  select(dif_topC1995,totalC_topsoil_2020, Cinputsum) 

pluto[,-1] %>% 
  pairs()

dif %>%
  ggplot(aes(y=dif_topC1995, x=Cinputsum, 
             group=interaction(croprot_man, hal),
             col=croprot_man,
             shape=hal))+
  geom_point(#size=0.2, 
    #alpha =.3
  )+
  #geom_smooth()+
  scale_color_manual(name="Crop rotation", 
                     values=c("#15863E","#55E88B", "#EF8D64" ))+
  # scale_x_continuous(breaks=seq(1996,2020,4))+
  #scale_y_continuous(breaks =seq(40,180,10))+
  scale_linetype_manual(name="Allometric",
                        values = c("dotted","longdash",
                                   "solid"))+
  facet_grid(~initC)+
  theme(legend.position="bottom",
        panel.background = NULL,
        text = element_text(size=12))+
  ylab("")

cor(out_a$totalC_topsoil, out_a$totalC_subsoil)
#### input----

tbl_fill <- readRDS("tbl_fill_hal.RDS")

out_a %>% filter(year2 > "2015-05-02" & year2 < "2020-06-02") %>%
  group_by(croprot_man, hal, soiltype, soilCinit, inittime) %>% summarise(
    BCtopsum = sum(`Carbon deposited in the topsoil (t/ha)`),
    Ctopmean = mean(`Carbon deposited in the topsoil (t/ha)`),
    Ctopsd = sd(`Carbon deposited in the topsoil (t/ha)`),
    
    ACsubsum = sum(`C deposited in the subsoil (t/ha)`),
    Csubmean = mean(`C deposited in the subsoil (t/ha)`),
    Csubsd = sd(`C deposited in the subsoil (t/ha)`),
    
    CCmansum = sum(`C deposited in the topsoil from manure (tC ha-1)`),
    Cmanmean = mean(`C deposited in the topsoil from manure (tC ha-1)`),
    Cmansd = sd(`C deposited in the topsoil from manure (tC ha-1)`)
  ) %>% filter(soiltype == 'JB4' &
                 soilCinit == 'sC1.5' & inittime == '30') %>%
  pivot_longer(cols=c(BCtopsum,ACsubsum,CCmansum), names_to = "Frac", values_to = "Cinp") %>% 
  ggplot(aes(y = Cinp,
             x = fct_reorder(interaction(croprot_man, hal,  sep = " "), Cinp),
             fill=Frac, Cinp)) +
  geom_bar(position="stack",stat = "identity")+
  scale_fill_manual(name="",labels=c("Manure","Topsoil", "Subsoil" ),
                    values=c("#330000","#9C5712","#E49D56" ))+
  theme(panel.background = NULL)+
  scale_y_continuous(breaks = seq(0,300,50))+
  labs(y = "Total C (tC/ha) deposited in a five-year rortation",
       x= "Crop rotation and Allometric for grass")


vca <- read.table("vca.txt", sep="\t", dec = ".", header = TRUE, na.strings = ".")


vca_col <- c("soiltype"="#3E2723",
             "soilCinit"="#795548",
             "inittime"="#01045E",
             "initC"="#00838F",
             "croprot_man"="#2E7D32",
             "hal"="#66BB6A",
             "Temperature"="#EF9A9A",
             "Residual"="#E0E0E0"
)


vca %>% 
  pivot_longer(c("soiltype","soilCinit","inittime","initC",
                 "croprot_man","hal","Temperature","Residual")) %>% 
  mutate(Factor=fct_relevel(name, 
                            c("soiltype","soilCinit","inittime","initC",
                              "croprot_man","hal","Temperature","Residual")),
         Outputs=fct_relevel(Output, 
                             c("totalC_topsoil","totalC_subsoil",
                               "Difernce C top soil","CO2","Transport"))
  ) %>% 
  ggplot(aes(x=Outputs, y=value, fill=Factor)) +
  geom_bar(stat = "identity" )+
  scale_fill_manual(
    labels=c("Soil type","Initial Soil C", 
             "Initialization time","Initialization C input",
             "Crop rotation",
             "Allometric",
             "Temperature",
             "Residual"),
    values=vca_col
  )+
  scale_x_discrete(labels=c("Topsoil total C",
                            "Subsoil total C",
                            "Difference C",
                            "C0_2",
                            "Transport")
  )+
  labs(y="VC %")+
  #theme_minimal()+
  theme(legend.position="bottom",
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        text = element_text(size=18))

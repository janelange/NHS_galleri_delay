args <- commandArgs(trailingOnly = TRUE)
scenario_no <-as.numeric(args[1])

library(ggplot2)
library(dplyr)
summary_list=list()


dirname=paste0("/home/groups/CEDAR/MCED_sim/Output/MARTA_sim/scenario_no_",scenario_no)
for(i in 1:200){
  thename=paste("summary_",i,".Rdata",sep="")
  
  try({load(file.path(dirname,thename))})
  summary_list[[i]]=out  

}

all_control=do.call(rbind,lapply(summary_list,"[[","control_cancers"))%>%mutate(rate=100000*clinical/PY)

p1=ggplot(data=subset(all_control,cancer_site=="Lung"),aes(x=age,y=rate))+
 geom_point(size=1,alpha=.2)+stat_summary(fun = "mean", colour = "red", geom = "point")+
  facet_grid(sex~stage)
p2=ggplot(data=subset(all_control,cancer_site=="Colorectal"),aes(x=age,y=rate))+
  geom_point(size=1,alpha=.2)+stat_summary(fun = "mean", colour = "red", geom = "point")+
  facet_grid(sex~stage)
p3=ggplot(data=subset(all_control,cancer_site=="Liver"),aes(x=age,y=rate))+
  geom_point(size=1,alpha=.2)+stat_summary(fun = "mean", colour = "red", geom = "point")+
  facet_grid(sex~stage)

all_screen=do.call(rbind,lapply(summary_list,"[[","summary_screen_cancers"))
all_at_risk=do.call(rbind,lapply(summary_list,"[[","summary_screen_at_risk"))
all_interval=do.call(rbind,lapply(summary_list,"[[","summary_interval_cancers"))

save(all_control,all_screen,all_at_risk, all_interval, file=file.path(dirname, paste0("combined_summary",scenario_no,".Rdata")))





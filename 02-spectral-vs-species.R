#Loading libraries
library(tidyverse)
library(raster)
library(rgdal)
library(mapview)
library(janitor)
library(ggthemes)
library(spdplyr)
library(broom)
scenes <-  stack(list.files(path="rao/", full.names=TRUE, pattern = ".grd")) # Loading files in SLC folder, these are calculated in "01-rao-calculation.R" script and saved in that folder
proj4string(scenes) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #assign the projection to be able to replot them and sample using shapefiles
extent(scenes)<-extent(566186.1, 574466.9, 2692043, 2701407) # setting the extent to the scenes

xy <- read.csv("data/data_table.csv")

xy <- xy %>% group_by(scene, id, x, y, rao_div) %>% 
        summarise(Hmean=mean(Hdiv)) 

summary(lm(xy$Hmean ~ xy$rao_div))

cor(xy[c(5,6)])

(p <- ggplot(xy, aes(x=Hmean, y=rao_div))+
        geom_point(pch=21, fill="gray90", alpha=0.7, size=3, stroke=1.2)+
        geom_smooth(method="lm", se=F, col="black")+
        labs(x="Shannon index", y="Rao spectral index")+
        theme_tufte()+
        theme(text=element_text(size=25), panel.border = element_rect(fill=NA, colour="black"), legend.position = ""))


ggsave("correlation.jpeg", plot = p,dpi = 300)

 
xy <- read.csv("data/data_table.csv")
xy <- xy %>% group_by(scene, id, x, y, substrate_type, rao_div) %>% 
        summarise(Hmean=mean(Hdiv))

(p <- ggplot(xy, aes(x=Hmean, y=rao_div))+
                geom_point(pch=21, fill="gray90", alpha=0.7, size=3, stroke=1.2)+
                geom_smooth(method="lm", se=F, col="black")+
                facet_wrap(~substrate_type)+
                labs(x="Shannon index", y="Rao spectral index")+
                theme_tufte()+
                theme(text=element_text(size=14), panel.border = element_rect(fill=NA, colour="black"), legend.position = ""))

ggsave("figs/correlation_types.jpeg", plot = p,dpi = 300)



xy_nested <- xy %>% group_by(substrate_type) %>% 
        nest()

mod_fun <- function(df){
        lm(Hmean~rao_div, data=df)
}

m_xy <- xy_nested %>% 
        mutate(model=map(data, mod_fun))

b_fun <- function(mod){
        summary(mod)
}

model_summary<- xy %>% 
        group_by(substrate_type) %>% 
        nest() %>% 
        mutate(
                model = map(data, mod_fun), # S3 list-col
                tidied = map(model, tidy)
        ) %>% 
        unnest(tidied, .drop = TRUE)


write.table(model_summary, "model_summary.txt", sep=",", quote=F, row.names = F)


xy %>% group_by(scene) %>% 
        summarise(meanH=mean(Hmean), sdH= sd(Hmean), meanRao=mean(rao_div), sdRao=sd(rao_div))



xy %>% mutate(latrange=cut(y,seq(2693359,2800210, by=100))) %>% 
        group_by(scene, latrange) %>% 
        summarise(meanH=mean(Hmean), meanRao=100*mean(rao_div)) %>% 
        gather(key="index", value = "value", meanH,meanRao) %>% 
        ggplot(aes(x=factor(latrange), y=value, fill=index))+
        geom_col()

xy %>% mutate(Trao=rao_div*10) %>% 
        gather(key="index", value = "value", Trao, Hmean, -rao_div) %>% 
        ggplot(aes(x=y, y=value, col=index))+
        geom_point() +
        coord_flip()+
        facet_grid(scene~.,)+
        theme_tufte()+
        theme(panel.border = element_rect(fill=NA, colour="black"), legend.position = "top")





plot(scenes)

plot(scenes$spr_20161214-scenes$spr_20161011    )

########################################################################################
### Supplementary code for the statistical analysis presented in section 3.1 and 3.2 ###
##                           A github page with data is available at:                 ##
##            https://github.com/Fabbiologia/supplementary_code_Favorettoetal2019     ##
##       Please contact favoretto.fabio@gmail.com for any problems running the code   ##
########################################################################################



# Loading needed libraries ------------------------------------------------

# if not previously installed you can uncomment the following to install and then load the needed
# packages

# install.packages(c("tidyverse", "janitor", "ggthemes", "broom", "raster", "mapview"))

# loading libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(broom)
library(raster)
library(mapview)
library(htt)

## Loading data
sampling_points <- shapefile("shp/Sampling.shp")
mapview(sampling_points)

read.csv(url(
        "https://www.dropbox.com/s/z8n8x250ew7rt8s/data_table.csv?dl=0"
))

# 01- Loading datasets ----------------------------------------------------
df <-read.csv('https://raw.githubusercontent.com/Fabbiologia/supplementary_code_Favorettoetal2019/master/data/data_table.csv')

# check the dataset
head(df)

# The transect column represent the transect # T1, T2, T3 in each sampling point
# the sampling point is represented by the id
# x, y are point coordinates
# scene is the name of the Landsat 8 scene
# for substrate class and type see section 3.2
# rao_div is the rao index value extracted from the scenes on the sampling point
# then taxa and substrate follows
# ttot is the number of total points registered in the transect
# Hdiv is the shannon diversity calculated


last_scenes <- df %>% group_by(scene, id, x, y, rao_div) %>% 
        summarise(Hmean=mean(Hdiv)) 

m1 <- lm(log(last_scenes$Hmean) ~ log(last_scenes$rao_div))
plot(m1)
summary(lm(last_scenes$Hmean ~ last_scenes$rao_div))

cor(last_scenes[c(5,6)])

(p <- ggplot(last_scenes, aes(x=Hmean, y=rao_div))+
                geom_point(pch=21, fill="gray90", alpha=0.7, size=3, stroke=1.2)+
                geom_smooth(method="lm", se=F, col="black")+
                labs(x="Shannon index", y="Rao spectral index")+
                theme_tufte()+
                theme(text=element_text(size=25), panel.border = element_rect(fill=NA, colour="black"), legend.position = ""))





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


library(tidyverse)
require(cowplot)
library(ggthemes)

df <- read.csv("data/data_table.csv")

dateref <- data.frame(scene=unique(df$scene), date=c("2016-10-11", "2016-12-14"))

df<- df %>% merge(., dateref, by="scene") %>% dplyr::select(-date.x)

df$substrate_type <- factor(df$substrate_type, labels = c("Caulerpa bed", "Coarse sand", "Fine sand", "Rhodolith bed", "Rocky outcrop"))

(pA <- df %>% 
        ggplot(aes(x=substrate_class, y=rao_div))+
        geom_boxplot()+
        labs(x="", y="Rao Q index")+
        theme_tufte()+
        theme(panel.border = element_rect(fill = NA), text=element_text(size=18)))

(pB <- df %>% 
        ggplot(aes(x=substrate_type, y=rao_div))+
        geom_boxplot()+
        labs(x="", y="")+
        theme_tufte()+
        theme(panel.border = element_rect(fill = NA), text=element_text(size=18)))
(pC <- df %>% 
        ggplot(aes(x=substrate_class, y=Hdiv))+
        geom_boxplot()+
        labs(x="", y="Shannon index (H)")+
        theme_tufte()+
        theme(panel.border = element_rect(fill = NA), text=element_text(size=18)))

(pD <- df %>% 
        ggplot(aes(x=substrate_type, y=Hdiv))+
        geom_boxplot()+
        labs(x="", y="")+
        theme_tufte()+
        theme(panel.border = element_rect(fill = NA), text=element_text(size=18)))


(p <- plot_grid(pA, pB, pC, pD, labels = c('A', 'B', 'C', 'D'), align = "h"))

ggsave2(p, filename = "figs/richness_substrate.png", dpi = 500, width = 14)


kruskal.test(Hdiv~substrate_class, data=df)

kruskal.test(Hdiv~substrate_type, data=df)
res <- FSA::dunnTest(Hdiv~substrate_type, data=df, method="bh")
res$res$p.rounded <- round(res$res$P.adj, 3)


df %>% select(id,ï..transect, substrate_class, substrate_type) %>% 
        count(substrate_type)




kruskal.test(Hdiv~substrate_class, data=df)

kruskal.test(Hdiv~substrate_type, data=df)
res <- FSA::dunnTest(Hdiv~substrate_type, data=df, method="bh")
res$res$p.rounded <- round(res$res$P.adj, 3)
res




kruskal.test(rao_div~substrate_class, data=df)

kruskal.test(rao_div~substrate_type, data=df)
res <- FSA::dunnTest(rao_div~substrate_type, data=df, method="bh")
res$res$p.rounded <- round(res$res$P.adj, 3)
res



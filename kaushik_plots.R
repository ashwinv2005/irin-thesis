sg = read.csv("sg.csv")
owl = read.csv("owl.csv")
require(tidyverse)
require(ggthemes)

theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

##### abundance

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Ind,length(a1$Total_Ind),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Ind,length(a1$Total_Ind),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Ind,length(a1$Total_Ind),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Ind,length(a1$Total_Ind),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge$type = "abundance"
edge1 = edge



##### richness

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Sp.,length(a1$Total_Sp.),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Sp.,length(a1$Total_Sp.),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Sp.,length(a1$Total_Sp.),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$Total_Sp.,length(a1$Total_Sp.),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge2 = edge
edge2$type = "richness"
edge = rbind(edge1,edge2)

edge$Edge_Type = factor(edge$Edge_Type, levels = c("SR","LR","PE","TE"))


#Code for the barplot
ggp = ggplot(data = edge, aes(x = Edge_Type, y = mean)) +
  facet_wrap(.~type, nrow = 2, ncol = 1, scales = "free") +
  geom_bar(stat="identity", width=0.6,fill="#FF9933")+
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.9, width = 0.125) +
  xlab("Edge Type") +
  ylab("Count")+
  theme_tufte_revised()
ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 13),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="serif")) +
  theme(strip.text.x = element_text(size = 16)) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1),
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("SR","LR","PE","TE"),
                   labels = c("Small Road","Large Road","Paddy","Tea"))
#theme(panel.background = element_rect(fill = NA, color = "black"))

png('Fig. a.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()








##### generalists

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$gen,length(a1$gen),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$gen,length(a1$gen),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$gen,length(a1$gen),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$gen,length(a1$gen),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge$type = "generalists"
edge1 = edge



##### insectivores

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ins,length(a1$ins),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ins,length(a1$ins),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ins,length(a1$ins),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ins,length(a1$ins),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge2 = edge
edge2$type = "insectivores"
edge = rbind(edge1,edge2)

edge$Edge_Type = factor(edge$Edge_Type, levels = c("SR","LR","PE","TE"))


#Code for the barplot
ggp = ggplot(data = edge, aes(x = Edge_Type, y = mean)) +
  facet_wrap(.~type, nrow = 2, ncol = 1, scales = "free") +
  geom_bar(stat="identity", width=0.6,fill="#FF9933")+
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.9, width = 0.125) +
  xlab("Edge Type") +
  ylab("Abundance")+
  theme_tufte_revised()
ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 13),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="serif")) +
  theme(strip.text.x = element_text(size = 16)) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1),
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("SR","LR","PE","TE"),
                   labels = c("Small Road","Large Road","Paddy","Tea"))
#theme(panel.background = element_rect(fill = NA, color = "black"))

png('Fig. b.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()








##### OSO

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$OSO,length(a1$OSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$OSO,length(a1$OSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$OSO,length(a1$OSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$OSO,length(a1$OSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge$species = "Oriental Scops Owl"
edge1 = edge



##### CSO

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$CSO,length(a1$CSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$CSO,length(a1$CSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$CSO,length(a1$CSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$CSO,length(a1$CSO),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)


edge$species = "Collared Scops Owl"
edge2 = edge




##### ABO

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ABO,length(a1$ABO),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ABO,length(a1$ABO),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ABO,length(a1$ABO),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$ABO,length(a1$ABO),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)


edge$species = "Asian Barred Owlet"
edge3 = edge




##### BHO

edge = data.frame(Edge_Type = c("SR","LR","PE","TE"))
edge$mean = edge$cil = edge$cir = 0

a1 = sg %>% filter(Edge_Type == "SR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$BHO,length(a1$BHO),replace = T)
  b1[i] = mean(b)
}

edge$mean[1] = median(b1)
edge$cil[1] = quantile(b1,0.025)
edge$cir[1] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "LR")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$BHO,length(a1$BHO),replace = T)
  b1[i] = mean(b)
}

edge$mean[2] = median(b1)
edge$cil[2] = quantile(b1,0.025)
edge$cir[2] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "PE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$BHO,length(a1$BHO),replace = T)
  b1[i] = mean(b)
}

edge$mean[3] = median(b1)
edge$cil[3] = quantile(b1,0.025)
edge$cir[3] = quantile(b1,0.975)

a1 = sg %>% filter(Edge_Type == "TE")

b1 = numeric(1000)
for (i in 1:1000)
{
  b = sample(a1$BHO,length(a1$BHO),replace = T)
  b1[i] = mean(b)
}

edge$mean[4] = median(b1)
edge$cil[4] = quantile(b1,0.025)
edge$cir[4] = quantile(b1,0.975)

edge$species = "Brown Hawk-Owl"
edge4 = edge

edge = rbind(edge1,edge2,edge3,edge4)
edge$Edge_Type = factor(edge$Edge_Type, levels = c("SR","LR","PE","TE"))
edge$species = factor(edge$species, levels = c("Oriental Scops Owl","Asian Barred Owlet","Collared Scops Owl",
                                               "Brown Hawk-Owl"))


#Code for the barplot
ggp = ggplot(data = edge, aes(x = Edge_Type, y = mean)) +
  facet_wrap(.~species, nrow = 2, ncol = 2, scales = "free") +
  geom_bar(stat="identity", width=0.6,fill="#FF9933")+
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.9, width = 0.125) +
  xlab("Edge Type") +
  ylab("Abundance")+
  theme_tufte_revised()
ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 13),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="serif")) +
  theme(strip.text.x = element_text(size = 16)) +
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1),
  #                   limits = c(0,1)) +
  scale_x_discrete(breaks = c("SR","LR","PE","TE"),
                   labels = c("Small Road","Large Road","Paddy","Tea"))
#theme(panel.background = element_rect(fill = NA, color = "black"))


png('Fig. c.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

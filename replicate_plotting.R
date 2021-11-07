## alternate plotting

library(tidyverse)
library(ggthemes)

data_rep = read.csv("data_rep.csv")
forage_data_rep = read.csv("forage_data_rep.csv")
data_seg = read.csv("data_seg.csv")
forage_data_seg = read.csv("forage_data_seg.csv")

forage_data_rep$total_attempts1 = round((forage_data_rep$total_attempts/forage_data_rep$duration)*3)
forage_data_seg$total_attempts1 = round((forage_data_seg$total_attempts/forage_data_seg$duration)*3)


pref.rep = data_rep %>%
  group_by(segment,replicate.no,pref) %>% summarize(abund.wo = sum(total.count))

data_rep = left_join(data_rep,pref.rep)
forage_data_rep = left_join(forage_data_rep,pref.rep)

pref.seg = data_seg %>%
  group_by(segment,pref) %>% summarize(abund.wo = sum(total.count))

data_seg = left_join(data_seg,pref.seg)
forage_data_seg = left_join(forage_data_seg,pref.seg)

pref.rep = data_rep %>%
  group_by(segment,replicate.no,pref) %>% summarize(rich.wo = length(which(total.count != 0)))

data_rep = left_join(data_rep,pref.rep)
forage_data_rep = left_join(forage_data_rep,pref.rep)

pref.seg = data_seg %>%
  group_by(segment,pref) %>% summarize(rich.wo = length(which(total.count != 0)))

data_seg = left_join(data_seg,pref.seg)
forage_data_seg = left_join(forage_data_seg,pref.seg)

#data_rep = data_rep[data_rep$total.abund<2000,]
#forage_data_rep = forage_data_rep[forage_data_rep$total.abund<2000,]
#data_seg = data_seg[data_seg$total.abund<2000,]
#forage_data_seg = forage_data_seg[forage_data_seg$total.abund<2000,]


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

data = data_rep %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% distinct()
data$food.others = data$food.abund-data$worms
data1 = data
data1$food.type = "All Prey"
data1$food.abund.new = data1$food.abund
data2 = data
data2$food.type = "Worms"
data2$food.abund.new = data2$worms
data3 = data
data3$food.type = "Other (Non-Worm) Prey"
data3$food.abund.new = data3$food.others
data = rbind(data1,data2,data3)
data$food.type = factor(data$food.type,levels = c("All Prey","Worms","Other (Non-Worm) Prey"))

datax = data
data = data %>% select(-pref,-abund.wo,-rich.wo) %>% distinct()

ggp = ggplot(data = data[data$total.abund < 2000,], aes(x = food.abund.new, y = total.abund, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebirds (abundance)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")


png('rFig. 1.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = food.abund.new, y = rich, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms (richness)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 4.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()



ggp = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms",], 
             aes(x = food.abund.new, y = abund.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms (abundance)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 2.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$pref == "prefers worms",], aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms (richness)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 5.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers other foods",], 
             aes(x = food.abund.new, y = abund.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Other (Non-Worm) Prey (abundance)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 3.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$pref == "prefers other foods",], aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Other (Non-Worm) Prey (richness)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 6.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
             aes(x = food.abund.new, y = abund.wo, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms (abundance)")+
  theme_tufte_revised()

ggpy = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
              aes(x = food.abund.new, y = abund.wo, col = island)) +
  ggtitle("Shorebirds that Prefer Worms") +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Abundance")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

ggpx = ggpy +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(plot.title = element_text(hjust = 0.5, size = 24)) +
  theme(legend.title = element_blank(), legend.text = element_blank()) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "none")

png('rFig. 10.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



ggp = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
             aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms (richness)")+
  theme_tufte_revised()

ggpy = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
              aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Richness")+
  theme_tufte_revised()


ggp2 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(2,4,6,8)) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

ggp2x = ggpy +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), labels = c("    0","2","4","6","8")) +
  theme(strip.text.x = element_blank()) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 11a.jpg', units="in", width=10, height=7, res=1000)
ggp2
dev.off()

require(cowplot)
g1 = plot_grid(ggpx,ggp2x,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

png('rFig. 11.jpg', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()


lsp = data_rep %>%
  filter(obs.sp == "Lesser Sand Plover") %>% select(segment,replicate.no,total.count)
names(lsp)[2] = "total.count1"

datax = left_join(datax,lsp)
datax$total.abund1 = datax$total.abund - datax$total.count1

ggp = ggplot(data = datax[datax$total.abund < 2000  & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
             aes(x = food.abund.new, y = total.abund1, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebirds (abundance excluding LSP)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 12.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



datax$abund.wo1 = datax$abund.wo - datax$total.count1

ggp = ggplot(data = datax[datax$total.abund < 2000 & datax$pref == "prefers worms" & datax$food.type == "Worms",], 
             aes(x = food.abund.new, y = abund.wo1, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebirds that Prefer Worms\n(abundance excluding LSP)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 13.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()







#######################################3

## species by species

datax = data_rep
datax$count = 1
datax$count[datax$high.count == 0] = 0
datax = datax %>%
  group_by(obs.sp) %>% summarize(dets = sum(count)) %>%
  arrange(desc(dets))

specs = datax$obs.sp[1:10]
specs = specs[specs != "Kentish Plover"]

data = data_rep %>% filter(obs.sp %in% specs)
data$food.others = data$food.abund-data$worms
data1 = data
data1$food.type = "All Prey"
data1$food.abund.new = data1$food.abund
data2 = data
data2$food.type = "Worms"
data2$food.abund.new = data2$worms
data3 = data
data3$food.type = "Other (Non-Worm) Prey"
data3$food.abund.new = data3$food.others
data = rbind(data1,data2,data3)
data$food.type = factor(data$food.type,levels = c("All Prey","Worms","Other (Non-Worm) Prey"))


ggp = ggplot(data = data[data$food.type == "Worms" & data$total.count < 2000,], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebirds (abundance)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 7.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data[data$food.type == "Other (Non-Worm) Prey" & data$total.count < 2000,], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Non-Worm~Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebirds (abundance")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 8.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data[data$food.type == "Worms" & data$total.count < 2000 & data$obs.sp == "Lesser Sand Plover",], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Lesser Sand-Plover (abundance)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 9.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



################################ foraging analyses

data = forage_data_rep

ggp = ggplot(data = data[data$total_attempts>=10,], aes(x = worms, y = success_prop*100, col = island)) +
  facet_wrap(.~pref, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 14.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = data, aes(x = worms, y = total_attempts1, col = island)) +
  facet_wrap(.~pref, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 15.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

data = forage_data_rep %>% filter(obs.sp %in% specs)

ggp = ggplot(data = data[data$total_attempts>=10,], aes(x = worms, y = success_prop*100, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 16.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = worms, y = total_attempts1, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 17.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


############### overall foraging analyses

require(lme4)
require(boot)
data = forage_data_rep

## Black-tailed Godwit - success

datax = data %>% filter(obs.sp == "Black-tailed Godwit")

fit = lm(success_prop ~ worms*island, data = datax, weights = total_attempts)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

#newdata$pred = inv.logit(newdata$pred)
#newdata$cil = inv.logit(newdata$cil)
#newdata$cir = inv.logit(newdata$cir)

newdata$species = "Black-tailed Godwit"
newdata1 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Black-tailed Godwit",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_smooth(data = newdata1, aes(x = worms, y = pred*100), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata1, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 20.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Common Greenshank - success

datax = data %>% filter(obs.sp == "Common Greenshank")

fit = lm(success_prop ~ worms*island, data = datax, weights = total_attempts)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

#newdata$pred = inv.logit(newdata$pred)
#newdata$cil = inv.logit(newdata$cil)
#newdata$cir = inv.logit(newdata$cir)

newdata$species = "Common Greenshank"
newdata2 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Common Greenshank",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_smooth(data = newdata2, aes(x = worms, y = pred*100), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata2, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 21.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Common Redshank - success

datax = data %>% filter(obs.sp == "Common Redshank")

fit = lm(success_prop ~ worms*island, data = datax, weights = total_attempts)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

#newdata$pred = inv.logit(newdata$pred)
#newdata$cil = inv.logit(newdata$cil)
#newdata$cir = inv.logit(newdata$cir)

newdata$species = "Common Redshank"
newdata3 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Common Redshank",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_smooth(data = newdata3, aes(x = worms, y = pred*100), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata3, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 22.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Lesser Sand Plover - success

datax = data %>% filter(obs.sp == "Lesser Sand Plover")

fit = lm(success_prop ~ worms*island, data = datax, weights = total_attempts)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

#newdata$pred = inv.logit(newdata$pred)
#newdata$cil = inv.logit(newdata$cil)
#newdata$cir = inv.logit(newdata$cir)

newdata$species = "Lesser Sand Plover"
newdata4 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Lesser Sand Plover",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_smooth(data = newdata4, aes(x = worms, y = pred*100), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata4, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 23.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

newdata4$pred = newdata4$cil = newdata4$cir = NaN

newdatas = rbind(newdata1,newdata2,newdata3,newdata4)

















## Black-tailed Godwit - total

datax = data %>% filter(obs.sp == "Black-tailed Godwit")

fit = glm(total_attempts1 ~ worms*island, data = datax,
          family=poisson(link = 'log'))
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = exp(newdata$pred)
newdata$cil = exp(newdata$cil)
newdata$cir = exp(newdata$cir)

newdata$species = "Black-tailed Godwit"
newdata1 = newdata

ggp = ggplot(data = data[data$obs.sp == "Black-tailed Godwit",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata1, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata1, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 24.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

newdata1$pred = newdata1$cil = newdata1$cir = NaN

## Common Greenshank - total

datax = data %>% filter(obs.sp == "Common Greenshank")

fit = glm(total_attempts1 ~ worms*island, data = datax,
          family=poisson(link = 'log'))
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = exp(newdata$pred)
newdata$cil = exp(newdata$cil)
newdata$cir = exp(newdata$cir)

newdata$species = "Common Greenshank"
newdata2 = newdata

ggp = ggplot(data = data[data$obs.sp == "Common Greenshank",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata2, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata2, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 25.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

newdata2$pred = newdata2$cil = newdata2$cir = NaN


## Common Redshank - total

datax = data %>% filter(obs.sp == "Common Redshank")

fit = glm(total_attempts1 ~ worms*island, data = datax,
          family=poisson(link = 'log'))
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = exp(newdata$pred)
newdata$cil = exp(newdata$cil)
newdata$cir = exp(newdata$cir)

newdata$species = "Common Redshank"
newdata3 = newdata

ggp = ggplot(data = data[data$obs.sp == "Common Redshank",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata3, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata3, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 26.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Lesser Sand Plover - total

datax = data %>% filter(obs.sp == "Lesser Sand Plover")

fit = glm(total_attempts1 ~ worms*island, data = datax,
          family=poisson(link = 'log'))
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = exp(newdata$pred)
newdata$cil = exp(newdata$cil)
newdata$cir = exp(newdata$cir)

newdata$species = "Lesser Sand Plover"
newdata4 = newdata

ggp = ggplot(data = data[data$obs.sp == "Lesser Sand Plover",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata4, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata4, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 27.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Eurasian Curlew - total

datax = data %>% filter(obs.sp == "Eurasian Curlew")

fit = glm(total_attempts1 ~ worms*island, data = datax,
          family=poisson(link = 'log'))
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm, island = "Damar Char")

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = exp(newdata$pred)
newdata$cil = exp(newdata$cil)
newdata$cir = exp(newdata$cir)

newdata$species = "Eurasian Curlew"
newdata5 = newdata

ggp = ggplot(data = data[data$obs.sp == "Eurasian Curlew",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata4, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata4, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 28.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



newdatat = rbind(newdata1,newdata2,newdata3,newdata4)



data$species = data$obs.sp
data = data %>% filter(species %in% unique(newdatas$species))

ggp = ggplot(data = data[data$total_attempts>=10,]) +
  facet_wrap(.~species, nrow = 2, ncol = 2, scales = "free") +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 1.5) +
  geom_smooth(data = newdatas, aes(x = worms, y = pred*100), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdatas, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Worm~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[c(3,1)]) +
  theme(legend.position = "bottom")

png('rFig. 29.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


head(data_rep)

r1 = data_rep %>% filter(replicate.no == 1, total.abund <= 1500) %>% distinct(segment,total.abund)
names(r1)[2] = c("food.abund1")

r2 = data_rep %>% filter(replicate.no == 2, total.abund <= 1500) %>% distinct(segment,total.abund)
names(r2)[2] = c("food.abund2")

r = left_join(r1,r2)

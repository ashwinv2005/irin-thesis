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

#data_rep = data_rep[data_rep$total.abund<350,]
#forage_data_rep = forage_data_rep[forage_data_rep$total.abund<350,]
#data_seg = data_seg[data_seg$total.abund<350,]
#forage_data_seg = forage_data_seg[forage_data_seg$total.abund<350,]


theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

clrs = c("#0072B2", "#E69F00")

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

data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% distinct()
data$food.others = data$food.abund-data$worms
data1 = data
data1$food.type = "All Prey"
data1$food.abund.new = data1$food.abund
data2 = data
data2$food.type = "Polychaetes"
data2$food.abund.new = data2$worms
data3 = data
data3$food.type = "Other (Non-Polychaete) Prey"
data3$food.abund.new = data3$food.others
data = rbind(data1,data2,data3)
data$food.type = factor(data$food.type,levels = c("All Prey","Polychaetes","Other (Non-Polychaete) Prey"))

datax = data
data = data %>% select(-pref,-abund.wo,-rich.wo) %>% distinct()
datay = data

datay$total.abund[datay$food.type == "Polychaetes"]  = 
  datax$abund.wo[datax$food.type == "Polychaetes" & datax$pref == "prefers worms"]
datay$total.abund[datay$food.type == "Other (Non-Polychaete) Prey"] = 
  datax$abund.wo[datax$food.type == "Other (Non-Polychaete) Prey" & datax$pref == "prefers other foods"]

datay = datay[datay$food.type != "All Prey",]
newdatay = datay
newdatay = newdatay[newdatay$total.abund < 350,]
newdatay$total.abund[newdatay$food.type == "Other (Non-Polychaete) Prey"] = NA

ggp = ggplot(data = datay[datay$total.abund < 350,], aes(x = food.abund.new, y = total.abund, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(data = newdatay, method = "lm", formula = y ~ x + 0, se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebird Density (Individuals per Segment)")+
  theme_tufte_revised()

clrs = c("black", "grey")
clrs = c("#de853a","#376a94")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")


png('Fig. 1.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()

ggp = ggplot(data = data[data$total.abund < 350,], aes(x = food.abund.new, y = total.abund, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebird Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")


png('Fig. 1a.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = food.abund.new, y = rich, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebird Richness (Species per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 4.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()



ggp = ggplot(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms",], 
             aes(x = food.abund.new, y = abund.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Polychaete-eating Shorebird Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 2.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$pref == "prefers worms",], aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Polychaete-eating Shorebird Richness (Species per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 5.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$total.abund < 350 & datax$pref == "prefers other foods",], 
             aes(x = food.abund.new, y = abund.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Other prey-eating Shorebird Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 3.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = datax[datax$pref == "prefers other foods",], aes(x = food.abund.new, y = rich.wo, col = island)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Other Prey-eating Shorebird Richness (Species per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(0,300,600,900,1200)) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 6.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


tempdat = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",]

fit1 = glm(abund.wo ~ 0 + log(food.abund.new):island, data = tempdat, family = poisson)
summary(fit1)

fd = seq(1,max(datax$food.abund.new),0.01)

newdata1 = data.frame(food.abund.new = fd, island = "Damar Char")
newdata2 = data.frame(food.abund.new = fd, island = "Nijhum Dweep")
newdata = rbind(newdata1,newdata2)


a = predict(fit1, newdata = newdata, se.fit = T, type = "link")

newdata$pred = exp(a$fit)
newdata$cil = exp(a$fit - 1.96*a$se.fit)
newdata$cir = exp(a$fit + 1.96*a$se.fit)

newdata1 = newdata
newdata1a = newdata1


ggp = ggplot() +
  #facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = abund.wo, col = island), size = 3) +
  geom_line(data = newdata1, aes(x = food.abund.new, y = pred, col = island), size = 1) +
  geom_ribbon(data = newdata1, 
              aes(x = food.abund.new, ymin = cil,ymax = cir, linetype = NA, fill = island), 
              alpha=0.3) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Polychaete-eating Shorebird Density (Individuals per Segment)")+
  theme_tufte_revised()

ggpy = ggplot() +
  ggtitle("Shorebirds that Prefer Polychaetes") +
  #facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = abund.wo, col = island), size = 4) +
  geom_line(data = newdata1, aes(x = food.abund.new, y = pred, col = island), size = 1) +
  geom_ribbon(data = newdata1, 
              aes(x = food.abund.new, ymin = cil,ymax = cir, linetype = NA, fill = island), 
              alpha=0.3) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Density\n(Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(plot.title = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_x_continuous(breaks = c(0,300,600,900),limits = c(0,1200)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  scale_fill_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                    labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

ggpx = ggpy +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(plot.title = element_text(hjust = 0.5, size = 24)) +
  theme(legend.title = element_blank(), legend.text = element_blank()) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_x_continuous(breaks = c(0,300,600,900),limits = c(0,1200)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  scale_fill_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "none")

png('Fig. 10.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


tempdat = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",]

fit2 = lm(rich.wo ~ 0 + log(food.abund.new):island, data = tempdat)
summary(fit2)




tempdat = datax[datax$total.abund < 350 & datax$pref == "prefers other foods" & datax$food.type == "Other (Non-Polychaete) Prey",]

fit1a = glm(abund.wo ~ 0 + log(food.abund.new+1):island, data = tempdat, family = poisson)
summary(fit1a)





fd = seq(1,max(datax$food.abund.new),0.01)

newdata1 = data.frame(food.abund.new = fd, island = "Damar Char")
newdata2 = data.frame(food.abund.new = fd, island = "Nijhum Dweep")
newdata = rbind(newdata1,newdata2)


a = predict(fit2, newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata2 = newdata



ggp = ggplot() +
  #facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = rich.wo, col = island), size = 3) +
  geom_line(data = newdata2, aes(x = food.abund.new, y = pred, col = island), size = 1) +
  geom_ribbon(data = newdata2, 
              aes(x = food.abund.new, ymin = cil,ymax = cir, linetype = NA, fill = island), 
              alpha=0.3) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Shorebird Richness (Species per Segment)")+
  theme_tufte_revised()

ggpy = ggplot() +
  #facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = rich.wo, col = island), size = 4) +
  geom_line(data = newdata2, aes(x = food.abund.new, y = pred, col = island), size = 1) +
  geom_ribbon(data = newdata2, 
              aes(x = food.abund.new, ymin = cil,ymax = cir, linetype = NA, fill = island), 
              alpha=0.3) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Richness\n(Species per Segment)")+
  theme_tufte_revised()


ggp2 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(2,4,6,8)) +
  scale_x_continuous(breaks = c(0,300,600,900),limits = c(0,1200)) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  scale_fill_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                    labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

ggp2x = ggpy +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), labels = c("    0","2","4","6","8")) +
  scale_x_continuous(breaks = c(0,300,600,900),limits = c(0,1200)) +
  theme(strip.text.x = element_blank()) +
  #scale_x_log10() +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  scale_fill_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 11a.jpg', units="in", width=10, height=7, res=1000)
ggp2
dev.off()

require(cowplot)
g1 = plot_grid(ggpx,ggp2x,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

png('Fig. 11.jpg', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()









lsp = data_seg %>%
  filter(obs.sp == "Lesser/Greater Sand-Plover") %>% select(segment,total.count)
names(lsp)[2] = "total.count1"

datax = left_join(datax,lsp)
datax$total.abund1 = datax$total.abund - datax$total.count1




ggp = ggplot(data = datax[datax$total.abund < 350  & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = total.abund1, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("All Shorebird Density\n(Individuals per Segment excluding LSP)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 12.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



datax$abund.wo1 = datax$abund.wo - datax$total.count1


tempdat = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",]

fit3 = glm(abund.wo1 ~ 0 + log(food.abund.new):island, data = tempdat, family = poisson)
summary(fit3)

ggp = ggplot(data = datax[datax$total.abund < 350 & datax$pref == "prefers worms" & datax$food.type == "Polychaetes",], 
             aes(x = food.abund.new, y = abund.wo1, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Polychaete-eating Shorebird Density\n(Individuals per Segment excluding LSP)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 13.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()







#######################################3

## species by species

datax = data_seg
datax$count = 1
datax$count[datax$high.count == 0] = 0
datax = datax %>%
  group_by(obs.sp) %>% summarize(dets = sum(count)) %>%
  arrange(desc(dets))

specs = datax$obs.sp[1:10]
specs = specs[specs != "Kentish Plover"]

datay = data_seg %>% filter(obs.sp %in% specs)
datay1 = datay %>% filter(pref == "prefers worms")
datay1$food.abund.new = datay1$worms
datay2 = datay %>% filter(pref == "prefers other foods")
datay2$food.abund.new = datay2$food.abund-datay2$worms
datay = rbind(datay1,datay2)

data = data_seg %>% filter(obs.sp %in% specs)
data$food.others = data$food.abund-data$worms
data1 = data
data1$food.type = "All Prey"
data1$food.abund.new = data1$food.abund
data2 = data
data2$food.type = "Polychaetes"
data2$food.abund.new = data2$worms
data3 = data
data3$food.type = "Other (Non-Polychaete) Prey"
data3$food.abund.new = data3$food.others
data = rbind(data1,data2,data3)
data$food.type = factor(data$food.type,levels = c("All Prey","Polychaetes","Other (Non-Polychaete) Prey"))

newdata = datay[datay$total.count < 350,]
newdata$total.count[!newdata$obs.sp %in% c("Common Redshank","Lesser/Greater Sand-Plover")] = NA

dataz = datay
dataz$obs.sp[dataz$obs.sp == "Lesser/Greater Sand-Plover"] = "Les/Gre Sand-Plover"
newdata1 = newdata
newdata1$obs.sp[newdata1$obs.sp == "Lesser/Greater Sand-Plover"] = "Les/Gre Sand-Plover"


tempdat = data[data$food.type == "Polychaetes" & data$total.count < 350 & data$obs.sp == "Lesser/Greater Sand-Plover",]

fit4 = glm(total.count ~ 0 + log(food.abund.new):island, data = tempdat, family = poisson)
summary(fit4)

fd = seq(1,max(data$food.abund.new),0.01)

newdata1 = data.frame(food.abund.new = fd, island = "Damar Char")
newdata2 = data.frame(food.abund.new = fd, island = "Nijhum Dweep")
newdata = rbind(newdata1,newdata2)


a = predict(fit4, newdata = newdata, se.fit = T, type = "link")

newdata$pred = exp(a$fit)
newdata$cil = exp(a$fit - 1.96*a$se.fit)
newdata$cir = exp(a$fit + 1.96*a$se.fit)

newdata4 = newdata
newdata4$obs.sp = "Les/Gre Sand-Plover"



tempdat = data[data$food.type == "Polychaetes" & data$total.count < 350 & data$obs.sp == "Common Redshank",]

fit5 = glm(total.count ~ 0 + log(food.abund.new):island, data = tempdat, family = poisson)
summary(fit5)


par(mfrow = c(2, 2))
plot(fit1)

par(mfrow = c(2, 2))
plot(fit2)

par(mfrow = c(2, 2))
plot(fit1a)

par(mfrow = c(2, 2))
plot(fit3)

par(mfrow = c(2, 2))
plot(fit4)

par(mfrow = c(2, 2))
plot(fit5)

fd = seq(1,max(data$food.abund.new),0.01)

newdata1 = data.frame(food.abund.new = fd, island = "Damar Char")
newdata2 = data.frame(food.abund.new = fd, island = "Nijhum Dweep")
newdata = rbind(newdata1,newdata2)


a = predict(fit5, newdata = newdata, se.fit = T, type = "link")

newdata$pred = exp(a$fit)
newdata$cil = exp(a$fit - 1.96*a$se.fit)
newdata$cir = exp(a$fit + 1.96*a$se.fit)


newdata5 = newdata
newdata5$obs.sp = "Common Redshank"

newdatax = rbind(newdata4, newdata5)

newdatar = newdata
newdatar$pred = newdatar$cil = newdatar$cir = NA

newdatar1 = newdatar
newdatar1$obs.sp = "Black-headed Ibis"
newdatar2 = newdatar
newdatar2$obs.sp = "Black-tailed Godwit"
newdatar3 = newdatar
newdatar3$obs.sp = "Common Greenshank"
newdatar4 = newdatar
newdatar4$obs.sp = "Eurasian Curlew"
newdatar5 = newdatar
newdatar5$obs.sp = "Grey Plover"
newdatar6 = newdatar
newdatar6$obs.sp = "Little/Red-necked Stint"
newdatar7 = newdatar
newdatar7$obs.sp = "Pied Avocet"

newdatax = rbind(newdatax,newdatar1,newdatar2,newdatar3,newdatar4,newdatar5,newdatar6,newdatar7)



ggp = ggplot(data = dataz[dataz$total.count < 350,]) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(data = dataz[dataz$total.count < 350,], 
             aes(x = food.abund.new, y = total.count, col = island), size = 2) +
  geom_line(data = newdatax, aes(x = food.abund.new, y = pred, col = island), size = 1) +
  geom_ribbon(data = newdatax, 
              aes(x = food.abund.new, ymin = cil,ymax = cir, linetype = NA, fill = island), 
              alpha=0.3) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  scale_x_continuous(breaks = c(0,300,600,900),limits = c(0,1200)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  scale_fill_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

#library(lemon)
#ggp1 = ggp1 + facet_rep_wrap(~obs.sp, scales='free_y', repeat.tick.labels = 'left')

png('Fig. 7.jpg', units="in", width=7, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data[data$food.type == "Polychaetes" & data$total.count < 350,], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 7a.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data[data$food.type == "Other (Non-Polychaete) Prey" & data$total.count < 350,], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Non-Polychaete~Prey~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 8.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data[data$food.type == "Polychaetes" & data$total.count < 350 & data$obs.sp == "Lesser/Greater Sand-Plover",], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Lesser/Greater Sand-Plover Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 9a.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = data[data$food.type == "Polychaetes" & data$total.count < 350 & data$obs.sp == "Common Redshank",], 
             aes(x = food.abund.new, y = total.count, col = island)) +
  facet_wrap(.~island, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, formula = y ~ x + 0) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Lesser/Greater Sand-Plover Density (Individuals per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 9b.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()




################################ foraging analyses

data = forage_data_seg

ggp = ggplot(data = data[data$total_attempts>=10,], aes(x = worms, y = success_prop*100, col = island)) +
  facet_wrap(.~pref, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 14.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = data, aes(x = worms, y = total_attempts1, col = island)) +
  facet_wrap(.~pref, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 2) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 15.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

data = forage_data_seg %>% filter(obs.sp %in% specs)

ggp = ggplot(data = data[data$total_attempts>=10,], aes(x = worms, y = success_prop*100, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 16.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = worms, y = total_attempts1, col = island)) +
  facet_wrap(.~obs.sp, nrow = 3, ncol = 3, scales = "free") +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 17.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()




############## relationship between food and temperature/ soil density


data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% select(-pref,-abund.wo,-rich.wo) %>% distinct()

datar = data_rep %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
datar = datar %>% select(-pref,-abund.wo,-rich.wo) %>% distinct()

data$food.abund.new = data$worms
data$type = "Polychaete Density"
data1 = data
data1$food.abund.new = data1$food.abund - data1$worms
data1$type = "Other Prey Density"
data = rbind(data,data1)


datar$food.abund.new = datar$worms
datar$type = "Polychaete Density"
datar1 = datar
datar1$food.abund.new = datar1$food.abund - datar1$worms
datar1$type = "Other Prey Density"
datar = rbind(datar,datar1)

data$type = factor(data$type, levels = c("Polychaete Density", "Other Prey Density"))
datar$type = factor(datar$type, levels = c("Polychaete Density", "Other Prey Density"))

ggp = ggplot(data = datar, aes(x = temp, y = food.abund.new, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(paste("Temperature","(°C)")) +
  ylab(expression(paste(Individuals~per~0.1~m^3))) +
  theme_tufte_revised()

ggpx = ggplot(data = datar, aes(x = temp, y = food.abund.new, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 4) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(paste("Temperature","(°C)")) +
  ylab("") +
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

ggp1x = ggpx +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 42), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=31","Nijhum Dweep N=25")) +
  theme(legend.position = "none")

png('Fig. 18.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



ggp = ggplot(data = data, aes(x = dryweight, y = food.abund.new, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Soil Density Index") +
  ylab(expression(paste(Individuals~per~0.1~m^3))) +
  theme_tufte_revised()

ggpy = ggplot(data = data, aes(x = dryweight, y = food.abund.new, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 4) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Soil Density Index") +
  ylab("") +
  theme_tufte_revised()

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

ggp2y = ggpy +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(angle = 90, size = 42), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_y_continuous(breaks = c(300,600,900), labels = c(" 300","600","900")) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 19a.jpg', units="in", width=10, height=7, res=1000)
ggp2
dev.off()

require(cowplot)
g1 = plot_grid(ggp1x,ggp2y,nrow=2,ncol=1,rel_widths = c(1/2, 1/2)) +
  draw_label(expression(paste(Individuals~per~m^2)), x=0, y=0.5, size = 24,
             vjust= 1.5, angle=90, fontfamily = "Gill Sans MT")

png('Fig. 19.jpg', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()

















############### overall foraging analyses

require(lme4)
require(boot)
data = forage_data_seg

## Black-tailed Godwit - success

datax = data %>% filter(obs.sp == "Black-tailed Godwit")

fit = glm(success_prop ~ log(worms), data = datax, weights = total_attempts, family = binomial)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm)


a = predict(fit, newdata = newdata, se.fit = T, type = "link")

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = inv.logit(newdata$pred)
newdata$cil = inv.logit(newdata$cil)
newdata$cir = inv.logit(newdata$cir)

newdata$species = "Black-tailed Godwit"
newdata1 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Black-tailed Godwit",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_line(data = newdata1, aes(x = worms, y = pred*100), size = 1, col = "grey") +
  geom_ribbon(data = newdata1, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 20.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Common Greenshank - success

datax = data %>% filter(obs.sp == "Common Greenshank")

fit = glm(success_prop ~ log(worms), data = datax, weights = total_attempts, family = binomial)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm)

a = predict(fit, newdata = newdata, se.fit = T, type = "link")

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = inv.logit(newdata$pred)
newdata$cil = inv.logit(newdata$cil)
newdata$cir = inv.logit(newdata$cir)

newdata$species = "Common Greenshank"
newdata2 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Common Greenshank",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_line(data = newdata2, aes(x = worms, y = pred*100), size = 1, col = "grey") +
  geom_ribbon(data = newdata2, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 21.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Common Redshank - success

datax = data %>% filter(obs.sp == "Common Redshank")

fit = glm(success_prop ~ log(worms), data = datax, weights = total_attempts, family = binomial)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm)

a = predict(fit, newdata = newdata, se.fit = T, type = "link")

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = inv.logit(newdata$pred)
newdata$cil = inv.logit(newdata$cil)
newdata$cir = inv.logit(newdata$cir)

newdata$species = "Common Redshank"
newdata3 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Common Redshank",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_line(data = newdata3, aes(x = worms, y = pred*100), size = 1, col = "grey") +
  geom_ribbon(data = newdata3, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 22.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Lesser Sand Plover - success

datax = data %>% filter(obs.sp == "Lesser/Greater Sand-Plover")

fit = glm(success_prop ~ log(worms), data = datax, weights = total_attempts, family = binomial)
summary(fit)

wm = seq(1,max(data$worms),0.01)


newdata = data.frame(worms = wm)

a = predict(fit, type = "link", newdata = newdata, se.fit = T)

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

newdata$pred = inv.logit(newdata$pred)
newdata$cil = inv.logit(newdata$cil)
newdata$cir = inv.logit(newdata$cir)

newdata$species = "Lesser/Greater Sand-Plover"
newdata4 = newdata

ggp = ggplot(data = data[data$total_attempts>=10 & data$obs.sp == "Lesser/Greater Sand-Plover",]) +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 2) +
  geom_line(data = newdata4, aes(x = worms, y = pred*100), size = 1, col = "grey") +
  geom_ribbon(data = newdata4, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 23.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

#newdata3$pred = newdata3$cil = newdata3$cir = NaN
#newdata4$pred = newdata4$cil = newdata4$cir = NaN

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
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 24.jpg', units="in", width=10, height=7, res=1000)
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
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 25.jpg', units="in", width=10, height=7, res=1000)
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
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 26.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


## Lesser Sand Plover - total

datax = data %>% filter(obs.sp == "Lesser/Greater Sand-Plover")

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

newdata$species = "Lesser/Greater Sand-Plover"
newdata4 = newdata

ggp = ggplot(data = data[data$obs.sp == "Lesser/Greater Sand-Plover",]) +
  geom_point(aes(x = worms, y = total_attempts1, col = island), size = 2) +
  geom_smooth(data = newdata4, aes(x = worms, y = pred), method = "glm", se = F, size = 1, col = cols[2]) +
  geom_ribbon(data = newdata4, aes(x = worms, ymin = cil,ymax = cir, linetype = NA), alpha=0.3, fill = cols[2]) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 27.jpg', units="in", width=10, height=7, res=1000)
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
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Total Foraging Attempts (per 3 min)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 28.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



newdatat = rbind(newdata1,newdata2,newdata3,newdata4)



data$species = data$obs.sp
data = data %>% filter(species %in% unique(newdatas$species))

ggp = ggplot(data = data[data$total_attempts>=10,]) +
  facet_wrap(.~species, nrow = 2, ncol = 2, scales = "free_y") +
  geom_point(aes(x = worms, y = success_prop*100, col = island), size = 1.5) +
  geom_line(data = newdatas, aes(x = worms, y = pred*100), size = 1, col = "grey") +
  geom_ribbon(data = newdatas, aes(x = worms, ymin = cil*100,ymax = cir*100, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab("Foraging Success (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Fig. 29.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% distinct() %>% filter(pref == "prefers worms")

t1 = data %>% group_by(island) %>% summarize(mn = mean(worms), cil = mean(worms) - 1.96*sd(worms),
                                             cir = mean(worms) + 1.96*sd(worms))
t1$type = "Polychaete Density"

a = numeric(1000)
b = numeric(1000)
a1 = data$total.abund[data$island == "Damar Char"]
b1 = data$total.abund[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

tx = t1
tx$type = "All Shorebirds"
tx$mn[tx$island == "Damar Char"] = quantile(a,0.5)
tx$cil[tx$island == "Damar Char"] = quantile(a,0.05)
tx$cir[tx$island == "Damar Char"] = quantile(a,0.95)

tx$mn[tx$island == "Nijhum Dweep"] = quantile(b,0.5)
tx$cil[tx$island == "Nijhum Dweep"] = quantile(b,0.05)
tx$cir[tx$island == "Nijhum Dweep"] = quantile(b,0.95)

a = numeric(1000)
b = numeric(1000)
a1 = data$abund.wo[data$island == "Damar Char"]
b1 = data$abund.wo[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

t2 = t1
t2$type = "Shorebirds that Prefer Polychaetes"
t2$mn[t2$island == "Damar Char"] = quantile(a,0.5)
t2$cil[t2$island == "Damar Char"] = quantile(a,0.05)
t2$cir[t2$island == "Damar Char"] = quantile(a,0.95)

t2$mn[t2$island == "Nijhum Dweep"] = quantile(b,0.5)
t2$cil[t2$island == "Nijhum Dweep"] = quantile(b,0.05)
t2$cir[t2$island == "Nijhum Dweep"] = quantile(b,0.95)

a = numeric(1000)
b = numeric(1000)
a1 = data$rich.wo[data$island == "Damar Char"]
b1 = data$rich.wo[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

t3 = t1
t3$type = "Shorebirds that Prefer Polychaetes"
t3$mn[t3$island == "Damar Char"] = quantile(a,0.5)
t3$cil[t3$island == "Damar Char"] = quantile(a,0.05)
t3$cir[t3$island == "Damar Char"] = quantile(a,0.95)

t3$mn[t3$island == "Nijhum Dweep"] = quantile(b,0.5)
t3$cil[t3$island == "Nijhum Dweep"] = quantile(b,0.05)
t3$cir[t3$island == "Nijhum Dweep"] = quantile(b,0.95)

a = numeric(1000)
b = numeric(1000)
a1 = data$rich[data$island == "Damar Char"]
b1 = data$rich[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

t4 = t1
t4$type = "All Shorebirds"
t4$mn[t4$island == "Damar Char"] = quantile(a,0.5)
t4$cil[t4$island == "Damar Char"] = quantile(a,0.05)
t4$cir[t4$island == "Damar Char"] = quantile(a,0.95)

t4$mn[t4$island == "Nijhum Dweep"] = quantile(b,0.5)
t4$cil[t4$island == "Nijhum Dweep"] = quantile(b,0.05)
t4$cir[t4$island == "Nijhum Dweep"] = quantile(b,0.95)

a = numeric(1000)
b = numeric(1000)
a1 = data$food.abund[data$island == "Damar Char"]
b1 = data$food.abund[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

ty = t1
ty$type = "Prey Density"
ty$mn[ty$island == "Damar Char"] = quantile(a,0.5)
ty$cil[ty$island == "Damar Char"] = quantile(a,0.05)
ty$cir[ty$island == "Damar Char"] = quantile(a,0.95)

ty$mn[ty$island == "Nijhum Dweep"] = quantile(b,0.5)
ty$cil[ty$island == "Nijhum Dweep"] = quantile(b,0.05)
ty$cir[ty$island == "Nijhum Dweep"] = quantile(b,0.95)


a = numeric(1000)
b = numeric(1000)
a1 = data$worms[data$island == "Damar Char"]
b1 = data$worms[data$island == "Nijhum Dweep"]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
}

t1$mn[t1$island == "Damar Char"] = quantile(a,0.5)
t1$cil[t1$island == "Damar Char"] = quantile(a,0.05)
t1$cir[t1$island == "Damar Char"] = quantile(a,0.95)

t1$mn[t1$island == "Nijhum Dweep"] = quantile(b,0.5)
t1$cil[t1$island == "Nijhum Dweep"] = quantile(b,0.05)
t1$cir[t1$island == "Nijhum Dweep"] = quantile(b,0.95)

ta = rbind(t1,ty)
tb = rbind(t3,t4)
tc = rbind(t2,tx)


ggp = ggplot(data = ta, aes(x = island, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.2) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Island") +
  ylab(expression(paste(Individuals~per~0.1~m^3)))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20, angle = 90), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 30.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = tb, aes(x = island, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.2) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Island") +
  ylab("Richness\n(Species per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(4,6,8,10,12), labels = c("      4","6","8","10","12")) +
  theme(strip.text.x = element_blank()) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")


ggp = ggplot(data = tc, aes(x = island, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.2) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab("Island") +
  ylab("Density\n(Individuals per Segment)")+
  theme_tufte_revised()


ggp2 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "none")



require(cowplot)
g1 = plot_grid(ggp2,ggp1,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

png('Fig. 31.jpg', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()

#######################################
## by season (proxy is replicate)


data = data_rep %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data1 = data %>% distinct() %>% filter(pref == "prefers other foods")
data1$food.abund = data1$food.abund-data1$worms
data = data %>% distinct() %>% filter(pref == "prefers worms")

t1 = data %>% group_by(island,replicate.no) %>% summarize(mn = mean(worms), cil = mean(worms) - 1.96*sd(worms),
                                             cir = mean(worms) + 1.96*sd(worms))


t1$type = "Polychaete Density"

a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data$abund.wo[data$island == "Damar Char" & data$replicate.no == 1]
b1 = data$abund.wo[data$island == "Nijhum Dweep" & data$replicate.no == 1]
c1 = data$abund.wo[data$island == "Damar Char" & data$replicate.no == 2]
d1 = data$abund.wo[data$island == "Nijhum Dweep" & data$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

t2 = t1
t2$type = "Shorebirds that Prefer Polychaetes"
t2$mn[t2$island == "Damar Char" & t2$replicate.no == 1] = quantile(a,0.5)
t2$cil[t2$island == "Damar Char" & t2$replicate.no == 1] = quantile(a,0.05)
t2$cir[t2$island == "Damar Char" & t2$replicate.no == 1] = quantile(a,0.95)

t2$mn[t2$island == "Nijhum Dweep" & t2$replicate.no == 1] = quantile(b,0.5)
t2$cil[t2$island == "Nijhum Dweep" & t2$replicate.no == 1] = quantile(b,0.05)
t2$cir[t2$island == "Nijhum Dweep" & t2$replicate.no == 1] = quantile(b,0.95)

t2$mn[t2$island == "Damar Char" & t2$replicate.no == 2] = quantile(c,0.5)
t2$cil[t2$island == "Damar Char" & t2$replicate.no == 2] = quantile(c,0.05)
t2$cir[t2$island == "Damar Char" & t2$replicate.no == 2] = quantile(c,0.95)

t2$mn[t2$island == "Nijhum Dweep" & t2$replicate.no == 2] = quantile(d,0.5)
t2$cil[t2$island == "Nijhum Dweep" & t2$replicate.no == 2] = quantile(d,0.05)
t2$cir[t2$island == "Nijhum Dweep" & t2$replicate.no == 2] = quantile(d,0.95)

a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data1$food.abund[data1$island == "Damar Char" & data1$replicate.no == 1]
b1 = data1$food.abund[data1$island == "Nijhum Dweep" & data1$replicate.no == 1]
c1 = data1$food.abund[data1$island == "Damar Char" & data1$replicate.no == 2]
d1 = data1$food.abund[data1$island == "Nijhum Dweep" & data1$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

tx = t1
tx$type = "Other Prey Density"
tx$mn[tx$island == "Damar Char" & tx$replicate.no == 1] = quantile(a,0.5)
tx$cil[tx$island == "Damar Char" & tx$replicate.no == 1] = quantile(a,0.05)
tx$cir[tx$island == "Damar Char" & tx$replicate.no == 1] = quantile(a,0.95)

tx$mn[tx$island == "Nijhum Dweep" & tx$replicate.no == 1] = quantile(b,0.5)
tx$cil[tx$island == "Nijhum Dweep" & tx$replicate.no == 1] = quantile(b,0.05)
tx$cir[tx$island == "Nijhum Dweep" & tx$replicate.no == 1] = quantile(b,0.95)

tx$mn[tx$island == "Damar Char" & tx$replicate.no == 2] = quantile(c,0.5)
tx$cil[tx$island == "Damar Char" & tx$replicate.no == 2] = quantile(c,0.05)
tx$cir[tx$island == "Damar Char" & tx$replicate.no == 2] = quantile(c,0.95)

tx$mn[tx$island == "Nijhum Dweep" & tx$replicate.no == 2] = quantile(d,0.5)
tx$cil[tx$island == "Nijhum Dweep" & tx$replicate.no == 2] = quantile(d,0.05)
tx$cir[tx$island == "Nijhum Dweep" & tx$replicate.no == 2] = quantile(d,0.95)


a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data$rich.wo[data$island == "Damar Char" & data$replicate.no == 1]
b1 = data$rich.wo[data$island == "Nijhum Dweep" & data$replicate.no == 1]
c1 = data$rich.wo[data$island == "Damar Char" & data$replicate.no == 2]
d1 = data$rich.wo[data$island == "Nijhum Dweep" & data$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

t3 = t1
t3$type = "Shorebirds that Prefer Polychaetes"
t3$mn[t3$island == "Damar Char" & t3$replicate.no == 1] = quantile(a,0.5)
t3$cil[t3$island == "Damar Char" & t3$replicate.no == 1] = quantile(a,0.05)
t3$cir[t3$island == "Damar Char" & t3$replicate.no == 1] = quantile(a,0.95)

t3$mn[t3$island == "Nijhum Dweep" & t3$replicate.no == 1] = quantile(b,0.5)
t3$cil[t3$island == "Nijhum Dweep" & t3$replicate.no == 1] = quantile(b,0.05)
t3$cir[t3$island == "Nijhum Dweep" & t3$replicate.no == 1] = quantile(b,0.95)

t3$mn[t3$island == "Damar Char" & t3$replicate.no == 2] = quantile(c,0.5)
t3$cil[t3$island == "Damar Char" & t3$replicate.no == 2] = quantile(c,0.05)
t3$cir[t3$island == "Damar Char" & t3$replicate.no == 2] = quantile(c,0.95)

t3$mn[t3$island == "Nijhum Dweep" & t3$replicate.no == 2] = quantile(d,0.5)
t3$cil[t3$island == "Nijhum Dweep" & t3$replicate.no == 2] = quantile(d,0.05)
t3$cir[t3$island == "Nijhum Dweep" & t3$replicate.no == 2] = quantile(d,0.95)

a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data1$rich.wo[data1$island == "Damar Char" & data1$replicate.no == 1]
b1 = data1$rich.wo[data1$island == "Nijhum Dweep" & data1$replicate.no == 1]
c1 = data1$rich.wo[data1$island == "Damar Char" & data1$replicate.no == 2]
d1 = data1$rich.wo[data1$island == "Nijhum Dweep" & data1$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

t4 = t1
t4$type = "Shorebirds that Prefer Other Prey"
t4$mn[t4$island == "Damar Char" & t4$replicate.no == 1] = quantile(a,0.5)
t4$cil[t4$island == "Damar Char" & t4$replicate.no == 1] = quantile(a,0.05)
t4$cir[t4$island == "Damar Char" & t4$replicate.no == 1] = quantile(a,0.95)

t4$mn[t4$island == "Nijhum Dweep" & t4$replicate.no == 1] = quantile(b,0.5)
t4$cil[t4$island == "Nijhum Dweep" & t4$replicate.no == 1] = quantile(b,0.05)
t4$cir[t4$island == "Nijhum Dweep" & t4$replicate.no == 1] = quantile(b,0.95)

t4$mn[t4$island == "Damar Char" & t4$replicate.no == 2] = quantile(c,0.5)
t4$cil[t4$island == "Damar Char" & t4$replicate.no == 2] = quantile(c,0.05)
t4$cir[t4$island == "Damar Char" & t4$replicate.no == 2] = quantile(c,0.95)

t4$mn[t4$island == "Nijhum Dweep" & t4$replicate.no == 2] = quantile(d,0.5)
t4$cil[t4$island == "Nijhum Dweep" & t4$replicate.no == 2] = quantile(d,0.05)
t4$cir[t4$island == "Nijhum Dweep" & t4$replicate.no == 2] = quantile(d,0.95)

a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data1$abund.wo[data1$island == "Damar Char" & data1$replicate.no == 1]
b1 = data1$abund.wo[data1$island == "Nijhum Dweep" & data1$replicate.no == 1]
c1 = data1$abund.wo[data1$island == "Damar Char" & data1$replicate.no == 2]
d1 = data1$abund.wo[data1$island == "Nijhum Dweep" & data1$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

ty = t1
ty$type = "Shorebirds that Prefer Other Prey"
ty$mn[ty$island == "Damar Char" & ty$replicate.no == 1] = quantile(a,0.5)
ty$cil[ty$island == "Damar Char" & ty$replicate.no == 1] = quantile(a,0.05)
ty$cir[ty$island == "Damar Char" & ty$replicate.no == 1] = quantile(a,0.95)

ty$mn[ty$island == "Nijhum Dweep" & ty$replicate.no == 1] = quantile(b,0.5)
ty$cil[ty$island == "Nijhum Dweep" & ty$replicate.no == 1] = quantile(b,0.05)
ty$cir[ty$island == "Nijhum Dweep" & ty$replicate.no == 1] = quantile(b,0.95)

ty$mn[ty$island == "Damar Char" & ty$replicate.no == 2] = quantile(c,0.5)
ty$cil[ty$island == "Damar Char" & ty$replicate.no == 2] = quantile(c,0.05)
ty$cir[ty$island == "Damar Char" & ty$replicate.no == 2] = quantile(c,0.95)

ty$mn[ty$island == "Nijhum Dweep" & ty$replicate.no == 2] = quantile(d,0.5)
ty$cil[ty$island == "Nijhum Dweep" & ty$replicate.no == 2] = quantile(d,0.05)
ty$cir[ty$island == "Nijhum Dweep" & ty$replicate.no == 2] = quantile(d,0.95)


a = numeric(1000)
b = numeric(1000)
c = numeric(1000)
d = numeric(1000)
a1 = data$worms[data$island == "Damar Char" & data$replicate.no == 1]
b1 = data$worms[data$island == "Nijhum Dweep" & data$replicate.no == 1]
c1 = data$worms[data$island == "Damar Char" & data$replicate.no == 2]
d1 = data$worms[data$island == "Nijhum Dweep" & data$replicate.no == 2]
for (i in 1:1000)
{
  a[i] = mean(sample(a1, replace = T))
  b[i] = mean(sample(b1, replace = T))
  c[i] = mean(sample(c1, replace = T))
  d[i] = mean(sample(d1, replace = T))
}

t1$mn[t1$island == "Damar Char" & t1$replicate.no == 1] = quantile(a,0.5)
t1$cil[t1$island == "Damar Char" & t1$replicate.no == 1] = quantile(a,0.05)
t1$cir[t1$island == "Damar Char" & t1$replicate.no == 1] = quantile(a,0.95)

t1$mn[t1$island == "Nijhum Dweep" & t1$replicate.no == 1] = quantile(b,0.5)
t1$cil[t1$island == "Nijhum Dweep" & t1$replicate.no == 1] = quantile(b,0.05)
t1$cir[t1$island == "Nijhum Dweep" & t1$replicate.no == 1] = quantile(b,0.95)

t1$mn[t1$island == "Damar Char" & t1$replicate.no == 2] = quantile(c,0.5)
t1$cil[t1$island == "Damar Char" & t1$replicate.no == 2] = quantile(c,0.05)
t1$cir[t1$island == "Damar Char" & t1$replicate.no == 2] = quantile(c,0.95)

t1$mn[t1$island == "Nijhum Dweep" & t1$replicate.no == 2] = quantile(d,0.5)
t1$cil[t1$island == "Nijhum Dweep" & t1$replicate.no == 2] = quantile(d,0.05)
t1$cir[t1$island == "Nijhum Dweep" & t1$replicate.no == 2] = quantile(d,0.95)

ta = rbind(t1,tx)
tb = rbind(t3,t4)
tc = rbind(t2,ty)

ta$type = factor(ta$type, levels = c("Polychaete Density","Other Prey Density"))
tb$type = factor(tb$type, levels = c("Shorebirds that Prefer Polychaetes","Shorebirds that Prefer Other Prey"))
tc$type = factor(tc$type, levels = c("Shorebirds that Prefer Polychaetes","Shorebirds that Prefer Other Prey"))

pd = position_dodge(0.3)
ggp = ggplot(data = ta, aes(x = replicate.no, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 3, position = pd) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.1, position = pd) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Season") +
  ylab(expression(paste(Individuals~per~m^2)))+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20, angle = 90), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(1,2), labels = c("Dec-Jan","Feb-Mar")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=31","Nijhum Dweep N=25")) +
  theme(legend.position = "bottom")

png('Fig. 32.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

ggp = ggplot(data = tb, aes(x = replicate.no, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.2, position = pd) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab("Season") +
  ylab("Richness\n(Species per Segment)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 22),
        axis.title.y = element_text(size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(1,2), labels = c("Dec-Jan","Feb-Mar")) +
  scale_y_continuous(breaks = c(4,6,8,10), labels = c("   4","6","8","10")) +
  theme(strip.text.x = element_blank()) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=31","Nijhum Dweep N=25")) +
  theme(legend.position = "bottom")

ggp = ggplot(data = tc, aes(x = replicate.no, y = mn, col = island)) +
  facet_wrap(.~type, nrow = 1, ncol = 2, scales = "free_x") +
  geom_point(size = 4, position = pd) +
  geom_errorbar(aes(ymin = cil,ymax = cir), size = 1, width = 0.2, position = pd) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 6))) +
  xlab("Season") +
  ylab("Density\n(Individuals per Segment)")+
  theme_tufte_revised()


ggp2 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 24), axis.text.y = element_text(size = 22)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(1,2), labels = c("Dec-Jan","Feb-Mar")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "none")

require(cowplot)
g1 = plot_grid(ggp2,ggp1,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

png('Fig. 33.jpg', units="in", width=10, height=10, res=1000)
grid::grid.draw(g1)
dev.off()


##### worms vs other foods

data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% select(-pref,-abund.wo,-rich.wo) %>% distinct()

data$otherfood = data$food.abund - data$worms


ggp = ggplot(data = data, aes(x = worms, y = otherfood, col = island)) +
  geom_point(size = 3) +
  #geom_smooth(method = "lm", se = F) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab(expression(paste(Polychaete~Density~(per~m^2~surface~and~10~cm~depth)))) +
  ylab(expression(paste(Other~Prey~Density~(Individuals~per~m^2)))) +
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs, breaks = c("Damar Char","Nijhum Dweep"), 
                      labels = c("Damar Char N=16","Nijhum Dweep N=14")) +
  theme(legend.position = "bottom")

png('Fig. 34.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

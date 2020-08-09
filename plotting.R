library(tidyverse)
library(ggthemes)

data_rep = read.csv("data_rep.csv")
forage_data_rep = read.csv("forage_data_rep.csv")
data_seg = read.csv("data_seg.csv")
forage_data_seg = read.csv("forage_data_seg.csv")

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

data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% distinct()
data = data[data$total.abund<2000,]
data$food.others = data$food.abund-data$worms
data1 = data
data1$food.type = "all foods"
data1$food.abund.new = data1$food.abund
data2 = data
data2$food.type = "worms"
data2$food.abund.new = data2$worms
data3 = data
data3$food.type = "other foods"
data3$food.abund.new = data3$food.others
data = rbind(data1,data2,data3)
data$food.type = factor(data$food.type,levels = c("all foods","worms","other foods"))


ggp = ggplot(data = data, aes(x = food.abund.new, y = total.abund)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlab("food abundance") +
  ylab("shorebird abundance")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "bottom")

png('Fig. 1.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = food.abund.new, y = rich)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlab("food abundance") +
  ylab("shorebird richness")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "bottom")

png('Fig. 2.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


ggp = ggplot(data = data, aes(x = food.abund.new, y = total.div)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free_x") +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlab("food abundance") +
  ylab("shorebird diversity")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "bottom")

png('Fig. 3.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()



ggp = ggplot(data = data, aes(x = temp, y = food.abund.new)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free_y") +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlab("temperature") +
  ylab("food abundance")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "bottom")

png('Fig. 4.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()



ggp = ggplot(data = data, aes(x = dryweight, y = food.abund.new)) +
  facet_wrap(.~food.type, nrow = 3, ncol = 1, scales = "free_y") +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  xlab("soil density") +
  ylab("food abundance")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "bottom")

png('Fig. 5.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()


fit = glm(data = data, food.abund~temp+dryweight, family=poisson)
summary(fit)

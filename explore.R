data_rep = read.csv("data_rep.csv")
forage_data_rep = read.csv("forage_data_rep.csv")
data_seg = read.csv("data_seg.csv")
forage_data_seg = read.csv("forage_data_seg.csv")


############################### community analyses

library(tidyverse)
LSP = data_seg %>% filter(obs.sp == "Lesser Sand Plover")
data = data_seg %>%
  select(-c(obs.sp,high.count,total.count,prop.foraging,prop.preening,prop.locomotion,prop.resting,prop.alert))
data = data %>% distinct()
lsp1 = LSP %>% select(segment,total.count)
data = left_join(data,lsp1) 

data = data[data$total.abund<2000,]
data$food.others = data$food.abund-data$worms
data$abund.others = data$total.abund-data$total.count

with(data,plot(total.count~worms))
summary(lm(data$total.count~data$worms))
abline(lm(data$total.count~data$worms))

with(data,plot(abund.others~worms))
summary(lm(data$abund.others~data$worms))
abline(lm(data$abund.others~data$worms))

with(data,plot(total.abund~dryweight))
summary(lm(data$total.abund~data$dryweight))
abline(lm(data$total.abund~data$dryweight))

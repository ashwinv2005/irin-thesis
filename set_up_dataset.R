library(tidyverse)
library(vegan)

scan = read.csv("scan.csv")
sediment = read.csv("sediment_weight.csv")

specs = unique(scan$obs.sp)
specs = specs[specs != 0]

t = scan %>%
  distinct(date,segment)

scantemp1 = t %>% slice(rep(1:n(), each = 6))
scantemp1$scan.no = rep(1:6,length(t[,1]))
scantemp = scantemp1 %>% slice(rep(1:n(), each = length(specs)))
scantemp$obs.sp = rep(specs,length(scantemp1[,1]))

t = t %>%
  group_by(segment) %>% mutate(replicate.no = row_number())

scan = left_join(scantemp,scan)
scan = left_join(scan,t)
scan = scan %>% select(segment,replicate.no,scan.no,obs.sp,obs.no,foraging,preening,locomotion,resting,alert)
scan[is.na(scan)] = 0

scan_scan = scan

scan_rep = scan %>%
  group_by(segment,replicate.no,obs.sp) %>% mutate(total = sum(obs.no)) %>%
  mutate(total = ifelse(total == 0, 100000, total)) %>%
  group_by(segment,replicate.no,obs.sp) %>% summarize(high.count = max(obs.no),total.count = sum(obs.no), 
                                                   prop.foraging = sum(foraging)/max(total),
                                                   prop.preening = sum(preening)/max(total),
                                                   prop.locomotion = sum(locomotion)/max(total),
                                                   prop.resting = sum(resting)/max(total),
                                                   prop.alert = sum(alert)/max(total))

temp1 = scan_rep %>% select(segment,replicate.no,obs.sp,high.count)
temp2 = scan_rep %>% select(segment,replicate.no,obs.sp,total.count)

scan_diversity_high_rep = spread(temp1,obs.sp,high.count)
scan_diversity_total_rep = spread(temp2,obs.sp,total.count)

scan_seg = scan %>%
  group_by(segment,obs.sp) %>% mutate(total = sum(obs.no)) %>%
  mutate(total = ifelse(total == 0, 100000, total)) %>%
  group_by(segment,obs.sp) %>% summarize(high.count = max(obs.no),total.count = sum(obs.no), 
                                                      prop.foraging = sum(foraging)/max(total),
                                                      prop.preening = sum(preening)/max(total),
                                                      prop.locomotion = sum(locomotion)/max(total),
                                                      prop.resting = sum(resting)/max(total),
                                                      prop.alert = sum(alert)/max(total),
                                         n = n_distinct(replicate.no))
scan_seg$total.count = round(scan_seg$total.count/scan_seg$n)

temp1 = scan_seg %>% select(segment,n,obs.sp,high.count)
temp2 = scan_seg %>% select(segment,n,obs.sp,total.count)

scan_diversity_high_seg = spread(temp1,obs.sp,high.count)
scan_diversity_total_seg = spread(temp2,obs.sp,total.count)


scan_diversity_high_rep$high.div = diversity(scan_diversity_high_rep[,-c(1:2)], 
                                        index = "shannon", MARGIN = 1, base = exp(1))
scan_diversity_high_rep$high.rich = specnumber(scan_diversity_high_rep[,-c(1:2,34)],MARGIN = 1)
scan_diversity_high_rep = scan_diversity_high_rep %>% ungroup %>% mutate(high.abund = rowSums(.[3:33]))
scan_diversity_high_rep = scan_diversity_high_rep %>%  select(1,2,34:36)
scan_rep = left_join(scan_rep,scan_diversity_high_rep)
names(scan_rep)[12] = "rich"

scan_diversity_total_rep$total.div = diversity(scan_diversity_total_rep[,-c(1:2)], 
                                        index = "shannon", MARGIN = 1, base = exp(1))
scan_diversity_total_rep$total.rich = specnumber(scan_diversity_total_rep[,-c(1:2,34)],MARGIN = 1)
scan_diversity_total_rep = scan_diversity_total_rep %>% ungroup %>% mutate(total.abund = rowSums(.[3:33]))
scan_diversity_total_rep = scan_diversity_total_rep %>%  select(1,2,34,36)
scan_rep = left_join(scan_rep,scan_diversity_total_rep)


scan_diversity_high_seg$high.div = diversity(scan_diversity_high_seg[,-c(1:2)], 
                                        index = "shannon", MARGIN = 1, base = exp(1))
scan_diversity_high_seg$high.rich = specnumber(scan_diversity_high_seg[,-c(1:2,34)],MARGIN = 1)
scan_diversity_high_seg = scan_diversity_high_seg %>% ungroup %>% mutate(high.abund = rowSums(.[3:33]))
scan_diversity_high_seg = scan_diversity_high_seg %>%  select(1,2,34:36)
scan_seg = left_join(scan_seg,scan_diversity_high_seg)
names(scan_seg)[12] = "rich"

scan_diversity_total_seg$total.div = diversity(scan_diversity_total_seg[,-c(1:2)], 
                                         index = "shannon", MARGIN = 1, base = exp(1))
scan_diversity_total_seg$total.rich = specnumber(scan_diversity_total_seg[,-c(1:2,34)],MARGIN = 1)
scan_diversity_total_seg = scan_diversity_total_seg %>% ungroup %>% mutate(total.abund = rowSums(.[3:33]))
scan_diversity_total_seg = scan_diversity_total_seg %>%  select(1,2,34:36)
scan_seg = left_join(scan_seg,scan_diversity_total_seg)





########################################## food

food = read.csv("food.csv")
t = food %>%
  distinct(date,segment)
t = t %>%
  group_by(segment) %>% mutate(replicate.no = row_number())

food = left_join(food,t)
food_rep = food %>%
  group_by(segment,replicate.no) %>% summarize(worms = sum(worms), crabs = sum(crabs),mollusks = sum(mollusks),
                                               fishfry = sum(fishfry))
food_seg = food %>% mutate(n = n_distinct(replicate.no)) %>%
  group_by(segment) %>% summarize(worms = round(sum(worms)/max(n)), crabs = round(sum(crabs)/max(n)),
                                  mollusks = round(sum(mollusks)/max(n)),fishfry = round(sum(fishfry)/max(n)))


food_rep$food.div = diversity(food_rep[,-c(1:2)], 
                         index = "shannon", MARGIN = 1, base = exp(1))
food_rep$food.rich = specnumber(food_rep[,-c(1:2,7)],MARGIN = 1)
food_rep = food_rep %>% ungroup %>% mutate(food.abund = rowSums(.[3:6]))


food_seg$food.div = diversity(food_seg[,-c(1)], 
                         index = "shannon", MARGIN = 1, base = exp(1))
food_seg$food.rich = specnumber(food_seg[,-c(1,6)],MARGIN = 1)
food_seg = food_seg %>% ungroup %>% mutate(food.abund = rowSums(.[2:5]))






####################### forage

forage = read.csv("forage.csv")
food = read.csv("food.csv")

t = food %>%
  distinct(date,segment)
t = t %>%
  group_by(segment) %>% mutate(replicate.no = row_number())

forage = left_join(forage,t)
forage = forage %>% select(segment,replicate.no,obs.sp,total_attempts,success_attempts,
                           duration,worms1,crabs1,fishfry1,mollusks1)
forage[is.na(forage)] = 0
forage$success_prop = forage$success_attempts/forage$total_attempts
forage$success_prop[is.na(forage$success_prop)] = NA






###################### soil temp

soiltemp = read.csv("soiltemp.csv")
food = read.csv("food.csv")

t = food %>%
  distinct(date,segment)
t = t %>%
  group_by(segment) %>% mutate(replicate.no = row_number())

soiltemp = left_join(soiltemp,t)

temp_rep = soiltemp %>% group_by(segment,replicate.no) %>%
  summarize(temp = mean(temp))

temp_seg = soiltemp %>% group_by(segment) %>%
  summarize(temp = mean(temp))







###################### sediment weight

sediment = read.csv("sediment_weight.csv")
sediment = sediment %>% select(segment,dryweight)





###################### create replicate level data frames

#library(stringr)

data_rep = left_join(food_rep,scan_rep)
data_rep = left_join(data_rep,temp_rep)
#names(data_rep_high) = str_replace_all(names(data_rep_high), c(" " = "." , "," = "" ))

forage_data_rep = left_join(forage,data_rep)



###################### create segment level data frames

#library(stringr)

data_seg = left_join(food_seg,scan_seg)
data_seg = left_join(data_seg,temp_seg)
data_seg = left_join(data_seg,sediment)
#names(data_seg_high) = str_replace_all(names(data_seg_high), c(" " = "." , "," = "" ))

forage_data_seg = left_join(forage,data_seg)

write.csv(data_rep,"data_rep.csv",row.names = F)
write.csv(forage_data_rep,"forage_data_rep.csv",row.names = F)
write.csv(data_seg,"data_seg.csv",row.names = F)
write.csv(forage_data_seg,"forage_data_seg.csv",row.names = F)

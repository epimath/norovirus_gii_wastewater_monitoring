library(tidyverse)
library(lubridate)
library(reshape2)
library(openxlsx)
library(ggpubr)

data_path <- "C:/Users/juliegil/Dropbox (University of Michigan)/SPH-COVID Response/Jules/norovirus_ww_crosscorr"

detroit_noro <- read.csv(paste0(data_path, "/norovirusDetroit20210101-20221129.csv"), skip = 1)
colnames(detroit_noro) <- c("Week", "Noro")
detroit_sf <- read.csv(paste0(data_path, "/stomach fluDetroit20210101-20221129.csv"), skip = 1)
colnames(detroit_sf) <- c("Week", "SF")
detroit_ge <- read.csv(paste0(data_path, "/gastroenteritisDetroit20210101-20221129.csv"), skip = 1)
colnames(detroit_ge) <- c("Week", "GE")

mi_noro <- read.csv(paste0(data_path, "/norovirusMichigan20210101-20221129.csv"), skip = 1)
colnames(mi_noro) <- c("Week", "Noro")
mi_sf <- read.csv(paste0(data_path, "/stomach fluMichigan20210101-20221129.csv"), skip = 1)
colnames(mi_sf) <- c("Week", "SF")
mi_ge <- read.csv(paste0(data_path, "/gastroenteritisMichigan20210101-20221129.csv"), skip = 1)
colnames(mi_ge) <- c("Week", "GE")

full_norosf_set <- merge(mi_noro, mi_sf, by = c("Week"), all = TRUE)
colnames(full_norosf_set) <- c("Week", "MI_Noro", "MI_SF")
full_norosf_set <- merge(full_norosf_set, detroit_noro, by = c("Week"), all = TRUE)
colnames(full_norosf_set) <- c("Week", "MI_Noro", "MI_SF", "DET_Noro")
full_norosf_set <- merge(full_norosf_set, detroit_sf, by = c("Week"), all = TRUE)
colnames(full_norosf_set) <- c("Week", "MI_Noro", "MI_SF", "DET_Noro", "DET_SF")

full_norosf_set <- merge(full_norosf_set, detroit_ge, by = c("Week"), all = TRUE)
colnames(full_norosf_set) <- c("Week", "MI_Noro", "MI_SF", "DET_Noro", "DET_SF", "DET_GE")
full_norosf_set <- merge(full_norosf_set, mi_ge, by = c("Week"), all = TRUE)
colnames(full_norosf_set) <- c("Week", "MI_Noro", "MI_SF", "DET_Noro", "DET_SF", "DET_GE", "MI_GE")

#####

ww_noro <- read.csv(paste0(data_path, "/norov_wastewater_data_all_cities.csv"))
#colnames(ww_noro)
ww_noro <- ww_noro %>% select(Date, City, Sample, NorovirusAvgConc, NVnormalizedPMMoV)
#table(ww_noro$SampleType)
# remove days with more than one sample
a <- ww_noro %>% group_by(Date, City) %>% summarize(count = length(Sample))
ww_noro <- merge(ww_noro, a, by = c("Date", "City"))
ww_noro <- filter(ww_noro, count <= 1)

# have to reshape data to compare cities
ww_noro2 <- ww_noro %>% select(Date, City, Sample, NorovirusAvgConc, NVnormalizedPMMoV)
ww_melt <- melt(ww_noro2, id.vars = c("Date", "Sample", "City"))
full_ww_set <- dcast(ww_melt, Date ~ City + variable, value.var = c("value"))


######

# all_data <- merge(full_ww_set, full_norosf_set, by.x = c("Date"), by.y = c("Week"), all = TRUE)
# # forward fill the search data
# 
# all_data <- all_data %>% fill(c(MI_Noro, MI_SF, MI_GE, DET_Noro, DET_SF, DET_GE), .direction = "down")

one_set <- full_ww_set %>% select(Date, YC_NorovirusAvgConc, YC_NVnormalizedPMMoV)
trend_set <- full_norosf_set %>% select(Week, MI_SF)
trend_set <- trend_set %>% mutate(Week = as_date(Week) - 7)

one_set <- one_set %>% mutate(Date = as_date(Date))

all_data <- merge(one_set, trend_set, by.x = c("Date"), by.y = c("Week"), all = TRUE)

all_data <- all_data %>% fill(c(MI_SF), .direction = "down")

cor(all_data$YC_NVnormalizedPMMoV, all_data$MI_SF, use = "complete.obs")


ggplot(all_data, aes(x = YC_NVnormalizedPMMoV, y = MI_SF)) + 
  geom_point(alpha = 0.7) + 
  theme_bw() + 
  labs(title = "Site = YC", 
       x = "PMMoV-Normalized Wastewater Values", 
       y = "Google Trends - Michigan - 'Stomach Flu'")


#####

one_set <- full_ww_set %>% select(Date, YC_NorovirusAvgConc, YC_NVnormalizedPMMoV)
trend_set <- full_norosf_set %>% select(Week, MI_SF, MI_Noro)

one_set <- one_set %>% mutate(Date = as_date(Date))
trend_set <- trend_set %>% mutate(Week = as_date(Week))

all_data <- merge(one_set, trend_set, by.x = c("Date"), by.y = c("Week"), all = TRUE)

all_data <- all_data %>% fill(c(MI_SF), .direction = "down")

cor(all_data$YC_NVnormalizedPMMoV, all_data$MI_SF, use = "complete.obs", method = "pearson")
cor(all_data$YC_NVnormalizedPMMoV, all_data$MI_SF, use = "complete.obs", method = "spearman")
cor(all_data$YC_NVnormalizedPMMoV, all_data$MI_Noro, use = "complete.obs", method = "pearson")
cor(all_data$YC_NVnormalizedPMMoV, all_data$MI_Noro, use = "complete.obs", method = "spearman")



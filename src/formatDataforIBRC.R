library(tidyverse)
library(readr)
library(dplyr)

USCAresults <- read.csv(here::here("data/USresults-survey718871_orig.csv")) %>% 
  filter(
    #cntry=="US",
         # sppCode %in% c('ANPAL', 'MYSEP', 'MYLUC', 'PESUB', 'EPFUS', 'COTOW', 'EUFLO', 'LASEH', 'LENIV', 'MYSOD', 'TABRA'),
         !is.na(submitdate)
         ) %>% 
  group_by(sppCode, token) %>% 
  mutate(endPage=max(lastpage)) %>% 
  filter(endPage==lastpage) %>% 
  select(-endPage) %>% 
  distinct() %>% 
  as.data.frame()
  

table(USresults$spp)

USCAresults %>% group_by(sppCode, token) %>% summarise(n = n()) %>% filter(n > 1)


# write_csv(USCAresults, here::here("data/US_CA_survey718871_25July2022.csv"))

MXresults <- read_csv(here::here("data/MX_results_20211109.csv"))

MXresults %>% group_by(sppCode, token) %>% summarise(n = n()) %>% filter(n > 1)


MXresults2 <- MXresults %>% 
  filter(cntry=="MX",
         # sppCode %in% c('ANPAL', 'MYSEP', 'MYLUC', 'PESUB', 'EPFUS', 'COTOW', 'EUFLO', 'LASEH', 'LENIV', 'MYSOD', 'TABRA'),
         !is.na(submitdate)
  ) %>% 
  group_by(sppCode, token) %>% 
  mutate(maxDate=max(submitdate)) %>% 
  filter(submitdate == maxDate) %>% 
  select(-maxDate) %>% 
  distinct() %>% 
  as.data.frame()

MXresults2 %>% group_by(sppCode, token) %>% summarise(n = n()) %>% filter(n > 1)

# write_csv(MXresults2, here::here("data/MX_results_2022July25.csv"))

surveyNames_US <- names(USresults)
surveyNames_MX <- names(MXresults2)

setdiff(surveyNames_US, surveyNames_MX)


### Format US data for analysis ###

USresults <- read_csv(here::here("data/results-survey718871_20220721.csv"))
threatNum <- read_csv(here::here("data/ThreatNum.csv"))

USlong <- USresults %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = `popSize.sz_min.`:`otherSev.user10_conf.`,
               names_to = "category") %>% 
  dplyr::select(id, token, sppCode, category, value)


USmeans <- USlong %>% 
  separate(category, into = c("category", "statistic"), sep = "_") %>% 
  separate(category, into = c("category", "subcategory"), sep = "\\.") %>% 
  mutate(metric = case_when(
    grepl("Scope", category) ~ "scope",
    grepl("Sev", category) ~ "severity"
  ),
  category = gsub("Scope", "", category),
  category = gsub("Sev", "", category)) %>% 
  filter(statistic == "mean.")

threats <- unique(threatNum$Q_group)


USsummary <- USmeans %>% 
  group_by(sppCode, category, subcategory) %>% 
  mutate(value = as.numeric(value)) %>% 
  dplyr::summarise(meanValue = mean(value, na.rm = T), 
            n = n(),
            sd = sd(value, na.rm = T)) %>% 
  left_join(threatNum, by = c("category" = "Q_group", "subcategory" = "Q_sub")) 

USsummary %>% 
  filter(!is.na(Threat)) %>% 
  ggplot(aes(x = Threat, y = meanValue, color = sppCode)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

USpca <- USsummary %>% 
  filter(!is.na(Threat)) %>% 
  mutate(meanValue = as.numeric(meanValue)) %>% 
  pivot_wider(id_cols = c("sppCode"), names_from = c(category, subcategory), values_from = meanValue) %>% 
  ungroup() %>% 
  dplyr::select(-sppCode) %>% 
  mutate_all(funs(replace_na(.,0))) %>% 
  as.data.frame()
row.names(USpca) <- unique(USsummary$sppCode)


USpcaResults <- prcomp(USpca)
USpcaResults$rotation <- -1*USpcaResults$rotation
USpcaResults$rotation
USpcaResults$x <- -1*USpcaResults$x
head(USpcaResults$x)
biplot(USpcaResults)





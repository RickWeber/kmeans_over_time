#!/usr/bin/R
#### setup libraries ####
library(tidyverse)
library(readxl)
library(countrycode)
library(viridisLite)

#### import data ####
efw <- read_excel(
    path = "efw-2019-master-index-data-for-researchers.xlsx",
    sheet = "EFW Panel Data 2019 Report")

# tidy up column names
colnames(efw) <- c("year","iso3c","country",
                   "EFW","EFW1","EFW2",
                   "EFW3","EFW4","EFW5")

# calculate EFW quartiles
efw <- efw %>% 
    group_by(year) %>% 
    within(quartile <- as.integer(cut(EFW,quantile(EFW, 
                                                   probs=0:4/4,
                                                   include.lowest=T,
                                                   na.rm=T)))) %>% 
    ungroup() %>% 
    mutate(year = as.integer(year)) %>%
    mutate(quartile = -quartile + 5,
           quartile =  as.integer(quartile))

# scale data before running clustering algorithm
efw_scaled <- efw %>% 
    group_by(year) %>% 
    mutate_if(is.double,scale) %>% 
    ungroup()
## colnames(efw_scaled) <- c("year","iso3c","country",
##                           "EFW_scaled","EFW1_scaled",
##                           "EFW2_scaled","EFW3_scaled",
##                           "EFW4_scaled","EFW5_scaled")

# years
years <- sort(unique(efw$year),decreasing = TRUE)

# map data
map_coords <- map_data("world") %>%
    as_tibble() %>%
    mutate(iso3c = countrycode(region,"country.name","iso3c"))

#### Functions ####

k_continuous <- function(efw_data = efw_scaled, yrs =  years, k = 4){
    yrs <- yrs %>% sort(decreasing = TRUE)
    cdr_yrs <- yrs[2:length(yrs)]
    first_df <- efw_data %>%
        filter(year == yrs[1],complete.cases(.))
    first_k <- first_df %>%
        select(contains("EFW")) %>%
        select(-EFW) %>%
        kmeans(k,nstart = 10)
    cent <- first_k$centers
    out_df <- cbind(first_df,cl = first_k$cluster) %>% as_tibble()
    for(y in cdr_yrs){
        print(y)
        next_df <- efw_data %>% filter(year == y,complete.cases(.))
        next_k <- next_df %>%
            select(contains("EFW")) %>%
            select(-EFW) %>%
            kmeans(cent,nstart = 10)
        cent <- next_k$centers
        next_df <- cbind(next_df, cl = next_k$cluster)
        out_df <- full_join(out_df,next_df)
    }
    return(out_df)
}


unscaled_k4 <- k_continuous(efw,years[years>1999],4)
## unscaled_k4 %>%
    k_continuous(efw,years,5) %>%
    mutate(country = fct_reorder(country,cl)) %>%
    ggplot(aes(year,country)) +
    geom_tile(aes(fill = as.factor(cl))) +
    theme_minimal() + scale_fill_brewer(type = "qual",
                                        palette = "Set1")

## I'd like the rows ordered by last year's cl then alphabetical...

scaled_k4 <- k_continuous(efw_scaled,k = 4)
scaled_k4 %>%
    mutate(country = fct_reorder(country,cl)) %>%
    ggplot(aes(year,country)) +
    geom_tile(aes(fill = as.factor(cl))) +
    theme_minimal() + scale_fill_brewer(type = "qual",
                                        palette = "Set1")

US_k4 <- k_continuous(efw,years,4)
US_k4 <- US_k4 %>%
    group_by(iso3c) %>%
    mutate(yrs_of_data = n()) %>%
    ## ungroup %>%
    group_by(iso3c,cl) %>%
    mutate(yrs_in_cluster = n()) %>%
    ## ungroup %>%
    group_by(iso3c) %>%
    mutate(cluster_stability = mean(yrs_in_cluster) / yrs_of_data) %>%
    ungroup

US_k4 %>%
    mutate(country = fct_reorder(country,cluster_stability)) %>%
    ggplot(aes(year,country)) +
    geom_tile(aes(fill = as.factor(cl))) +
    theme_minimal() + scale_fill_brewer(type = "qual",
                                        palette = "Set1")

png("cluster_membership_over_time.png",height=2000,width=800)

US_k4 %>%
    mutate(country = fct_reorder(country,cluster_stability)) %>%
    ggplot(aes(year,country)) +
    geom_tile(aes(fill = as.factor(cl))) +
    theme_minimal() + scale_fill_brewer(type = "qual",
                                        palette = "Set1")
dev.off()

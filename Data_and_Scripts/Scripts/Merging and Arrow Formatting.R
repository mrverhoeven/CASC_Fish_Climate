#' Preamble

## libraries

library(arrow)
library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(janitor)
library(tidyr)
library(lubridate)
library(ggplot2)

options(scipen = 999)


wi_flat_effort_indivfish_merge <- readRDS("E:/My Drive/Documents/UMN/Hansen Lab/Rprojects/CASC_Fish_Climate/Data_and_Scripts/Data/output/wi_flat_effort_indivfish_merge.rds")

wi_flat_effort_indivfish_merge <- as_arrow_table(wi_flat_effort_indivfish_merge)


mi_flat_effort_indivfish_merge <- readRDS("E:/My Drive/Documents/UMN/Hansen Lab/Rprojects/CASC_Fish_Climate/Data_and_Scripts/Data/output/mi_flat_effort_indivfish_merge.rds")

mi_flat_effort_indivfish_merge <- as_arrow_table(mi_flat_effort_indivfish_merge)

mn_flat_effort_indivfish_merge <- readRDS("E:/My Drive/Documents/UMN/Hansen Lab/Rprojects/CASC_Fish_Climate/Data_and_Scripts/Data/output/mn_flat_effort_indivfish_merge.rds")

mn_flat_effort_indivfish_merge <- as_arrow_table(mn_flat_effort_indivfish_merge)


mn_flat_effort_indivfish_merge%>%
    summarise(n)

colnames(mn_flat_effort_indivfish_merge)

MNMIWI_obs <- bind_rows(mn_flat_effort_indivfish_merge, wi_flat_effort_indivfish_merge)


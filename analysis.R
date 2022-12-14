rm(list=ls())
library(tidyverse)
library(igraph)
library(visreg)
library(Rcpp)
library(readxl)
library(ggraph)
library(data.table)
library(stringr)
library(ggsci)
library(stargazer)
library(latex2exp)
library(patchwork)

# Loading field data ------------------------------------------------------
d <- read.csv('data/osotua_dataset.csv')
load('data/network_osotua')
source('cleaning.R')
source('plotting-results.R')

# Experimental data analysis ----------------------------------------------

drought_num = sum(d$osotua_condition=='drought', na.rm = TRUE)
bridewealth_num = sum(d$osotua_condition=='bridewealth', na.rm = TRUE)
perc_female = signif(mean(d$gender=='female', na.rm=TRUE)*100, 3)

drought_mean <- signif(mean(d$osotua_shared[d$osotua_condition=='drought'], na.rm=TRUE),2)
bridewealth_mean <- signif(mean(d$osotua_shared[d$osotua_condition=='bridewealth'], na.rm=TRUE),2)
drought_median <- median(d$osotua_shared[d$osotua_condition=='drought'], na.rm=TRUE)
bridewealth_median <- median(d$osotua_shared[d$osotua_condition=='bridewealth'], na.rm=TRUE)
drought_sd <- signif(sd(d$osotua_shared[d$osotua_condition=='drought'], na.rm=TRUE),2)
bridewealth_sd <- signif(sd(d$osotua_shared[d$osotua_condition=='bridewealth'], na.rm=TRUE),2)

amt_shared <- wilcox.test(
  d$osotua_shared[d$osotua_condition=='drought'], 
  d$osotua_shared[d$osotua_condition=='bridewealth']
)

amt_shared_plot <- 
  ggplot(d, aes(x=osotua_shared, fill=osotua_condition)) + 
  geom_bar(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=viridis::magma(11)[c(4,8)]) +
  scale_x_continuous(breaks=0:10) +
  theme_bw() +
  labs(x='\namount shared', y='\ncount', fill='condition')

amt_shared_plot2 <- 
  ggplot(d, aes(x=osotua_shared, fill=osotua_condition)) + 
  geom_bar(width=1, alpha=0.7) +
  scale_fill_manual(values=viridis::magma(11)[c(4,8)]) +
  scale_x_continuous(breaks=0:10) +
  theme_bw() +
  labs(x='\namount shared', y='\ncount', fill='condition')

amt_shared_plot3 <- 
  d %>% 
  filter(!is.na(osotua_shared)) %>% 
  ggplot(aes(x=osotua_shared, linetype=osotua_condition)) + 
  stat_ecdf() +
  scale_x_continuous(breaks=0:10) +
  theme_bw() +
  labs(x='\nAmount shared', y='f(shared)\n', linetype='Condition')

m_amt_shared <- glm(osotua_shared ~ osotua_condition, data=d, family='gaussian')
m_amt_shared2 <- glm(osotua_shared ~ osotua_condition + insecure, data=d, family='gaussian')

b_need <- signif(m_amt_shared2$coefficients,2)[2]
b_insecure <- signif(m_amt_shared2$coefficients,2)[3]

se_need <- signif(summary(m_amt_shared2)$coef[2,2],2)
se_insecure <- signif(summary(m_amt_shared2)$coef[3,2],2)

p_need <- signif(summary(m_amt_shared2)$coef[2,4],2)
p_insecure <- signif(summary(m_amt_shared2)$coef[3,4],2)

b_simple <- signif(m_amt_shared$coefficients,2)[2]
se_simple <- signif(summary(m_amt_shared)$coef[2,2],2)

p_simple <- signif(summary(m_amt_shared)$coef[2,4],2)


# Exploratory analysis ----------------------------------------------------

deg_osotua <- 
  gs %>% 
  igraph::as_data_frame() %>% 
  filter(subnetwork=='osotua') %>% 
  group_by(from) %>% 
  summarise(degree=length(to))

deg_osotua_plot <- ggplot(deg_osotua, aes(x=degree)) + 
  geom_bar() +
  theme_bw() +
  labs(x='\nosotua degree', y='count\n')

mo_insecure <- lm(insecure ~ scale(degree_share), data=d)
mo_insecure2 <- lm(insecure ~ degree_share + I(degree_share^2), data=d)

osotua_insecure <- 
  visreg(mo_insecure, points=NULL, gg=TRUE, partial=FALSE, rug=FALSE) +
  geom_count(alpha=0.6) +
  scale_size(range=c(1,4)) +
  theme_bw() +
  labs(x='\nosotua degree', y='food insecurity\n', size="number of\nparticipants") +
  theme(legend.position = "top")

exp_beta_insecure <- signif(mo_insecure$coefficients[2], 2)
exp_p_insecure <- signif(summary(mo_insecure)$coef[2,4], 2)
exp_se_insecure <- signif(summary(mo_insecure)$coef[2,2], 2)

# exp_beta_insecure <- signif(mo_insecure2$coefficients[2], 2)
# exp_p_insecure <- signif(summary(mo_insecure2)$coef[2,4], 2)
# exp_se_insecure <- signif(summary(mo_insecure2)$coef[2,2], 2)
# 
# exp_beta_insecure2 <- signif(mo_insecure2$coefficients[3], 2)
# exp_p_insecure2 <- signif(summary(mo_insecure2)$coef[3,4], 2)
# exp_se_insecure2 <- signif(summary(mo_insecure2)$coef[3,2], 2)

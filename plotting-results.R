
load("data/model-stats.rda")

dk <- out %>% 
  pivot_longer(starts_with("t"), names_to = "time", values_to = "survival") %>% 
  mutate(time = as.numeric(str_remove_all(time, "t"))) %>% 
  select(-groups)

dk$condition <- NA
dk$condition[dk$greedy==1 & dk$nbr==1] <- "Greedy, p = 1"
dk$condition[dk$greedy==1 & dk$nbr==0] <- "Greedy without\nneed-based requests"
dk$condition[dk$stingy==0.5] <- "Stingy, p = 0.5"
dk$condition[(dk$stingy + dk$greedy) == 0] <- "Standard"

dk$Stat[dk$Stat=="Mean"] <- "Mean survival"
dk$Stat[dk$Stat=="SD_cattle"] <- "SD cattle"

model_dyad_plot <- 
  dk %>% 
  filter(Stat != "SD_survival") %>% 
  ggplot(aes(x = time, y = survival, colour = condition)) +
  geom_vline(xintercept = 50, alpha = 0.4, linetype = 2) +
  geom_line(lwd = 1) +
  theme_classic() +
  scale_colour_viridis_d() +
  facet_grid(Stat~group_size, scales = "free_y") +
  labs(x = "time", y = "", colour = "") +
  xlim(c(0, 100)) +
  theme(legend.position = "top",
        strip.text.y = element_text(angle = 0))


load("data/model-survival.rda")
model_heatmap <- out %>% 
  select(group_size, greedy, stingy, t50) %>% 
  filter(group_size %in% c(2, 4, 8, 16)) %>% 
  mutate(group_size = paste0("N = ", group_size),
         group_size = factor(group_size, levels = c("N = 2", "N = 4", "N = 8", "N = 16")),
         numlab = case_when(
           (greedy + stingy) %% 0.5 == 0 ~ TRUE, 
           TRUE ~ FALSE
         ),
         txtcol = 1 - t50) %>% 
  ggplot(aes(x = greedy, y = stingy, fill = t50)) +
  geom_tile() +
  scale_fill_viridis_c() + 
  theme_classic() +
  geom_contour(aes(z = t50), colour = "white", linetype=2) +
  geom_text(aes(label = ifelse(numlab, round(t50, 2), ""), colour = ifelse(txtcol < 0.5, "a", "b"))) +
  scale_colour_manual(values = c("black", "white")) +
  facet_wrap(~group_size) +
  labs(x = "Prob. greedy defection", y = "Prob. stingy defection", fill = "Survival") +
  guides(colour = "none") +
  theme(legend.position = "top")

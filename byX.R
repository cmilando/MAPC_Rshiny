library(readxl)
library(tidyverse)
library(ggpubr)
library(patchwork)

by_X_df <- read_xlsx("level_by_COMMTYPE.xlsx")

head(by_X_df)

p1 <- by_X_df %>%
  mutate(vals = mean_annual_attr_ED_rate_est,
         lb = mean_annual_attr_ED_rate_lb,
         ub =mean_annual_attr_ED_rate_ub ) %>%
  ggplot(aes(x = reorder(level, vals), y = vals)) +
  geom_hline(yintercept = 0) +
  geom_col(fill = "lightblue", width = 0.75, color = 'black', alpha = 0.75) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
  theme_classic2(base_size = 12) +
  theme(
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    # plot.margin = margin(t=5,r=150,b =5,l=5)
  ) +
  xlab(NULL)+ ylab("Annual Heat.Attr.\nED visit rate\n(# per 100,000)")




by_X_df <- read_xlsx("county_by_MONTH.xlsx")

head(by_X_df)

by_X_df %>%
  mutate(vals = mean_annual_attr_ED_rate_est,
         lb = mean_annual_attr_ED_rate_lb,
         ub =mean_annual_attr_ED_rate_ub ) %>%
  ggplot(aes(x = reorder(AREA, vals), y = vals, fill = level)) +
  geom_hline(yintercept = 0) +
  geom_col(width = 0.75, color = 'black', alpha = 0.75) +
  #geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
  theme_classic2(base_size = 12) +
  theme(
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    # plot.margin = margin(t=5,r=150,b =5,l=5)
  ) +
  scale_fill_discrete(name = 'Month') +
  xlab(NULL)+ ylab("Annual Heat.Attr.\nED visit rate\n(# per 100,000)")


by_X_df <- read_xlsx("county_by_AGE.xlsx")

head(by_X_df)

by_X_df %>%
  mutate(vals = mean_annual_attr_ED_rate_est,
         lb = mean_annual_attr_ED_rate_lb,
         ub =mean_annual_attr_ED_rate_ub ) %>%
  ggplot(aes(x = reorder(AREA, vals), y = vals, fill = level)) +
  geom_hline(yintercept = 0) +
  geom_col(width = 0.75, color = 'black', alpha = 0.75) +
  #geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
  theme_classic2(base_size = 12) +
  theme(
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    # plot.margin = margin(t=5,r=150,b =5,l=5)
  ) +
  scale_fill_discrete(name = 'Month') +
  xlab(NULL)+ ylab("Annual Heat.Attr.\nED visit rate\n(# per 100,000)")

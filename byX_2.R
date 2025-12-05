# ugh jesus dude, byX 2

library(tidyverse)
library(readxl)
library(ggpubr)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' AGE
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
byX_df <- readxl::read_xlsx("county_by_AGE.xlsx")

byX_df %>%
  mutate(
    val = mean_annual_attr_ED_rate_est,
    lb  = mean_annual_attr_ED_rate_lb,
    ub  = mean_annual_attr_ED_rate_ub,
    fct = factor(level,
                 levels = c('0-4', '5-17', '18-39',
                            '40-64', '65-79','80+'),
                 ordered = T)
  ) %>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  geom_col(aes(x = reorder(AREA, val), y = val, fill = fct),
           color = 'grey15') +
  scale_fill_brewer(name = 'Age Group',type = 'qual') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

byX_df %>%
  mutate(
    val = mean_annual_attr_ED_rate_est,
    lb  = mean_annual_attr_ED_rate_lb,
    ub  = mean_annual_attr_ED_rate_ub,
    fct = factor(level,
                 levels = c('0-4', '5-17', '18-39',
                            '40-64', '65-79','80+'),
                 ordered = T),
    AREA_fct = reorder(AREA, val)
  ) %>%
  #filter(level %in% c('0-4', '80+'))%>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  facet_grid(~AREA_fct, scales = 'free_x', switch = 'x') +
  geom_col(aes(x = fct, y = val, fill = fct),
           color = 'grey15', linewidth = 0.4) +
  geom_errorbar(aes(x = fct, 
                    ymax = ub, ymin = lb, group = fct),
                color = 'grey15', width = 0.3,
                linewidth = 0.4) +
  scale_fill_brewer(name = 'Age Group',type = 'qual') +
  xlab(NULL) +

  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(strip.text.x = element_text(angle = 45, 
                                    hjust = 1, 
                                    vjust = 1),
        panel.grid.major.y = element_line(linetype = '11',
                                                color = 'grey'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.background = element_blank(),
        strip.clip = 'off', 
        strip.switch.pad.grid = unit(-0.15,'cm'),
        strip.placement = 'outside')

ggsave("img/by_age.png", width = 10.8, height = 3.2)

byX_df %>%
  mutate(
    val = mean_annual_attr_ED_visit_est,
    lb  = mean_annual_attr_ED_visit_lb,
    ub  = mean_annual_attr_ED_visit_ub,
    fct = factor(level,
                 levels = c('0-4', '5-17', '18-39',
                            '40-64', '65-79','80+'),
                 ordered = T)
  ) %>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  geom_col(aes(x = reorder(AREA, val), y = val, fill = fct),
           color = 'grey15') +
  scale_fill_brewer(name = 'Age Group',type = 'qual') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visits\n(#)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5))



#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' AGE
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
byX_df <- readxl::read_xlsx("county_by_MONTH.xlsx")

byX_df %>%
  mutate(
    val = mean_annual_attr_ED_rate_est,
    lb  = mean_annual_attr_ED_rate_lb,
    ub  = mean_annual_attr_ED_rate_ub,
    fct = factor(level,
                 levels = c('5', '6', '7',
                            '8', '9'),
                 ordered = T),
    AREA_fct = reorder(AREA, val)
  ) %>%
  filter(level != '9')%>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  facet_grid(~AREA_fct, scales = 'free_x', switch = 'x') +
  geom_col(aes(x = fct, y = val, fill = fct),
           color = 'grey15', linewidth = 0.4) +
  geom_errorbar(aes(x = fct, 
                    ymax = ub, ymin = lb, group = fct),
                color = 'grey15', width = 0.3,
                linewidth = 0.4) +
  scale_fill_brewer(name = 'Month',type = 'qual') +
  xlab(NULL) +
  
  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(strip.text.x = element_text(angle = 45, 
                                    hjust = 1, 
                                    vjust = 1),
        panel.grid.major.y = element_line(linetype = '11',
                                          color = 'grey'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.background = element_blank(),
        strip.clip = 'off', 
        strip.switch.pad.grid = unit(-0.15,'cm'),
        strip.placement = 'outside')

ggsave("img/by_month.png", width = 10.8, height = 3.2)

byX_df %>%
  mutate(
    val = mean_annual_attr_ED_visit_est,
    lb  = mean_annual_attr_ED_visit_lb,
    ub  = mean_annual_attr_ED_visit_ub,
    fct = factor(level,
                 levels = c('0-4', '5-17', '18-39',
                            '40-64', '65-79','80+'),
                 ordered = T)
  ) %>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  geom_col(aes(x = reorder(AREA, val), y = val, fill = fct),
           color = 'grey15') +
  scale_fill_brewer(name = 'Age Group',type = 'qual') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visits\n(#)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5))



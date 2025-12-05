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
  filter(AREA %in% c('MIDDLESEX', 'SUFFOLK')) %>%
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
  geom_label(aes(x = fct, y = val+1, label = round(val, 1)),
             color = 'grey15', linewidth = 0.4, fill = 'white') +
  scale_fill_brewer(name = 'Age Group',type = 'qual') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(
        panel.grid.major.y = element_line(linetype = '11',
                                                color = 'grey'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.background = element_blank(),
        strip.clip = 'off', 
        strip.switch.pad.grid = unit(-0.15,'cm'),
        strip.placement = 'outside')

# ggsave("img/by_age.png", width = 10.8, height = 3.2)

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' MONTH
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
byX_df <- readxl::read_xlsx("county_by_MONTH.xlsx")

byX_df %>%
  filter(AREA %in% c('MIDDLESEX', 'SUFFOLK')) %>%
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
  geom_label(aes(x = fct, y = val+1, label = round(val, 1)),
             color = 'grey15', linewidth = 0.4, fill = 'white') +
  
  scale_fill_brewer(name = 'Month',type = 'qual') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(strip.text.x = element_text(angle = 0, 
                                    hjust = 0.5, 
                                    vjust = 0),
        panel.grid.major.y = element_line(linetype = '11',
                                          color = 'grey'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.background = element_blank(),
        strip.clip = 'off', 
        strip.switch.pad.grid = unit(-0.15,'cm'),
        strip.placement = 'outside')

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' MONTH
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
byX_df <- readxl::read_xlsx("level_by_COMMTYPE.xlsx")
head(byX_df, 10)

byX_df %>%
  mutate(
    AREA = 'MA',
    val = mean_annual_attr_ED_rate_est,
    lb  = mean_annual_attr_ED_rate_lb,
    ub  = mean_annual_attr_ED_rate_ub,
    fct = factor(level),
    AREA_fct = reorder(level, val)
  ) %>%
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
  scale_fill_brewer(name = 'Community Type',type = 'qual') +
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

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' CITY
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
byX_df <- readxl::read_xlsx("by_city.xlsx")
head(byX_df, 10)
dim(byX_df)

subregions <- c('CHELSEA', 'REVERE', 'EVERETT',
               'MALDEN', 'WINTHROP', 'BOSTON')


byX_df %>%
  filter(region %in% subregions) %>%
  filter(POP2020 > 1e4) %>%
  mutate(
    AREA = region,
    val = mean_annual_attr_ED_rate_est,
    lb  = mean_annual_attr_ED_rate_lb,
    ub  = mean_annual_attr_ED_rate_ub,
    fct = factor(region),
    AREA_fct = reorder(region, val)
  ) %>%
  arrange(-val) %>%
  slice(1:30) %>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  facet_grid(~AREA_fct, scales = 'free_x', switch = 'x') +
  geom_col(aes(x = fct, y = val),
           color = 'grey15', linewidth = 0.4, fill = 'lightblue') +
  geom_errorbar(aes(x = fct, 
                    ymax = ub, ymin = lb, group = fct),
                color = 'grey15', width = 0.3,
                linewidth = 0.4) +
  geom_label(aes(x = fct, y = val+1, label = round(val, 1)),
           color = 'grey15', linewidth = 0.4, fill = 'white') +
  xlab(NULL) +
  ylab("Annual Heat Attr.\nAll Cause ED visit rates\n(# per 100,000)") +
  theme(strip.text.x = element_text(angle = 15, 
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


##

byX_df %>%
  filter(region %in% subregions) %>%
  filter(POP2020 > 1e4) %>%
  mutate(
    AREA = region,
    val = mean_annual_attr_ED_visit_est,
    lb  = mean_annual_attr_ED_visit_lb,
    ub  = mean_annual_attr_ED_visit_ub,
    fct = factor(region),
    AREA_fct = reorder(region, val)
  ) %>%
  arrange(-val) %>%
  slice(1:30) %>%
  ggplot(.) +
  coord_cartesian() +
  theme_classic2() +
  facet_grid(~AREA_fct, scales = 'free_x', switch = 'x') +
  geom_col(aes(x = fct, y = val),
           color = 'grey15', linewidth = 0.4, fill = 'lightblue') +
  geom_errorbar(aes(x = fct, 
                    ymax = ub, ymin = lb, group = fct),
                color = 'grey15', width = 0.3,
                linewidth = 0.4) +
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

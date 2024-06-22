library(here)
library(tidyverse)
library(ggdist)

power_out <- readRDS(file=here("results","simulated_time_series_summary.rds"))

power_gg <- power_out %>% 
  ggplot(aes(x=rate, y=sig_time_series_length)) +
  stat_lineribbon() +
  geom_hline(aes(yintercept=101), linetype="dashed", lwd=1.2, color="black") +
  theme_bw() +
 # scale_color_brewer() +
  scale_fill_brewer() +
  scale_x_continuous(limits=c(0.0009, 0.1), breaks=seq(0, 0.1, 0.01)) +
  labs(x="Range edge shift rate (°lat/yr)", y="Number of years to \ndetect significant shift", fill="Proportion of \nsimulations") +
  theme(legend.position=c(0.8, 0.6)) +
  NULL

power_gg  
ggsave(power_gg, filename=here("figures","power.png"), width=8, height=4, dpi=160)

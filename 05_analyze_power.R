library(here)
library(tidyverse)
library(ggdist)

power_out <- readRDS(file=here("results","simulated_time_series_summary.rds"))

load(file=here("results","parameters.Rdata"))
# hacky way to change this into a df 

powerdat <- NULL
for(i in 1:iters) {
  mat <- power_out[[i]]
  tmp <- as.data.frame(mat) %>% 
    rowid_to_column() %>% 
    mutate(ts_length = rowid + 2) %>% 
    select(-rowid) %>% 
    pivot_longer(- ts_length) %>% 
    mutate(power = value, 
           shiftrate_id = as.numeric(gsub("V","",name)),
           rate = shiftrate[shiftrate_id]) %>% 
    select(rate, power, ts_length) %>% 
    mutate(iter = i) 
  powerdat <- rbind(powerdat, tmp)
}

shiftrate_ex <- c(shiftrate[1], shiftrate[34], shiftrate[67], shiftrate[100])

power_gg <- powerdat %>% 
  filter(rate %in% shiftrate_ex) %>% 
  mutate(ratelab = paste0(rate, " °lat/yr")) %>% 
  ggplot(aes(x=ts_length, y=power, group = iter)) +
  geom_hline(aes(yintercept=0.8), color="black", linetype="dashed", lwd=1.2) +
  geom_line(aes(alpha = 0.2), color="grey30") +
  facet_wrap(~ratelab, ncol=4) +
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +
  theme_bw() +
  labs(x="Time-series length (years)", y="Power") +
  theme(legend.position = "none") +
  NULL
power_gg

ts_gg <- powerdat %>% 
  filter(power >= 0.8) %>% 
  ggplot(aes(x=rate, y=ts_length)) +
  stat_lineribbon() +
  geom_hline(aes(yintercept=100), linetype="dashed", lwd=1.2, color="black") +
  theme_bw() +
 # scale_color_brewer() +
  scale_fill_brewer() +
  scale_x_continuous(limits=c(0.001, 0.1), breaks=seq(0, 0.1, 0.01)) +
  labs(x="Range edge shift rate (°lat/yr)", y="Minimum years to \ndetect significant shift", fill="Proportion of \nsimulations") +
  theme(legend.position=c(0.1, 0.2)) +
  NULL
ts_gg

ggsave(ts_gg, filename=here("figures","time-series.png"), width=8, height=4, dpi=160)
ggsave(power_gg, filename=here("figures","power.png"), width=8, height=4, dpi=160)

# in-text results

# what is the fastest we got to significant power? 
powerdat %>% 
  filter(power >= 0.8) %>% 
  filter(ts_length == min(ts_length))

# quantiles of ts_length
quantile(powerdat %>% 
           filter(power >= 0.8) %>% pull(ts_length),
         probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1))

# shortest minimum time-series lengths that DID meet the power threshold 
med_ts_dat <- powerdat %>% 
  filter(power >= 0.8) %>% 
  group_by(rate) %>% 
  summarise(med_ts_length = median(ts_length))

# median of minimum time-series lengths that DID meet the power threshold 
min_ts_dat <- powerdat %>% 
  filter(power >= 0.8) %>% 
  group_by(rate) %>% 
  summarise(min_ts_length = min(ts_length))

# range of maximum time-series lengths that DIDN'T meet the power threshold
max_ts_dat <- powerdat %>% 
  filter(power < 0.8) %>% 
  group_by(rate) %>% 
  summarise(max_ts_length = max(ts_length))

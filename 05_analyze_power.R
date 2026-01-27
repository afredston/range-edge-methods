library(here)
library(tidyverse)
library(ggdist)

load(file=here("results","parameters_test.Rdata"))

power_out <- readRDS(file=here("results","simulated_time_series_summary_test.rds"))

# reshape into a dataframe for plotting, analysis, etc. 
# reshaping code thanks to chatgpt
powerdat <- imap_dfr(power_out, \(a, iter) {
  d <- dim(a)  # c(J, L, M)
  
  expand_grid(
    error_i     = seq_len(d[3]),   # slowest
    tslen_i     = seq_len(d[2]),
    shiftrate_i = seq_len(d[1])    # fastest (matches as.vector() on arrays)
  ) |>
    mutate(
      iter      = iter,
      power     = as.vector(a),
      shiftrate = shiftrate[shiftrate_i],
      ts_length = ts_lengths[tslen_i],
      error_sd  = errors[error_i]
    ) |>
    select(iter, shiftrate, ts_length, error_sd, power)
})

# add nice columns for plotting
powerdat <- powerdat %>% 
  mutate(ratelab = paste0(shiftrate, " °lat/yr"),
         sdlab = 
           ifelse(error_sd == errors[1], "Low SD", ifelse(error_sd == errors[2], "Mid SD", "High SD")),
         sdlab = factor(sdlab, levels = c("Low SD", "Mid SD", 'High SD'))
  ) 

shiftrate_ex <- c(shiftrate[1], shiftrate[34], shiftrate[67], shiftrate[100])

power_gg <- powerdat %>% 
  filter(shiftrate %in% shiftrate_ex) %>% 
  ggplot(aes(x=ts_length, y=power, group = iter)) +
  geom_hline(aes(yintercept=0.8), color="black", linetype="dashed", lwd=1.2) +
  geom_line(aes(alpha = 0.2), color="grey30") +
  facet_grid(sdlab~ratelab) +
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +
  theme_bw() +
  labs(x="Time-series length (years)", y="Power") +
  theme(legend.position = "none") +
  NULL
power_gg

ts_gg <- powerdat %>% 
  filter(power >= 0.8) %>% 
  ggplot(aes(x=shiftrate, y=ts_length)) +
  stat_lineribbon() +
  geom_hline(aes(yintercept=100), linetype="dashed", lwd=1.2, color="black") +
  theme_bw() +
 # scale_color_brewer() +
  scale_fill_brewer() +
  scale_x_continuous(limits=c(0.001, 0.1), breaks=seq(0, 0.1, 0.01)) +
  labs(x="Range edge shift rate (°lat/yr)", y="Minimum years to \ndetect significant shift", fill="Proportion of \nsimulations") +
  theme(legend.position=c(0.1, 0.2)) +
  facet_wrap(~sdlab) + 
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

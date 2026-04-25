library(here)
library(tidyverse)
library(ggdist)

load(file=here("results","parameters.Rdata"))

power_out <- readRDS(file=here("results","simulated_time_series_summary.rds"))

# reshape into a dataframe for plotting, analysis, etc. 
# reshaping code thanks to chatgpt
powerdat <- imap_dfr(power_out, \(a, iter) {
  d <- dim(a$power)  # c(J, L, M)
  
  expand_grid(
    error_i     = seq_len(d[3]),   # slowest
    tslen_i     = seq_len(d[2]),
    shiftrate_i = seq_len(d[1])    # fastest (matches as.vector() on arrays)
  ) |>
    mutate(
      iter      = iter,
      power     = as.vector(a$power),
      shiftrate = shiftrate[shiftrate_i],
      ts_length = ts_lengths[tslen_i],
      error_sd  = errors[error_i]
    ) |>
    select(iter, shiftrate, ts_length, error_sd, power)
})

out_true_dat <- imap_dfr(power_out, \(a, iter) {
  d <- dim(a$out_true)  # c(J, K, M)
  
  expand_grid(
    error_i     = seq_len(d[3]),   # slowest
    sampleyr_i  = seq_len(d[2]),
    shiftrate_i = seq_len(d[1])    # fastest (matches as.vector() on arrays)
  ) |>
    mutate(
      iter      = iter,
      out_true  = as.vector(a$out_true),
      shiftrate = shiftrate[shiftrate_i],
      sampleyr  = sampleyrs[sampleyr_i],
      error_sd  = errors[error_i]
    ) |>
    select(iter, shiftrate, sampleyr, error_sd, out_true)
})

# BEGIN RESURVEY ANALYSIS 
# resurvey analysis (this chunk also written with help from chatgpt)
library(dplyr)
library(purrr)
library(foreach)
library(doParallel)
library(here)

registerDoParallel(cores = 150)

gaps <- 3:100

dir.create(here("results", "out_compare_chunks"), showWarnings = FALSE)

group_keys <- out_true_dat |>
  distinct(iter, shiftrate, error_sd)

calc_slopes_one_ts <- function(df, gaps = 3:100) {
  df <- df |> arrange(sampleyr)
  
  x <- df$sampleyr
  y <- df$out_true
  n_years <- length(x)
  
  c_y  <- c(0, cumsum(y))
  c_xy <- c(0, cumsum(x * y))
  c_x  <- c(0, cumsum(x))
  c_x2 <- c(0, cumsum(x^2))
  
  map_dfr(gaps, function(gap) {
    starts <- seq_len(n_years - gap)
    ends <- starts + gap
    N <- gap + 1
    
    sum_y  <- c_y[ends + 1]  - c_y[starts]
    sum_xy <- c_xy[ends + 1] - c_xy[starts]
    sum_x  <- c_x[ends + 1]  - c_x[starts]
    sum_x2 <- c_x2[ends + 1] - c_x2[starts]
    
    slope_regression <- (sum_xy - sum_x * sum_y / N) /
      (sum_x2 - sum_x^2 / N)
    
    slope_endpoint <- (y[ends] - y[starts]) / gap
    
    tibble(
      gap = gap,
      start_year = x[starts],
      end_year = x[ends],
      slope_endpoint = slope_endpoint,
      slope_regression = slope_regression
    )
  })
}

foreach(
  g = seq_len(nrow(group_keys)),
  .packages = c("dplyr", "purrr", "here")
) %dopar% {
  
  key <- group_keys[g, ]
  
  df <- out_true_dat |>
    filter(
      iter == key$iter,
      shiftrate == key$shiftrate,
      error_sd == key$error_sd
    )
  
  # run resurvey analysis for all iterations and parameter combinations 
  out <- calc_slopes_one_ts(df, gaps = gaps) |>
    mutate(
      iter = key$iter,
      shiftrate = key$shiftrate,
      error_sd = key$error_sd,
      .before = 1
    )
  
  saveRDS(
    out,
    file = here(
      "results", "out_compare_chunks",
      paste0("out_compare_group_", g, ".rds")
    )
  )
  
  NULL
}

resurvey_thin <- out_compare |> 
  group_by(shiftrate, error_sd, gap) |> 
  slice_sample(n=1000) |> 
  ungroup()  |> 
  filter(gap %in% c(10, 20, 50, 100)) |> 
left_join(powerdat_summ |> select(error_sd, sdlab) |> distinct()) 


quad_pct <- resurvey_thin |>
  mutate(
    quadrant = case_when(
      slope_regression >= 0 & slope_endpoint >= 0 ~ "pos_pos",
      slope_regression <  0 & slope_endpoint >= 0 ~ "neg_pos",
      slope_regression <  0 & slope_endpoint <  0 ~ "neg_neg",
      slope_regression >= 0 & slope_endpoint <  0 ~ "pos_neg"
    )
  ) |>
  count(gap, sdlab, quadrant) |>
  complete(
    gap,
    sdlab,
    quadrant = c("pos_pos", "neg_pos", "neg_neg", "pos_neg"),
    fill = list(n = 0)
  ) |>
  group_by(gap, sdlab) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(
    x = case_when(
      quadrant %in% c("pos_pos", "pos_neg") ~ Inf,
      TRUE ~ -Inf
    ),
    y = case_when(
      quadrant %in% c("pos_pos", "neg_pos") ~ Inf,
      TRUE ~ -Inf
    ),
    hjust = case_when(
      quadrant %in% c("pos_pos", "pos_neg") ~ 1.2,
      TRUE ~ -0.2
    ),
    vjust = case_when(
      quadrant %in% c("pos_pos", "neg_pos") ~ 1.2,
      TRUE ~ -0.5
    ),
    label = paste0(round(pct, 1), "%")
  )

resurvey_gg <- resurvey_thin |> 
  ggplot(aes(x = slope_regression, y = slope_endpoint)) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = "grey85", alpha = 0.6
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "grey85", alpha = 0.6
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_text(
    data = quad_pct,
    aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  facet_grid(sdlab ~ gap, labeller = labeller(
    gap = \(x) paste0(x, "-year gap")
  )) +
  labs(
    x = "Regression slope estimated from full interval",
    y = "Endpoint slope estimated from resurvey only"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(resurvey_gg, filename=here("figures","resurvey.png"), width=8, height=6, dpi=160)

############ END RESURVEY ANALYSIS 

# plot for reviewer showing the data I summarized to get power estimates 
powerdat |> 
  filter(shiftrate %in% c(0.050, 0.075), 
         ts_length %in% c(5, 50),
         error_sd == errors[2]) |> 
  ggplot(aes(x=power)) +
  geom_histogram() +
  facet_grid(shiftrate ~ ts_length)

# get median power for each parameter combination
powerdat_summ <- powerdat |> 
  group_by(shiftrate, ts_length, error_sd) |> 
  summarise(power = median(power))|> 
  ungroup()

# add nice columns for plotting
powerdat_summ <- powerdat_summ %>% 
  mutate(ratelab = paste0(shiftrate, " °lat/yr"),
         sdlab = 
           ifelse(error_sd == errors[1], "Low SD", ifelse(error_sd == errors[2], "Mid SD", "High SD")),
         sdlab = factor(sdlab, levels = c("Low SD", "Mid SD", "High SD"))
  ) 
powerdat <- powerdat %>% 
  mutate(ratelab = paste0(shiftrate, " °lat/yr"),
         sdlab = 
           ifelse(error_sd == errors[1], "Low SD", ifelse(error_sd == errors[2], "Mid SD", "High SD")),
         sdlab = factor(sdlab, levels = c("Low SD", "Mid SD", "High SD"))
  ) 

shiftrate_ex <- c(shiftrate[1], shiftrate[34], shiftrate[67], shiftrate[100])

# ts_gg <- powerdat %>% 
# #  filter(power >= 0.8) %>% 
#   ggplot(aes(x=shiftrate, y=ts_length)) +
#   stat_lineribbon() +
#   geom_hline(aes(yintercept=100), linetype="dashed", lwd=1.2, color="black") +
#   theme_bw() +
#  # scale_color_brewer() +
#   scale_fill_brewer() +
#   scale_x_continuous(limits=c(0.001, 0.1), breaks=seq(0, 0.1, 0.02)) +
#   labs(x="Range edge shift rate (°lat/yr)", y="Minimum years to \ndetect significant shift", fill="Proportion of \nsimulations") +
#   theme(legend.position=c(0.1, 0.2)) +
#   facet_wrap(~sdlab) + 
#   NULL
# ts_gg

#ggsave(ts_gg, filename=here("figures","time-series.png"), width=8, height=4, dpi=160)

# in-text results

# what is the fastest we got to significant power? 
powerdat %>% 
  filter(power >= 0.8) %>% 
  filter(ts_length == min(ts_length))

# at intermediate shift rate, intermediate error rate, when did we reach power threshold? 
powerdat_summ |> 
  filter(shiftrate == 0.050,
         error_sd == errors[2]) |> 
  filter(power >= 0.8) |> 
  arrange(ts_length) 

power_summ_gg <- powerdat_summ |>
  group_by(shiftrate, ts_length, error_sd, sdlab) |>
  ggplot(aes(x=shiftrate, y=ts_length, fill=power)) +
  geom_tile() +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 0.1, 0.02)) +
  scale_fill_gradientn(
    colours = c("#a50026", "#ffffbf", "#313695"),
    values  = scales::rescale(c(0, 0.8, 1)),
    limits  = c(0, 1),
    breaks  = seq(0, 1, 0.2),
    labels  = seq(0, 1, 0.2),
    na.value = "grey50"
  ) +
  labs(x="Range edge shift rate (°lat/yr)", y="Time-series length", fill="Power") +
  facet_wrap(~sdlab) +
  theme(
    legend.position = "right",
    legend.direction = "vertical"
  ) +
  NULL
power_summ_gg
ggsave(power_summ_gg, filename=here("figures","power_threshold.png"), width=8, height=3, dpi=160)


power_iters_gg <- powerdat %>% 
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
power_iters_gg
ggsave(power_iters_gg, filename=here("figures","power.png"), width=8, height=4, dpi=160)


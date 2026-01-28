library(tidyverse)
library(here)

fish_edgetype <- "eq"
focal_fish <- c('Urophycis tenuis')
fish_edgetidy <- read_csv(file=here("results",paste0(focal_fish, "_", fish_edgetype, "_results.csv"))) %>% 
  mutate(sig = factor(sig, levels=c('yes','no')))

# which were significant, and what were the coefficients?
fish_edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct()

# report in km (approximate)
# fish_edgetidy %>% 
#   select(Method, p.value, estimate, std.error) %>% 
#   distinct() %>% 
#   mutate(estimate = estimate * 111,
#          std.error = std.error * 111)

# make plots

colorblind_safe_eq <- c( '#EECC66', '#004488', '#6699CC', '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/
colorblind_safe_pol <- c( '#EECC66','#004488', '#6699CC',  '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/

fishtitle <- expression(paste(italic("Urophycis tenuis")," equatorward edge"))
# once final species are chosen can italicize title more easily

# adding SD bars 
fish_sd_dat <- fish_edgetidy %>%
  group_by(Method) %>%
  summarise(
    residual_sd = first(residual_sd), 
    last_year   = max(year),
    y_last      = lat_position[which.max(year)],
    .groups = "drop"
  ) %>%
  arrange(Method) %>%
  mutate(
    x_bar = last_year + 0.4,
    x_off = c(0, 0.4, 0.8, 1.2, 1.6),
    x_bar = x_bar + x_off,
    y_min = y_last - residual_sd,
    y_max = y_last + residual_sd,
    y_lab = y_last + c(0.7, -0.85, 0.6, -0.6, 0), 
    x_lab = x_bar + c(-0.4, 0, 0.3, 0.5, 0.7),
    label = sprintf("SD=%.2f", residual_sd)
  )

fishplot <- ggplot(fish_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe_eq) +
  scale_fill_manual(values=colorblind_safe_eq) +
  labs(title=fishtitle, x="Year", y="Latitude") +
  guides(linetype="none") +
  theme_bw() +
  theme(legend.position=c(0.25, 0.20),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank()) +
  geom_segment(
    data = fish_sd_dat,
    aes(x = x_bar, xend = x_bar, y = y_min, yend = y_max, color = Method),
    inherit.aes = FALSE,
    linewidth = 2,
    show.legend = FALSE
  ) +
  geom_text(
    data = fish_sd_dat,
    aes(x = x_lab, y = y_lab, label = label, color = Method),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks=seq(1980, 2020, 10), limits=c(1970, 2023)) +
  NULL
  
fishplot
ggsave(fishplot, filename=here("figures","fish_edge.png"), width=8, height=4, dpi=160)


bird_edgetype <- "pol"
focal_bird <- c('Coragyps atratus')
bird_edgetidy <- read_csv(file=here("results",paste0(focal_bird, "_", bird_edgetype, "_results.csv"))) %>% 
  mutate(sig = factor(sig, levels=c('yes','no')))

# which were significant, and what were the coefficients?
bird_edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct()

# report in km (approximate)
# bird_edgetidy %>% 
#   select(Method, p.value, estimate, std.error) %>% 
#   distinct() %>% 
#   mutate(estimate = estimate * 111,
#          std.error = std.error * 111)


# make plots

birdtitle <- expression(paste(italic("Coragyps atratus")," poleward edge"))

# adding SD bars 
bird_sd_dat <- bird_edgetidy %>%
  group_by(Method) %>%
  summarise(
    residual_sd = first(residual_sd), # always the same 
    last_year   = max(year),
    y_last      = lat_position[which.max(year)],
    .groups = "drop"
  ) %>%
  arrange(Method) %>%
  mutate(
    x_bar = last_year + 0.4,
    x_off = c(0, 0, 0.4, 0.8, 0.4),
    x_bar = x_bar + x_off,
    y_min = y_last - residual_sd,
    y_max = y_last + residual_sd,
    y_lab = y_last + c(0, -0.5, -0.25, 0.25, 0), 
    x_lab = x_bar + 0.2,
    label = sprintf("SD=%.2f", residual_sd)
  )

birdplot <- ggplot(bird_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe_pol) +
  scale_fill_manual(values=colorblind_safe_pol) +
  labs(title=birdtitle, x="Year", y="Latitude") +
  guides(linetype="none",
         fill=guide_legend(),
         color=guide_legend()) +
  theme_bw() +
  theme(legend.position=c(0.22, 0.8),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank()) +
  geom_segment(
    data = bird_sd_dat,
    aes(x = x_bar, xend = x_bar, y = y_min, yend = y_max, color = Method),
    inherit.aes = FALSE,
    linewidth = 2,
    show.legend = FALSE
  ) +
  geom_text(
    data = bird_sd_dat,
    aes(x = x_lab, y = y_lab, label = label, color = Method),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks=seq(1980, 2020, 10), limits=c(1975, 2027)) +
  NULL
birdplot
ggsave(birdplot, filename=here("figures","bird_edge.png"), width=8, height=4, dpi=160)

resid_sd_dat <- bird_edgetidy |> 
  select(Edge, Method, residual_sd) |> 
  distinct() |> 
  bind_rows(fish_edgetidy |> 
              select(Edge, Method, residual_sd) |> 
              distinct())
write_csv(resid_sd_dat, here("results","residual_sd_after_detrending.csv"))

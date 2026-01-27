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
fish_edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct() %>% 
  mutate(estimate = estimate * 111,
         std.error = std.error * 111)

# calculate variance 
fish_edgetidy |> 
  group_by(Method) |> 
  summarise(variance = var(lat_position))

# make plots

colorblind_safe_eq <- c( '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/
colorblind_safe_pol <- c( '#EECC66','#004488', '#6699CC',  '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/

fishtitle <- expression(paste(italic("Urophycis tenuis")," equatorward edge"))
# once final species are chosen can italicize title more easily

fishplot <- ggplot(fish_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe_eq) +
  scale_fill_manual(values=colorblind_safe_eq) +
  labs(title=fishtitle, x="Year", y="Latitude") +
  guides(linetype="none") +
  theme_bw() +
  theme(legend.position=c(0.33, 0.25),
        legend.background = element_rect(fill="transparent")) +
  # theme(legend.position="bottom",
  #   legend.position.inside = c(0.5, 0.5)) +
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
bird_edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct() %>% 
  mutate(estimate = estimate * 111,
         std.error = std.error * 111)

# calculate variance 
bird_edgetidy |> 
  group_by(Method) |> 
  summarise(variance = var(lat_position))

# make plots

birdtitle <- expression(paste(italic("Coragyps atratus")," poleward edge"))

# once final species are chosen can italicize title more easily

birdplot <- ggplot(bird_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe_pol) +
  scale_fill_manual(values=colorblind_safe_pol) +
  labs(title=birdtitle, x="Year", y="Latitude") +
  guides(linetype="none",
         fill=guide_legend(ncol=2),
         color=guide_legend(ncol=2)) +
  theme_bw() +
  theme(legend.position=c(0.235, 0.84),
        legend.background = element_rect(fill="transparent")) +
  # theme(legend.position="bottom",
  #   legend.position.inside = c(0.5, 0.5)) +
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

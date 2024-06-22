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

# make plots

colorblind_safe <- c( '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/

title <- ifelse(fish_edgetype=="eq", paste0(focal_fish, " equatorward edge"), paste0(focal_fish, " poleward edge"))

# once final species are chosen can italicize title more easily

fishplot <- ggplot(fish_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe) +
  scale_fill_manual(values=colorblind_safe) +
  labs(title=title, x="Year", y="Latitude") +
  guides(linetype="none") +
  theme_bw() +
  theme(legend.position=c(0.38, 0.3),
        legend.background = element_rect(fill="transparent")) +
  # theme(legend.position="bottom",
  #   legend.position.inside = c(0.5, 0.5)) +
  NULL
#fishplot
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

# make plots

colorblind_safe <- c( '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/

title <- ifelse(bird_edgetype=="eq", paste0(focal_bird, " equatorward edge"), paste0(focal_bird, " poleward edge"))

# once final species are chosen can italicize title more easily

birdplot <- ggplot(bird_edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig), lwd=0.8)  +
  scale_color_manual(values=colorblind_safe) +
  scale_fill_manual(values=colorblind_safe) +
  labs(title=title, x="Year", y="Latitude") +
  guides(linetype="none",
         fill=guide_legend(ncol=2),
         color=guide_legend(ncol=2)) +
  theme_bw() +
  theme(legend.position=c(0.235, 0.84),
        legend.background = element_rect(fill="transparent")) +
  # theme(legend.position="bottom",
  #   legend.position.inside = c(0.5, 0.5)) +
  NULL

ggsave(birdplot, filename=here("figures","bird_edge.png"), width=8, height=4, dpi=160)

# sd_dat <- bind_rows(bird_edgetidy, fish_edgetidy) %>% 
#   group_by(Species, Method) %>% 
#   summarise(SD = sd(lat_position))
# 
# sd_gg_fish <- sd_dat %>%
#   filter(Species == focal_fish) %>% 
#   ggplot(aes(x=Method, y=SD, color=Method, fill=Method)) + 
#   geom_point() +
#   scale_color_manual(values=colorblind_safe) +
#   scale_fill_manual(values=colorblind_safe) +
#   labs(title=title, x="Method", y="Standard deviation of latitude") +
#   theme_bw() +
#   NULL
#   sd_gg_fish
 
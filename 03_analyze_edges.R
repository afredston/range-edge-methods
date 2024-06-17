library(tidyverse)
library(here)

edgetype <- "eq"
focal_spp <- c('Urophycis tenuis')
edgetidy <- read_csv(file=here("results",paste0(focal_spp, "_", edgetype, "_results.csv"))) %>% 
  mutate(sig = factor(sig, levels=c('yes','no')))

# which were significant, and what were the coefficients?
edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct()

# report in km (approximate)
edgetidy %>% 
  select(Method, p.value, estimate, std.error) %>% 
  distinct() %>% 
  mutate(estimate = estimate * 111,
         std.error = std.error * 111)

# make plots

colorblind_safe <- c( '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA') # https://personal.sron.nl/~pault/

title <- ifelse(edgetype=="eq", paste0(focal_spp, " Equatorward Edge"), paste0(focal_spp, " Poleward Edge"))

ggplot(edgetidy, aes(x=year, y=lat_position, color=Method, fill=Method, group=Method)) +
  geom_line(aes(linetype=sig))  +
  scale_color_manual(values=colorblind_safe) +
  scale_fill_manual(values=colorblind_safe) +
  labs(title=title, x="Year", y="Latitude") +
  guides(linetype="none") +
  theme_bw() +
  NULL

# choosing a bird
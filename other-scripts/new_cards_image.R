# parameters:
# images

library(tibble)
library(dplyr)
library(ggplot2)

# list of images to plot
images <- c(
  "06RU009",
  "06RU009T6",
  "06RU009T11",
  "06RU009T9",
  "06RU009T7",
  "06BC028",
  "06BC038",
  "06BW026",
  "06BW026T1",
  "06BW039",
  "06MT032",
  "06MT040",
  "06FR017",
  "06FR034",
  "06PZ007",
  "06PZ037"
)

# to test format before cards come out
#images <- rep('06IO004', length(images))

# number of the current set
#set_number <- substr(images[1], start = 2, stop = 2)
set_number <- '6cde'

# plot parameters
if(length(images) > 8){
  n_rows <- 3
  n_cols <- 2 * n_rows
  aspect_ratio <- 2
  images_size <- (0.7)/(n_cols+1)
  shrink_parameter <- 0.5
} else {
  n_rows <- 2
  n_cols <- 2 * n_rows
  aspect_ratio <- (1024/680)
  images_size <- 1/(n_cols+1)
  shrink_parameter <- 1
}

# convert to df
data <- tibble(code = images) %>% 
  mutate(img = sprintf('https://dd.b.pvp.net/latest/set%s/en_us/img/cards/%s.png', set_number, code))

# add cohordinates
data <- data %>% 
  mutate(
    x = (shrink_parameter*(rep(seq(from = 1, to = n_cols, by = 1), ceiling(nrow(data) / n_cols))))[1:length(images)],
    y = (rep(c(n_rows:1), rep(ceiling(nrow(data) / n_rows), n_rows)))[1:length(images)]
  )

# make plot
plot <- ggplot(data) +
  geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf), fill = 'mediumslateblue', alpha = 0.05) +
  expand_limits(y = c(0.5, n_rows+0.5), x = c(shrink_parameter/2, (shrink_parameter*(n_cols+0.5)))) +
  theme_void() +
  ggimage::geom_image(aes(image = img, x = x, y = y), asp = aspect_ratio, size = images_size)

# add background
plot <- ggimage::ggbackground(plot, background = "https://i.imgur.com/t71V6K2.jpeg")

p_width  <- (680*(n_cols*1.1+0.2))/840
p_height <- (680*(n_rows*1.1+0.2))/840

ggsave(filename = "/home/balco/dev/lor-meta-report/data-dumps/new_cards.png", plot = plot, width = p_width, height = p_height, dpi = 840)


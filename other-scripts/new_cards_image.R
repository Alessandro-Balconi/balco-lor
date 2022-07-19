# parameters:
# images

library(tibble)
library(dplyr)
library(ggplot2)

# list of images to plot
images <- c(
  "06SH048","06BC039","06BC035","06BW021","06RU025T3","06RU025T10"
)

# to test format before cards come out
#images <- rep('06IO004', length(images))

# number of the current set
set_number <- substr(images[1], start = 2, stop = 2)

# number of rows in the plot
n_rows <- ceiling(length(images)/5)
n_cols <- ceiling(length(images) / n_rows)

# convert to df
data <- tibble(code = images) %>% 
  mutate(img = sprintf('https://dd.b.pvp.net/latest/set%s/en_us/img/cards/%s.png', set_number, code))

# add cohordinates
data <- data %>% 
  mutate(
    x = (rep(seq(from = 1, to = n_cols, by = 1), ceiling(nrow(data) / n_cols)))[1:length(images)],
    y = (rep(c(n_rows:1), rep(ceiling(nrow(data) / n_rows), n_rows)))[1:length(images)]
  )

# make plot
plot <- ggplot(data) +
  geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf), fill = 'mediumslateblue', alpha = 0.05) +
  expand_limits(y = c(0.5, n_rows+0.5), x = c(0.5, n_cols+0.5)) +
  theme_void() +
  ggimage::geom_image(aes(image = img, x = x, y = y), asp = (1024/680), size = size = 1/(n_cols+1))
  
# add background
plot <- ggimage::ggbackground(plot, background = "https://i.imgur.com/t71V6K2.jpeg")

p_width  <- (680*(n_cols*1.1+0.2))/840
p_height <- (680*(n_rows*1.1+0.2))/840

ggsave(filename = "/home/balco/dev/lor-meta-report/data_dumps/new_cards.png", plot = plot, width = p_width, height = p_height, dpi = 840)

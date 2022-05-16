library(tidyverse)

# list of images to plot
images <- c(
  "06BW006T3",
  "06BW009",
  "06BW007",
  "06BW016",
  "06BW035",
  "06BW015"#,
  #"06BW015T5"
)

# number of the current set
set_number <- 6

# number of columns, rows in the plot
n_rows <- 2
n_cols <- 3

# convert to df
data <- tibble(code = images) %>% 
  mutate(img = sprintf('https://dd.b.pvp.net/latest/set%s/en_us/img/cards/%s.png', set_number, code))

# add coordinates
data <- data %>% 
  mutate(
    x = (rep(seq(from = 1, to = n_cols, by = 1), ceiling(nrow(data) / n_cols)))[1:length(images)],
    y = (rep(seq(from = 1, to = n_rows, by = 1), ceiling(nrow(data) / n_rows)))[1:length(images)]
  )

plot <- ggplot(data) +
  geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf), fill = '#F5F5F5', color = 'white', alpha = 0.1) +
  ggimage::geom_image(aes(image = img, x = x, y = y), size = 0.2, asp = (1024/680)) +
  theme_void() +
  expand_limits(y = c(0.5, n_rows+0.5), x = c(0.5, n_cols+0.5))

plot <- ggimage::ggbackground(plot, background = "https://wallpaperaccess.com/full/3823169.jpg")

ggsave(filename = "/home/balco/dev/lor-meta-report/data_dumps/new_cards.png", plot = plot, width = 12, height = 8, dpi = 210)

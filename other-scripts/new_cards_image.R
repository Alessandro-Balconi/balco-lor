library(tidyverse)

# list of images to plot
images <- c("06FR029",
            "06FR037",
            "06NX031",
            "06MT044",
            "06RU025T1",
            "06RU025T2",
            "06RU025T9"
            )

# to test format before cards come out
#images <- rep('06IO004', length(images))

# number of the current set
set_number <- 6

# number of columns, rows in the plot
n_rows <- 2
n_cols <- 4

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
  ggimage::geom_image(aes(image = img, x = x, y = y), size = 1/(n_cols+1), asp = (1024/680)) +
  theme_void() +
  expand_limits(y = c(0.5, n_rows+0.5), x = c(0.5, n_cols+0.5))
  
# add background
plot <- ggimage::ggbackground(plot, background = "https://i.imgur.com/t71V6K2.jpeg")

ggsave(filename = "/home/balco/dev/lor-meta-report/data_dumps/new_cards.png", plot = plot, width = 3, height = 2, dpi = 840)

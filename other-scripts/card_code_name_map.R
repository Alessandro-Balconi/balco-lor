# champions names / codes / images from set JSONs
data <- map_dfr(
  .x = 1:5,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
)

data <- data %>% 
  select(cardCode, name, cost) %>%
  filter(nchar(cardCode) <= 8) # additional check because sometimes Riot messes up

data <- data %>% 
  arrange(cardCode)

with_gs4_quiet(sheet_write(data = data, ss = '1eWTbWBmyYYO2Qwj_q0xEHgZTi3NvtL3oOfz8sxC92Hs', sheet = "cards"))

# names of the spreadsheet to update
ss_names <- sheet_names('1eWTbWBmyYYO2Qwj_q0xEHgZTi3NvtL3oOfz8sxC92Hs')

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = '1eWTbWBmyYYO2Qwj_q0xEHgZTi3NvtL3oOfz8sxC92Hs', sheet = ., dimension = "columns"))
)

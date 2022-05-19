# check if packages are installed ------------------------------------------

packages <- c("tidyverse", "readxl", "qrcode", "magick")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

# data preprocessing ------------------------------------------------------
setwd("/home/agricolamz/work/materials/2022_badges")
library(tidyverse)

df <- readxl::read_xls("data/Мглин_каталог.xls")

colnames(df) <- c("id", "name_ru", "name_tr", "date", "link")

df %>% 
  mutate_all(function(x) ifelse(is.na(x), "", x)) %>% # remove NA in years 
  mutate(length = str_count(name_ru, "\n")+1,
         name_ru = str_split(name_ru, "\n"), # split multiperson tombs
         name_tr = str_split(name_tr, "\n"),
         date = str_split(date, "\n")) %>% 
  unnest_longer(c(name_ru, name_tr, date)) %>% 
  mutate(id_l = str_remove_all(id, "\\d{1,3}\\D?"), # split codes into numbers and letters
         id_d = str_remove_all(id, "^\\D{1,3}"),
         name_ru = str_replace_all(name_ru, "сын ", "сын~"),
         name_ru = str_replace_all(name_ru, "дочь ", "дочь~"),
         name_ru = str_replace_all(name_ru, "сын/дочь ", "сын/дочь~"),
         name_tr = str_replace_all(name_tr, "son of ", "son~of~"),
         name_tr = str_replace_all(name_tr, "daughter of ", "daughter~of~"),
         name_tr = str_replace_all(name_tr, "son/daughter of ", "son/daughter~of~")) ->
  for_the_badges

write_csv(for_the_badges, "data/for_the_badges.csv", na = "")

# create qr-codes ---------------------------------------------------------
for_the_badges <- read_csv("data/for_the_badges.csv", na = "NA")
for_the_badges %>% 
  distinct(id) %>% 
  mutate(link = str_c("https://sfira.ru/t/", id)) ->
  qr_codes

library(qrcode)
library(magick)
logo <- image_read_svg("images/logo.svg") %>% image_background('white') %>% image_border("white", "8x42")

qr_codes %>% 
  filter(nchar(link) <= 24) ->
  qr_codes_small

map(seq_along(qr_codes_small$id), function(i){
  path <- str_c("qr_codes/", qr_codes_small$id[i], ".png")
  png(path, width = 350, height = 350)
  plot(qr_code(qr_codes_small$link[i], ecl = "H"))
  dev.off()
  image_read(path) %>% 
    image_composite(logo,  offset = "+120+120") %>% 
    image_crop(geometry_area(296, 296, 27, 27)) %>% 
    image_write(path)
})

# There is a different pattern for longer links

qr_codes %>% 
  filter(nchar(link) > 24) ->
    qr_codes_big
logo <- image_read_svg("images/logo.svg") %>% image_background('white') %>% image_border("white", "6x40")

map(seq_along(qr_codes_big$id), function(i){
  path <- str_c("qr_codes/", qr_codes_big$id[i], ".png")
  png(path, width = 350, height = 350)
  plot(qr_code(qr_codes_big$link[i], ecl = "H"))
  dev.off()
  image_read(path) %>% 
    image_composite(logo,  offset = "+117+117") %>% 
    image_crop(geometry_area(300, 300, 25, 25)) %>% 
    image_write(path)
})

# create 1-tomb data ------------------------------------------------------
for_the_badges %>% 
  filter(length == 1) %>%  
  mutate(text = str_c("\\tombstonecard{", id_l, "}",
                      "{", id_d, "}",
                      "{qr_codes/", id, ".png}",
                      "{", name_ru, "}",
                      "{", name_tr, "}",
                      "{", date, "}")) %>% 
  pull(text) %>% 
  write_lines("data-1.tex")

# create 2-tomb data ------------------------------------------------------
for_the_badges %>% 
  filter(length == 2) %>% 
  group_by(id_l, id_d) %>% 
  mutate(name_ru = str_c(name_ru, collapse= "}{"),  # merge multiple lines
         name_tr = str_c(name_tr, collapse= "}{"),
         date = str_c(date, collapse= "}{")) %>% 
  distinct() %>% 
  mutate(text = str_c("\\tombstonecard{", id_l, "}",
                      "{", id_d, "}",
                      "{qr_codes/", id, ".png}",
                      "{", name_ru, "}",
                      "{", name_tr, "}",
                      "{", date, "}")) %>% 
  pull(text) %>% 
  write_lines("data-2.tex")

# create 2-tomb data ------------------------------------------------------
for_the_badges %>% 
  filter(length == 3) %>% 
  group_by(id_l, id_d) %>% 
  mutate(name_ru = str_c(name_ru, collapse= "}{"),  # merge multiple lines
         name_tr = str_c(name_tr, collapse= "}{"),
         date = str_c(date, collapse= "}{")) %>% 
  distinct() %>% 
  mutate(text = str_c("\\tombstonecardf{", id_l, "}",
                      "{", id_d, "}",
                      "{qr_codes/", id, ".png}",
                      "{", name_ru, "}",
                      "{", name_tr, "}",
                      "{", date, "}")) %>% 
  pull(text) %>% 
  write_lines("data-2.tex")

# compile pdfs ------------------------------------------------------------
name_prefix <- "MGL"

system("xelatex template-1.tex")
file.rename("template-1.pdf", str_c(name_prefix, "-1.pdf"))

system("xelatex template-2.tex")
file.rename("template-2.pdf", str_c(name_prefix, "-2.pdf"))

file.remove(c(list.files(pattern = "(log$)|(synctex.gz$)|(aux$)")))

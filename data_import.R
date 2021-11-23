library(readxl)
library(tidyverse)
if(!require(showtext)) {
  install.packages('showtext')
  library(showtext)
}

showtext_auto()

df <- read_excel('../univ_data/학교별합본.xlsx', skip = 4, na = '-', col_names = T, col_types = c(rep('text', 14), rep('numeric', 60)))


df <- df[, -c(10:14)]

df$연도 <- factor(df$연도, ordered = T)

df$시도 <- fct_relevel(df$시도,'전체', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')



df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도, 시도) |>
  summarise(경쟁률 = sum(지원자_전체_계) / sum(입학자_전체_계)) |> print(n = 100)

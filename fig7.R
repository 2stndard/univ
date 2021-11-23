glimpse(df)

df.전임비율 <- df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도, 시도) |>
  summarise(전임비율 = sum(재적생_전체_계) / sum(전임교원_계), 교원비율 = sum(재적생_전체_계) / sum(전임교원_계+비전임교원_계+시간강사_계), 재적생 = sum(재적생_전체_계), 전임교원수 = sum(전임교원_계), 교원수 = sum(전임교원_계+비전임교원_계+시간강사_계))

install.packages('GGally')

library(GGally)

df.전임비율

spread(df.전임비율, 연도, 전임비율, 교원비율, 재적생, 전임교원수, 교원수)

?spread

ggparcoord()
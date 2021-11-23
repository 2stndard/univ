df$학제 <- fct_relevel(df$학제,'전체', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')


df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  mutate(경쟁률 = 지원자_전체_계 / 입학자_전체_계) |>
  ggplot(aes(x = 연도,  y = 경쟁률)) + 
  geom_boxplot() + geom_jitter(alpha = 0.1, size = 1) +
  facet_wrap(~학제) + 
  labs(y = '대입경쟁률') +
#  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_discrete(label = c(12:21))

install.packages('ggridges')
library(ggridges)

df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
#  filter(연도 == '2021') |>
  mutate(경쟁률 = 지원자_전체_계 / 입학자_전체_계) |>
  select(연도, 학제, 시도, 경쟁률) |>
  ggplot(aes(x = 경쟁률, y = as.factor(학제))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

+ 
  facet_wrap(~연도)

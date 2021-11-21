df.spagetti <- df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도, 시도) |>
  summarise(경쟁률 = sum(지원자_전체_계) / sum(입학자_전체_계), 입학자 = sum(입학자_전체_계), 지원자 = sum(지원자_전체_계))

df.spagetti <- rbind(df.spagetti, 
df.spagetti |>
  group_by(연도) |>
  summarise(경쟁률 = sum(지원자) / sum(입학자), 입학자 = sum(입학자), 지원자 = sum(지원자), 시도 = '전체')
)

df.spagetti <- df.spagetti |>
  group_by(시도) |>
  mutate(diff = ifelse(c(0,diff(입학자))<0,"감소","증가"))

df.spagetti <- df.spagetti |>
  mutate(diff = ifelse(연도 == min(연도), '-', diff)) 


df.spagetti.temp <- df.spagetti |> mutate(시도1 = 시도) |> ungroup() |> select(-시도)

df.spagetti$시도 <- fct_relevel(df.spagetti$시도,'전체', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')


df.spagetti |>
  ggplot(aes(x = 연도,  y = 경쟁률)) + 
  geom_line(data = df.spagetti.temp, aes(group = 시도1), color = 'grey80') +
  # geom_point(data = df.spagetti |> filter(시도 != '전체'), aes(size = 지원자), color = 'black', fill = NA) +
  geom_point(data = df.spagetti |> filter(시도 != '전체'), aes(size = 입학자, color = diff), alpha = 0.6) +
  geom_line(aes(group = 시도), color = 'black') + 
  facet_wrap(~시도, nrow = 3) + 
  geom_line(data = df.spagetti.temp |> filter(시도1 == '전체'), aes(group = 1), color = 'black', linetype = 2) +
  labs(y = '대입경쟁률 평균') +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_discrete(label = c(12:21)) +
  scale_color_manual(name = '전년대비 \n입학생수 증감', values = c('감소' = 'blue', '증가' = 'red', '-' = 'black')) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = 15))


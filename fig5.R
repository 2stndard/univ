df.dumbell <- df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도, 시도) |>
  summarise(경쟁률 = sum(지원자_전체_계) / sum(입학자_전체_계), 입학자 = sum(입학자_전체_계), 지원자 = sum(지원자_전체_계))


df.dumbell <- rbind(df.dumbell, 
                    df.dumbell |>
                       group_by(연도) |>
                       summarise(경쟁률 = sum(지원자) / sum(입학자), 입학자 = sum(입학자), 지원자 = sum(지원자), 시도 = '전체')
)


df.dumbell <- df.dumbell |>
  group_by(시도) |>
  mutate(diff = c(0, 0, 0, diff(입학자, 3)), 
         lag = lag(입학자, 3), 
         증감률 = round(diff / lag * 100, 1), 
         pos = ifelse(증감률 >= 0, '증가', '감소')) |>
  ungroup()


View(df.dumbell)

df.dumbell$시도 <- fct_relevel(df.dumbell$시도,'전체', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')


df.dumbell |>
  filter(연도 == 2021) |>
  ggplot(aes(x = 시도, xend = 시도, y = 0, yend = 증감률)) + 
  geom_segment(aes(group = 시도, colour = as.factor(pos)), size = 2, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(aes(x = 시도, y = 증감률, label = ifelse(pos == '감소', paste0(증감률, '%p'), NA)), vjust = 1.5) +
  geom_text(aes(x = 시도, label = ifelse(pos == '증가', paste0(증감률, '%p'), NA)), vjust = -1.5) +
  theme(legend.position="bottom") +
  ylim(-20, 1)  + 
  scale_color_manual(values = c('red', 'blue')) +
  labs(title = '2019년 대비 2021년 지역별 고등교육기관 입학생수 증감율', y = '증감율(%p)', color = '증감') +
  theme_bw()



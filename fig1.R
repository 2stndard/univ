df.미래 <- read_excel('./미래1.xlsx', na = '-', col_names = F, col_types = c('numeric', 'numeric', 'numeric'))

names(df.미래) <- c('연도', '학생수', '정원')

df.미래 |>
  group_by(연도) |>
  summarise(입학연령인구 = sum(학생수), 입학정원 = 474996)


df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도) |>
  summarise(전체입학자 = sum(입학자_전체_계), 전체정원 = sum(입학정원_전체)) |>
  ggplot(aes(x = 연도)) + 
  geom_line(aes(y = 전체입학자, group = 1, color = '입학자수')) + 
  geom_point(aes(y = 전체입학자, group = 1, color = '입학자수', shape = '입학자수', size = '입학자수')) +
  geom_line(aes(y = 전체정원, group = 1, color = '전체정원')) +
  geom_point(aes(y = 전체정원, group = 1, color = '전체정원', shape = '전체정원', size = '전체정원')) +
  geom_line(data = df.미래 |> filter(연도 <= 2021), aes(x = as.factor(연도+1), y = 학생수, group = 1, color = '전년 고3생수(22년이후는 추정치)')) + 
  geom_line(data = df.미래 |> filter(연도 > 2021), aes(x = as.factor(연도+1), y = 학생수, group = 1, color = '전년 고3생수(22년이후는 추정치)'), linetype = 2) + 
  geom_point(data = df.미래, aes(x = as.factor(연도+1), y = 학생수, group = 1, color = '전년 고3생수(22년이후는 추정치)', shape = '전년 고3생수(22년이후는 추정치)', size = '전년 고3생수(22년이후는 추정치)')) +
  geom_line(data = df.미래, aes(x = as.factor(연도), y = 정원, group = 1, color = '21년정원')) +
  geom_point(data = df.미래, aes(x = as.factor(연도), y = 정원, group = 1, color = '21년정원', shape = '21년정원', size = '21년정원')) +
#  scale_color_manual(name = '구분', values = c('입학자수' = 'grey50', '전체정원' = 'red', '전년 고3생수' = 'black', '21년정원' = 'red4')) +
  scale_shape_manual(name = '구분', values = c('입학자수' = 1, '전체정원' = 2, '전년 고3생수(22년이후는 추정치)' = 3, '21년정원' = 4, '고3생수 추세선' = 0)) +
  scale_size_manual(name = '구분', values = c('입학자수' = 2, '전체정원' = 2, '전년 고3생수(22년이후는 추정치)' = 2, '21년정원' = 2, '고3생수 추세선' = 0)) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  geom_smooth(data = df.미래, aes(x = 연도-2011+1, y = 학생수), formula = y ~ x, method = 'loess') + 
  scale_color_manual(name = '구분', values = c('입학자수' = 'grey50', '전체정원' = 'red', '전년 고3생수(22년이후는 추정치)' = 'black', '21년정원' = 'red4', '고3생수 추세선' =  'blue')) + 
  geom_text(data = df.미래, aes(x = max(factor(df.미래$연도, ordered = TRUE)), y = 474996, label = '475k'), hjust = -0.1, color = 'darkred') +
  geom_text(data = df.미래, aes(x = as.factor(연도+1), y = 학생수, 
                              label = paste0(round(학생수/1000, 0), 'k')), vjust = -1) +
  geom_text(aes(y = 전체정원, 
                              label = paste0(round(전체정원/1000, 0), 'k')), vjust = 2, color = 'red') +
  geom_text(aes(y = 전체입학자, 
                label = paste0(round(전체입학자/1000, 0), 'k')), vjust = 2, color = 'grey50') +
  labs(y = '학생수') +
  geom_vline(aes(xintercept = 11), color = 'grey80', linetype = 2) +
  annotate('rect', xmin = 8.5, xmax = 13.5, ymin = 380000, ymax = 580000, alpha = 0.2) +
  annotate('rect', xmin = 23.5, xmax = 27.5, ymin = 260000, ymax = 380000, alpha = 0.2) +
  annotate('text', x = 11, y = 620000, label = '1차 충격') + 
  annotate('segment', x = 11, y = 600000, xend = 11, yend = 580000, arrow = arrow(length = unit(0.01, "npc"))) + 
  annotate('text', x = 22, y = 325000, label = '2차 충격') + 
  annotate('segment', x = 23, y = 325000, xend = 23.5, yend = 325000, arrow = arrow(length = unit(0.01, "npc")))
  

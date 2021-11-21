df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도) |>
  summarise(전체입학자 = sum(입학자_전체_계), 전체정원 = sum(입학정원_전체)) |>
  ggplot(aes(x = 연도)) + 
  geom_line(aes(y = 전체입학자, group = 1, color = '입학자수')) + 
  geom_point(aes(y = 전체입학자, group = 1, color = '입학자수', shape = '입학자수', size = '입학자수')) +
  geom_line(aes(y = 전체정원, group = 1, color = '전체정원')) +
  geom_point(aes(y = 전체정원, group = 1, color = '전체정원', shape = '전체정원', size = '전체정원')) +
  geom_line(data = df.미래, aes(x = as.factor(연도+1), y = 학생수, group = 1, color = '전년 고3생수')) + 
  geom_point(data = df.미래, aes(x = as.factor(연도+1), y = 학생수, group = 1, color = '전년 고3생수', shape = '전년 고3생수', size = '전년 고3생수')) +
  geom_line(data = df.미래, aes(x = as.factor(연도), y = 정원, group = 1, color = '21년정원')) +
  geom_point(data = df.미래, aes(x = as.factor(연도), y = 정원, group = 1, color = '21년정원', shape = '21년정원', size = '21년정원')) +
  scale_color_manual(name = '구분', values = c('입학자수' = 'grey50', '전체정원' = 'red', '전년 고3생수' = 'black', '21년정원' = 'red4')) +
  scale_shape_manual(name = '구분', values = c('입학자수' = 1, '전체정원' = 2, '전년 고3생수' = 3, '21년정원' = 4)) +
  scale_size_manual(name = '구분', values = c('입학자수' = 2, '전체정원' = 2, '전년 고3생수' = 2, '21년정원' = 2)) + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = 'bottom')  -> p

p + 
  geom_smooth(data = df.미래, aes(x = 연도-2011+1, y = 학생수), formula = y ~ x, color = 'green', method = 'loess')

df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  mutate(경쟁률 = 지원자_전체_계 / 입학자_전체_계) |>
  ggplot(aes(x = 연도,  y = 경쟁률)) + 
  geom_boxplot() + geom_jitter(alpha = 0.2, size = 1) +
  facet_wrap(~시도, nrow = 3) + 
  labs(y = '대입경쟁률') +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_discrete(label = c(12:21))

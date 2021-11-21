View(df.spagetti)

df.spagetti <- df.spagetti |>
  mutate(수도권 =  case_when(시도 == '전체' ~ '전체', 
                            시도 == '서울' ~ '서울', 
                            시도 == '인천' ~ '인천', 
                            시도 == '경기' ~ '경기', 
                            TRUE ~ '비수도권')
  )

df.spagetti |>
  ggplot(aes(x = 연도,  y = 경쟁률)) + 
  geom_line(aes(group = 시도, color = as.factor(수도권), linetype = as.factor(수도권))) + 
  scale_color_manual(name = '수도권 vs 비수도권', values = c('서울' = 'darkred', '인천' = 'red', '경기' = 'purple', '비수도권' = 'grey70', '전체' = 'black')) + 
  scale_linetype_manual(name = '수도권 vs 비수도권', values = c('서울' = 1, '인천' = 1, '경기' = 1, '비수도권' = 1, '전체' = 2)) + 
  theme(legend.position = 'bottom')



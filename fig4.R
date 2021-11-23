library(patchwork)

glimpse(df)

df |> 
  group_by(연도, 시도) |>
  summarise(전임교원수 = sum(전임교원_계), 재학생 = sum(재학생_전체_계), 전임대재학생 = 재학생 / 전임교원수) |>
  ggplot(aes(x = 연도, y =  전임대재학생)) +
  geom_line(aes(group = 시도)) +
  facet_wrap(~시도)



i <- 1
plot_list = list()

for(i in 1:10) {
df |> 
  group_by(연도, 시도) |>
  summarise(전임 = sum(전임교원_계), 비전임 = sum(비전임교원_계), 강사 = sum(시간강사_계), 
              전임비율 = 전임 / (전임+비전임+강사)*100, 비전임비율 = 비전임 / (전임+비전임+강사)*100,
              강사비율 = 강사 / (전임+비전임+강사)*100) -> df.강사비율
  
df.강사비율 |> 
  filter(연도 == i+2011) |>
  mutate(시도1 = fct_reorder(시도, 전임비율, desc)) -> df.강사비율

df.강사비율 <- df.강사비율|> ungroup() |> 
  select(-c('전임', '비전임', '강사')) |>
  gather(key = '채용', value = '비율', 3:5)

mean.전임강사비율 <- round(mean(df |> filter(연도 == i + 2011) |> mutate(비율 = sum(전임교원_계)/sum(전임교원_계 + 비전임교원_계 + 시간강사_계)) |> pull(비율))*100, 1)

facet.연도 <- 2011 + i

df.강사비율 |> 
  ggplot(aes(x = 시도1, y = 비율)) +
  geom_col(aes(fill = 채용)) +
  geom_text(aes(label = round(비율, 1)),
            position = position_stack(vjust = .5), size = 4) +
  geom_hline(yintercept = mean.전임강사비율, 
             color = 'red', 
             linetype = 2) +
  annotate('text', x = mean(range(as.integer(df.강사비율$시도1))), y = mean.전임강사비율, 
           label = paste0(facet.연도, ' 전임강사 비율 평균 :', mean.전임강사비율)) +
  labs(title = facet.연도, x = '') + labs(fill = '채용비율')-> p

plot_list[[i]] <- p

}


(plot_list[[1]] +  plot_list[[2]]) / (plot_list[[3]] + plot_list[[4]]) / (plot_list[[5]] +  plot_list[[6]]) / (plot_list[[7]] + plot_list[[8]]) / (plot_list[[9]] +  plot_list[[10]])  + plot_layout(guides = "collect")

ggsave('2.pdf', w = 24, h = 15, units = 'in', dpi = 1200)

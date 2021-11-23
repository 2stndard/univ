glimpse(df)

df.전임비율 <- df |>
  filter(학제 %in% c('대학교', '교육대학', '산업대학', '전문대학(2년제)', '전문대학(3년제)', '전문대학(4년제)')) |>
  group_by(연도, 시도) |>
  summarise(전임비율 = sum(재적생_전체_계) / sum(전임교원_계), 교원비율 = sum(재적생_전체_계) / sum(전임교원_계+비전임교원_계+시간강사_계), 재적생 = sum(재적생_전체_계), 전임교원수 = sum(전임교원_계), 교원수 = sum(전임교원_계+비전임교원_계+시간강사_계))

df.전임비율 <- rbind(df.전임비율, df |>
  group_by(연도) |>
  summarise(시도 = '전체', 전임비율 = sum(재적생_전체_계) / sum(전임교원_계), 교원비율 = sum(재적생_전체_계) / sum(전임교원_계+비전임교원_계+시간강사_계), 재적생 = sum(재적생_전체_계), 전임교원수 = sum(전임교원_계), 교원수 = sum(전임교원_계+비전임교원_계+시간강사_계)))


View(df.전임비율)

?pivot_longer

df.전임비율.longer <- df.전임비율 |> pivot_longer(cols = c(전임비율, 교원비율, 재적생, 전임교원수, 교원수), names_to = '구분')

MySpecial <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
#  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

df.전임비율.longer |>
  filter(연도 >= 2018, 구분 == '전임비율') |>
  ggplot(aes(x = 연도)) +
  geom_line(aes(y = value, group = 시도, color = 시도), size = 1) + 
  geom_point(aes(y = value, group = 시도, color = 시도), size = 2) + 
  ggrepel::geom_text_repel(data = df.전임비율.longer %>% filter(연도 == 2018, 구분 == '전임비율'),
                  aes(y = value, label = paste0(시도, " - ", round(value, 1), "명")) ,
                  hjust = "left", 
                  fontface = "bold",  
                  size = 3, 
                  nudge_x = -.45, 
                  direction = "y") + 
  ggrepel::geom_text_repel(data = df.전임비율.longer %>% filter(연도 == 2021, 구분 == '전임비율'),
                           aes(y = value, label = paste0(시도, " - ", round(value, 1), "명")) ,
                           hjust = "right", 
                           fontface = "bold",  
                           size = 3, 
                           nudge_x = .5, 
                           direction = "y") + 
  geom_label(data = df.전임비율.longer %>% filter(연도 %in% c(2019, 2020), 구분 == '전임비율', 시도 != '전체'),
             aes(y = value, label = round(value, 1), color = 시도), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) + 
  geom_line(data = df.전임비율.longer %>% filter(연도 >= 2018, 구분 == '전임비율', 시도 == '전체'),
            aes(y = value, group = 시도, color = 시도), size = 1, color = 'black') + 
  geom_label(data = df.전임비율.longer %>% filter(연도 %in% c(2019, 2020), 구분 == '전임비율', 시도 == '전체'),
             aes(y = value, label = round(value, 1)), 
             size = 3, 
             label.padding = unit(0.2, "lines"), 
             label.size = 1.0, color = 'white', fill = 'black') + 
  geom_point(data = df.전임비율.longer %>% filter(연도 %in% c(2018, 2021), 구분 == '전임비율', 시도 == '전체'), 
             aes(y = value, group = 시도), size = 2, color = 'black') + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(y = '전임교원 1인당 재적학생수')


df.age <- read_excel('./연령별 입학생수.xlsx', skip = 5, na = '-', sheet = '입학생수', col_names = T, col_types = c(rep('text', 2), rep('numeric', 73)))

df.age <- pivot_longer(df.age, 3:75, names_to = 'age')



df.age1 <- df.age |>
  filter(!(age %in% c('합계','9이하',	'10',	'11',	'12',	'13',	'14',	'15',	'16',	'17',	'18',	'19')), 학교급 != '전체') |>
  mutate(div.age = case_when(
    age %in% c('9이하') ~ '9세이하', 
    age %in% as.character(10:14) ~ '10~14세이하',
    age %in% as.character(15:19) ~ '15~19세이하',
    age %in% as.character(20:24) ~ '20~24세이하',
    age %in% as.character(25:29) ~ '25~29세이하',
    age %in% as.character(30:34) ~ '30~34세이하',
    age %in% as.character(35:39) ~ '35~39세이하',
    age %in% as.character(40:44) ~ '40~44세이하',
    age %in% as.character(45:49) ~ '45~49세이하',
    age %in% as.character(50:54) ~ '50~54세이하',
    age %in% as.character(55:59) ~ '55~59세이하',
    age %in% as.character(60:64) ~ '60~64세이하',
    age %in% as.character(65:69) ~ '65~69세이하',
    age %in% as.character(70:74) ~ '70~74세이하',
    age %in% as.character(75:79) ~ '75~79세이하',
    age %in% c('80이상') ~ '80이상'
  )) |>
  group_by(연도, div.age) |>
  summarise(sum = sum(value)) |> 
  group_by(연도) |>
  mutate(total = sum(sum), per = round(sum / total, 3))

df.age1 <- df.age |>
  filter(!(age %in% c('합계','9이하',	'10',	'11',	'12',	'13',	'14',	'15',	'16',	'17',	'18',	'19')), 학교급 != '전체') |>
  mutate(div.age = case_when(
    age %in% as.character(20:24) ~ '20대초',
    age %in% as.character(25:29) ~ '20대후',
    age %in% as.character(30:39) ~ '30대',
    age %in% as.character(40:49) ~ '40대',
    age %in% as.character(50:59) ~ '50대',
    age %in% as.character(60:69) ~ '60대',
    age %in% as.character(70:79) ~ '70대',
    age %in% c('80이상') ~ '80대이상'
  )) |>
  group_by(연도, div.age) |>
  summarise(sum = sum(value)) |> 
  group_by(연도) |>
  mutate(total = sum(sum), per = round(sum / total, 3))

df.age1$div.age <- fct_relevel(df.age1$div.age,'20대초','20대후', '30대', '40대', '50대', '60대', '70대', '80대이상')


# df.age1 |>
#   ggplot(aes(x = 연도, y = per, fill = div.age)) +
#   geom_bar(stat = 'identity', position = 'fill',  width = .6, colour="black", lwd=0.1) + 
#   geom_text(aes(x = 연도, y = per, label = ifelse(per > 0.04, paste0(per * 100,"%"), '')), 
#             position=position_stack(vjust= 0.5), size=4) +
# #  geom_text(aes(label=per), position=position_stack(vjust=0.5), colour="black") +
#   coord_flip() +
#   scale_y_continuous(labels = scales::percent_format(suffix = "%", prefix = "")) +
#   #  scale_y_discrete(labels = scales::percent_format()) +
#   labs(x="연도", y="연령별 구성비", fill = '연령대') +
#   theme(axis.line = element_line(size=1, colour = "black"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.border = element_blank(), panel.background = element_blank()) 
  
df.age1 |>
  ggplot(aes(x = 연도, y = per, fill = fct_rev(div.age))) +
  geom_col(position = 'fill',  width = .8, lwd=0.1) + 
  geom_text(aes(x = 연도, y = per, label = ifelse(per > 0.04, paste0(per * 100,"%"), '')), 
            position=position_stack(vjust= 0.5), size=4) +
  #  geom_text(aes(label=per), position=position_stack(vjust=0.5), colour="black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(suffix = "%", prefix = "")) +
  #  scale_y_discrete(labels = scales::percent_format()) +
  labs(x="연도", y="연령별 구성비", fill = '연령대') +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette="Paired")

library(DT)

df.age2 <- df.age |>
  filter(!(age %in% c('합계')), 학교급 != '전체') |>
  mutate(div.age = case_when(
    age %in% c('9이하') ~ '17세이하',
    age %in% as.character(10:17) ~ '17세이하', 
    age %in% as.character(18) ~ '18세', 
    age %in% as.character(19) ~ '19세', 
    age %in% as.character(20:24) ~ '20대초',
    age %in% as.character(25:29) ~ '20대후',
    age %in% as.character(30:39) ~ '30대',
    age %in% as.character(40:49) ~ '40대',
    age %in% as.character(50:59) ~ '50대',
    age %in% as.character(60:69) ~ '60대',
    age %in% as.character(70:79) ~ '70대',
    age %in% c('80이상') ~ '80대이상'
  )) |>
  group_by(연도, div.age) |>
  summarise(sum = sum(value)) |> 
  group_by(연도) |>
  mutate(total = sum(sum), per = round(sum / total, 3))


df.age2 |>
  select(1, 2, 5) |>
  pivot_wider(id_cols = 연도, names_from = div.age, values_from = per) |>
  write.csv('clipboard', sep = '\t')
  
  
df.age2 |>
  ggplot(aes(x = reorder(연도, desc(연도)), y = per, fill = fct_rev(div.age))) +
  geom_col(position = 'fill',  width = .8, lwd=0.1) + 
  geom_text(aes(x = 연도, y = per, label = ifelse(per > 0.04, paste0(per * 100,"%"), '')), 
            position=position_stack(vjust= 0.5), size=4) +
  #  geom_text(aes(label=per), position=position_stack(vjust=0.5), colour="black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(suffix = "%", prefix = "")) +
  #  scale_y_discrete(labels = scales::percent_format()) +
  labs(x="연도", y="연령별 구성비", fill = '연령대') +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette="Paired")

library(readxl)
library(tidyverse)
library(showtext)
library(patchwork)
showtext_auto()


df <- read_excel('C:/R/git/univ1/15년 고등교육기관 학과별 입학정원 입학 지원 재적 재학 휴학 외국인유학생 졸업 교원.xlsx', skip = 14, na = '-', sheet = '학과별 교육통계자료', col_types = c(rep('text', 8), rep('numeric', 54)), col_names = T)


df.전처리 <- df |>
  filter(학제 %in% c('대학교', '전문대학'))

df.전처리$대계열 <- fct_relevel(df.전처리$대계열, c('인문계열', '사회계열', '자연계열', '공학계열', '예체능계열', '교육계열', '의약계열'))

df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram() +
  labs(x = '재적학생수', y = '학과수')



bin60<- df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(bins = 60) +
  labs(title = 'bin = 60', x = '재적학생수', y = '학과수')

bin120<- df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(bins = 120) +
  labs(title = 'bin = 120', x = '재적학생수', y = '학과수')


binwidth500 <- df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(binwidth = 500) +
  labs(title = 'binwidth = 500', x = '재적학생수', y = '학과수')

binwidth1000 <- df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(binwidth = 1000) +
  labs(title = 'binwidth = 1000', x = '재적학생수', y = '학과수')

(bin60 + bin120) / (binwidth1000 + binwidth500)

df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(bins = 60) +
  labs(x = '재적학생수', y = '학과수') + 
#  scale_y_continuous(trans = 'log',
 #                    breaks = c(1, 10, 100, 1000), minor_breaks = NULL) +
  scale_x_continuous(trans = 'log',
                   breaks = c(1:5, 10, 100, 1000, 10000, 50000), minor_breaks = NULL)

df.전처리 |>
  filter(재적생_전체_계 > 0) |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(bins = 60, fill="green", col="dark green") +
  labs(x = '재적학생수', y = '학과수')

df.전처리 |>
  filter(재적생_전체_계 < 100, 재적생_전체_계 >= 1) |>
  group_by(재적생_전체_계) |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(binwidth = 1, fill="green", col="dark green") +
  labs(x = '재적학생수', y = '학과수')


df.전처리 |>
  filter(재적생_전체_계 < 100) |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_histogram(binwidth = 1, fill="green", col="dark green") +
  labs(x = '재적학생수', y = '학과수')

+ 
  scale_y_continuous(trans = 'log10',
                     breaks = c(1:5, 10, 100, 1000)) +
  scale_x_continuous(trans = 'log10',
                     breaks = c(1:5, 10, 100, 1000, 10000, 50000), minor_breaks = NULL)


df.전처리 |>
  ggplot(aes(x = 재적생_전체_계)) +
  geom_density() +
  labs(x = '재적학생수', y = '학과수') + 
  scale_y_continuous(trans = 'sqrt',
                     breaks = c(1:5, 10, 100, 1000)) +
  scale_x_continuous(trans = 'log2',
                     breaks = c(1:5, 10, 100, 1000, 10000, 50000), minor_breaks = NULL)

df.전처리 |>
  group_by(재적생_전체_계) |>
  count() |>
  arrange(desc(n))|>
  head(100) |>
  View()

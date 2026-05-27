library(dplyr)
library(ggplot2)
library(janeaustenr)
library(tokenizers)
library(purrr)
library(stringr)
library(tidyr)

books_raw <- austen_books()

books_ru_map <- c(
  "Sense & Sensibility" = "Разум и чувства",
  "Pride & Prejudice" = "Гордость и предубеждение",
  "Mansfield Park" = "Мэнсфилд-парк",
  "Emma" = "Эмма",
  "Northanger Abbey" = "Нортенгерское аббатство",
  "Persuasion" = "Доводы рассудка"
)

chapters_data <- books_raw %>%
  group_by(book) %>%
  mutate(
    chapter = cumsum(str_detect(text, regex("^chapter\\s+[0-9ivxlc]+", ignore_case = TRUE)))
  ) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  group_by(book, chapter) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop")

chapter_stats <- chapters_data %>%
  mutate(
    sentences = map(text, tokenize_sentences),
    sentence_lengths = map(
      sentences,
      ~ sapply(.x, function(s) {
        words <- unlist(tokenize_words(s))
        length(words)
      })
    ),
    mean_sentence_length = map_dbl(
      sentence_lengths,
      ~ mean(.x[.x > 0], na.rm = TRUE)
    ),
    book_ru = recode(book, !!!books_ru_map)
  )

my_palette <- c(
  "Гордость и предубеждение" = "#8FA876",
  "Разум и чувства" = "#E07A5F",
  "Эмма" = "#7B9ACC",
  "Мэнсфилд-парк" = "#C38D9E",
  "Доводы рассудка" = "#81B29A",
  "Нортенгерское аббатство" = "#F2CC8F"
)

subtitle_text <- "Распределение средней длины предложения по главам"

final_plot <- ggplot(chapter_stats, aes(x = book_ru, y = mean_sentence_length, fill = book_ru)) +
  geom_boxplot(
    width = 0.58,
    outlier.shape = NA,
    alpha = 0.9,
    color = "#2B2B2B",
    linewidth = 0.35
  ) +
  geom_jitter(
    width = 0.12,
    size = 1.7,
    alpha = 0.45,
    color = "#2B2B2B"
  ) +
  coord_flip() +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "Средняя длина предложения по главам в романах Джейн Остин",
    subtitle = subtitle_text,
    x = NULL,
    y = "Средняя длина предложения, слов",
    caption = "Источник данных: пакет janeaustenr"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 11, color = "#222222"),
    axis.text.x = element_text(size = 10, color = "#222222"),
    axis.title.x = element_text(size = 11),
    plot.caption = element_text(size = 9, color = "#555555")
  )

print(final_plot)

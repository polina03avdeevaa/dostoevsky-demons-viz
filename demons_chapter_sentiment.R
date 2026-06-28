# Динамика эмоционального напряжения по главам (AFINN + UDPipe)

# Шаг 0. Загрузка необходимых пакетов
library(udpipe)
library(tidyverse)
library(stringr)
library(ggplot2)
library(svglite)


# Шаг 1. Загрузка модели udpipe для русского
model <- udpipe_load_model("russian-syntagrus-ud-2.5-191206.udpipe")


# Шаг 2. Загрузка текста и разбивка на главы
# 2.1. Загрузка текста
text_raw <- readLines("Demons.txt", encoding = "UTF-8") |> 
  paste(collapse = "\n")

# 2.2. Разбиение текста по шаблону "Глава ..." (новая строка + слово Глава)
chunks <- str_split(text_raw, "\n(?=Глава\\s+[^\\n]+\\.)")[[1]]

# 2.3. Извлечение заголовков и создание таблицы
chapters <- tibble(
  raw_text = chunks,
  title = str_extract(chunks, "^Глава\\s+[^\\n]+\\.?") |> 
    str_squish(),
  chapter_id = seq_along(chunks)
) |> 
  filter(!is.na(title) & nchar(raw_text) > 200)

# 2.4. Добавляется краткое имя для подписей
chapters <- chapters |> 
  mutate(short_title = str_remove(title, "Глава\\s+") |> 
           str_trunc(30))


# Шаг 3. Анализ тональности каждой главы через udpipe + AFINN
if (!exists("afinn_ru")) {
  afinn_ru <- tribble(
    ~word, ~score,
    "хороший", 3, "плохой", -3, "любовь", 4, "ненависть", -4,
    "свет", 2, "тьма", -2, "радость", 3, "ужас", -3,
    "смерть", -5, "бес", -4, "добро", 4, "зло", -5
  )
}

# Функция для обработки одной главы
get_chapter_sentiment <- function(text, model, afinn_dict) {
  anno <- udpipe_annotate(model, 
                          x = text, 
                          doc_id = "chapter") |> 
    as.data.frame()
  
  content_pos <- c("NOUN", "ADJ", "VERB", "ADV")
  tokens <- anno |> 
    filter(upos %in% content_pos) |> 
    pull(lemma) |> 
    tolower()
  
  sentiment_df <- tibble(lemma = tokens) |> 
    inner_join(afinn_dict, by = c("lemma" = "word"))
  
  if (nrow(sentiment_df) == 0) return(tibble(score_sum = 0, 
                                             score_mean = 0, 
                                             n_words = 0))
  tibble(
    score_sum = sum(sentiment_df$score),
    score_mean = mean(sentiment_df$score),
    n_words = nrow(sentiment_df)
  )
}

# Применяем ко всем главам
sentiment_results <- chapters |> 
  mutate(sentiment = map(raw_text, 
                         ~get_chapter_sentiment(.x, model, afinn_ru))) |> 
  unnest_wider(sentiment)


# Шаг 4. График динамики эмоционального напряжения
p1 <- ggplot(sentiment_results, aes(x = chapter_id, 
                                    y = score_mean)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "#666666", 
             linewidth = 0.4) +
  geom_ribbon(data = sentiment_results |> 
                mutate(y_pos = ifelse(score_mean > 0, 
                                      score_mean, 0)),
              aes(ymin = 0, 
                  ymax = y_pos), 
              fill = "#6B7B5A", 
              alpha = 0.4) +
  geom_ribbon(data = sentiment_results |> 
                mutate(y_neg = ifelse(score_mean < 0, 
                                      score_mean, 0)),
              aes(ymin = y_neg, ymax = 0), 
              fill = "#8B3A3A",
              alpha = 0.4) +
  geom_smooth(method = "loess", 
              se = FALSE, 
              color = "#2C2C2C", 
              linewidth = 0.8, 
              linetype = "dotted") +
  geom_line(color = "#2C2C2C", 
            linewidth = 0.7) +
  geom_point(size = 2,
             color = "#2C2C2C",
             alpha = 0.6) +
  scale_x_continuous(
    breaks = sentiment_results$chapter_id, 
    labels = 1:24,                          
    limits = range(sentiment_results$chapter_id) 
  ) +
  scale_y_continuous(limits = c(-3, 3), 
                     breaks = seq(-3, 3, by = 1)) +
  labs(x = "Глава", 
       y = "Средняя тональность") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#2C2C2C", 
                               size = 11),
    axis.text.y = element_text(color = "#2C2C2C", 
                               size = 11),
    axis.title = element_text(color = "#2C2C2C", 
                              size = 12),
    plot.background = element_rect(fill = "#F2EFE9", 
                                   color = NA),
    panel.background = element_rect(fill = "#F2EFE9", 
                                    color = NA)
  )


# Шаг 5. Сохранение в SVG (в текущую папку)
svg_file <- "demons_chapter_sentiment.svg"
svglite(svg_file, width = 8, height = 5, bg = "#F2EFE9")
print(p1)
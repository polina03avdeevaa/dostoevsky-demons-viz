# Анализ тональности романов Достоевского

# Шаг 0. Загрузка пакетов
library(tidyverse)
library(udpipe)
library(rulexicon)
library(showtext)
library(svglite)

# Шаг 1. Загрузка текстов
load_text <- function(file_path) {
  text <- readChar(file_path, file.info(file_path)$size, useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  return(text)
}

text_files <- list(
  "Demons" = "Demons.txt",
  "Crime_and_punishment" = "Crime and punishment.txt",
  "Idiot" = "Idiot.txt",
  "The_Brothers_Karamazov" = "The Brothers Karamazov.txt",
  "The_Adolescent" = "The Adolescent.txt"
)

texts <- map(text_files, load_text)

# Шаг 2. Создание корпуса
dostoevsky_corpus <- tibble(
  id = names(texts),
  text = unlist(texts)
)

# Шаг 3. Загрузка модели UDPipe
model_file <- "russian-syntagrus-ud-2.5-191206.udpipe"

if (!file.exists(model_file)) {
  udpipe_download_model(language = "russian-syntagrus")
}

rus <- udpipe_load_model(file = model_file)

# Шаг 4. Аннотация (с разбивкой на части для больших текстов)
annotate_safe <- function(text, doc_id, model, chunk_size = 50000) {
  if (nchar(text) <= chunk_size) {
    result <- udpipe_annotate(model, x = text, doc_id = doc_id)
    return(as_tibble(result))
  }
  
  # Разбиваем на части
  chunks <- split_text(text, chunk_size)
  results <- map2(chunks, seq_along(chunks), function(chunk, i) {
    udpipe_annotate(model, x = chunk, doc_id = paste0(doc_id, "_", i))
  }) |> 
    map(as_tibble) |> 
    bind_rows()
  
  results$doc_id <- doc_id
  return(results)
}

split_text <- function(text, chunk_size) {
  n <- nchar(text)
  starts <- seq(1, n, by = chunk_size)
  map(starts, ~ substr(text, ., min(. + chunk_size - 1, n)))
}

# Аннотируем все тексты
dostoevsky_ann_tibble <- dostoevsky_corpus |> 
  mutate(annotation = map2(text, id, ~ annotate_safe(.x, .y, rus))) |> 
  select(-text) |> 
  unnest(annotation)

# Шаг 5. Загрузка словаря AFINN
afinn <- rulexicon::hash_sentiment_afinn_ru
colnames(afinn) <- c("token", "score")

# Шаг 6. Разбивка на чанки по 100 слов
ann_clear <- dostoevsky_ann_tibble |> 
  filter(upos != "PUNCT") |> 
  select(lemma, doc_id) |> 
  rename(token = lemma) |> 
  group_by(doc_id) |> 
  mutate(chunk = (row_number() - 1) %/% 100 + 1) |> 
  ungroup()

# Шаг 7. Расчет тональности
dostoevsky_sent <- ann_clear |> 
  inner_join(afinn, by = "token") |> 
  select(doc_id, token, chunk, score)

# Шаг 8. Переименование романов
dostoevsky_total <- dostoevsky_sent |> 
  mutate(doc_id = recode(doc_id,
                         "Demons" = "«Бесы»",
                         "Idiot" = "«Идиот»",
                         "Crime_and_punishment" = "«Преступление и наказание»",
                         "The_Brothers_Karamazov" = "«Братья Карамазовы»",
                         "The_Adolescent" = "«Подросток»"
  ))

# Шаг 9. Результаты
sentiment_results <- dostoevsky_total |> 
  group_by(doc_id) |> 
  summarise(
    avg_sentiment = mean(score),
    n_words = n(),
    .groups = "drop"
  ) |> 
  arrange(desc(avg_sentiment))

# Шаг 10. Подготовка к визуализации
sentiment_results <- sentiment_results |> 
  mutate(doc_id = fct_reorder(doc_id, avg_sentiment))

# Шаг 11. Шрифты
tryCatch({
  font_add("Angst", regular = "Angst-Bold.otf")
  font_add_google("Inter", "Inter")
  showtext_auto()
}, error = function(e) {
  message("Не удалось загрузить шрифты: ", e$message)
})

# Шаг 12. График
sentiment_plot <- ggplot(sentiment_results, aes(y = doc_id, x = avg_sentiment)) +
  geom_col(aes(fill = ifelse(doc_id == "«Бесы»", "#2B3B60", "#B59D81")),
           color = NA, width = 0.7
  ) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "#666666",
             linewidth = 0.4) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(-1, NA), 
                     expand = expansion(add = c(0.1, 0))) +
  scale_y_discrete(position = "right") +
  labs(y = NULL, x = "Среднее значение эмоциональности") +
  theme_minimal(base_family = "Inter") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#2C2C2C", 
                               size = 11),
    axis.text.y = element_text(color = "#2C2C2C", 
                               size = 11, 
                               hjust = 0),
    axis.title.x = element_text(color = "#2C2C2C",
                                size = 11),
    plot.background = element_rect(fill = "#F2EFE9", 
                                   color = NA),
    panel.background = element_rect(fill = "#F2EFE9", 
                                    color = NA)
  )

# Шаг 13. Сохранение в SVG (в текущую папку)
svg_file <- "dostoevsky_sentiment.svg"
svglite(svg_file, width = 7, height = 5, bg = "#F2EFE9")
print(sentiment_plot)
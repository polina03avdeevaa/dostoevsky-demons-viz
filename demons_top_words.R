# Полный частотный анализ лемм романа «Бесы» с очисткой от стоп-слов

# Шаг 0. Загрузка библиотек
library(udpipe)
library(tidyverse)
library(ggplot2)
library(stopwords)   


# Шаг 1. Загрузка модели UDPipe
model_file <- "russian-syntagrus-ud-2.5-191206.udpipe"
if (!file.exists(model_file)) {
  udpipe_download_model(language = "russian", model_dir = ".")
}
model <- udpipe_load_model(model_file)


# Шаг 2. Чтение текста романа
text_full <- readLines("Demons.txt", 
                       encoding = "UTF-8") |> 
  paste(collapse = "\n")


# Шаг 3. Аннотирование 
anno_full <- udpipe_annotate(model, 
                             x = text_full, 
                             doc_id = "Besy")  |> 
  as.data.frame()


# Шаг 4. Определение знаменательных частей речи и стоп-лемм
content_pos <- "NOUN"
stop_words_base <- stopwords("ru")
extra_stop_lemmas <- c(
  # Глаголы-связки, модальные, вспомогательные
  "быть", "стать", "становиться", "мочь", "иметь", "сказать", "говорить",
  "знать", "думать", "понять", "оказаться", "казаться", "видеть", "слышать",
  # Частотные, но малосодержательные для идейного анализа
  "вдруг", "даже", "уж", "вот", "только", "ещё", "очень", "так", "потом",
  "теперь", "можно", "надо", "нужно", "идти", "стоять", "сидеть", "лежать",
  "хотеть", "стало", "было", "стал", "стала", "такой", "такова", "сам",
  "самый", "другой", "каждый", "любой", "некоторый", "многие", "один",
  "два", "три", "раз", "тогда", "там", "здесь", "туда", "сюда", "куда",
  "откуда", "зачем", "почему", "как", "также", "причём", "зато", "или",
  "либо", "будто", "словно", "точно", "едва", "лишь", "год", "день", "минута", "час", "место", 
  "комната", "город", "дверь", "франц", "петр", "ночь", "утро", "время", "рука", "лицо", 
  "пора", "день", "нога", "пётр", "право", "вечер", "глаз", "голова", "голос", "степан", 
  "капитан", "варвар", "вид", "угол", "рубль", "шаг", "мера", "конец", "сторона"
)

# Шаг 5. Объединение и удаление дубликатов
all_stop_lemmas <- unique(c(stop_words_base, extra_stop_lemmas))


# Шаг 6. Подсчёт частоты лемм с фильтрацией
keywords <- anno_full |> 
  filter(upos %in% content_pos) |>        
  mutate(lemma = tolower(lemma)) |>        
  filter(!lemma %in% all_stop_lemmas) |>    
  filter(nchar(lemma) >= 3) |>           
  count(lemma, sort = TRUE) |> 
  slice_head(n = 20)                        

print(keywords)

 
# Шаг 7. Визуализация
keywords_plot <- keywords |> 
  mutate(lemma = factor(lemma, 
                        levels = rev(lemma))) 
keywords_plot <- keywords_plot |> 
  mutate(color_fill = ifelse(row_number() <= 2, "#2B3B60", "#B59D81"))


p2_styled <- ggplot(keywords_plot, aes(y = lemma, 
                                       x = n, 
                                       fill = color_fill)) +
  geom_col(color = NA, 
           width = 0.7) +
  geom_text(aes(label = n, 
                x = n),
            hjust = -0.2, family = "Inter", size = 3.5, color = "#2C2C2C") +
  scale_fill_identity() +   
  scale_x_continuous(
    limits = c(0, max(keywords_plot$n) * 1.15),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "#2C2C2C", size = 11, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.background = element_rect(fill = "#F2EFE9", color = NA),
    panel.background = element_rect(fill = "#F2EFE9", color = NA)
  )

print(p2_styled)

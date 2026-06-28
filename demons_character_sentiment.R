# Анализ тональности персонажей романа «Бесы»

# Шаг 0. Загрузка пакетов
library(rulexicon)
library(data.table)
library(stringr)
library(ggplot2)
library(showtext)


# Шаг 1. Загрузка текста
text_raw <- readLines("Demons.txt", warn = FALSE, encoding = "UTF-8")
text_full <- paste(text_raw, collapse = "\n")


# Шаг 2. Определение персонажей и их вариантов написания
person_variants <- list(
  "Ставрогин" = c(
    "Ставрогин", "Ставрогина", "Ставрогину", "Ставрогиным", "Ставрогине",
    "Николай Всеволодович", "Николая Всеволодовича", "Николаю Всеволодовичу",
    "Коля", "Коли", "Коле"
  ),
  "Пётр Верховенский" = c(
    "Петр Верховенский", "Петра Верховенского", "Петру Верховенскому",
    "Верховенский", "Верховенского", "Верховенскому", "Верховенским",
    "Петр Степанович", "Петра Степановича", "Петру Степановичу",
    "Петруша", "Петруши"
  ),
  "Шатов" = c(
    "Шатов", "Шатова", "Шатову", "Шатовым", "Шатове",
    "Иван Павлович", "Ивана Павловича", "Ивану Павловичу"
  ),
  "Кириллов" = c(
    "Кириллов", "Кириллова", "Кириллову", "Кирилловым",
    "Алексей Нилыч", "Алексея Нилыча", "Алексею Нилычу"
  ),
  "Степан Верховенский" = c(
    "Степан Верховенский", "Степана Верховенского", "Степану Верховенскому",
    "Степан Трофимович", "Степана Трофимовича", "Степану Трофимовичу",
    "Степан", "Степана", "Степану"
  )
)


# Шаг 3. Создание таблицы соответствий (вариант -> каноническое имя)
name_map <- data.table()
for (canon in names(person_variants)) {
  for (variant in person_variants[[canon]]) {
    name_map <- rbind(name_map, data.table(raw = tolower(variant), canon = canon))
  }
}
name_map <- unique(name_map)


# Шаг 4. Разбивка на предложения
sentences <- unlist(strsplit(text_full, "(?<=[.!?])\\s+", perl = TRUE))


# Шаг 5. Функция расчёта тональности
get_sentiment_afinn <- function(text) {
  if (is.na(text) || text == "") return(0)
  
  words <- str_to_lower(text) |>
    str_replace_all("[^а-яё]", " ") |>
    str_split("\\s+") |>
    unlist()
  
  words <- words[words != ""]
  if (length(words) == 0) return(0)
  
  scores <- hash_sentiment_afinn_ru[token %in% words, score]
  if (length(scores) == 0) return(0)
  
  return(sum(scores))
}


# Шаг 6. Поиск упоминаний персонажей в предложениях
dt <- data.table(sentence = sentences)
dt[, sentence_lower := tolower(sentence)]

# Функция поиска всех вариантов имён в предложении
find_matches <- function(txt_low) {
  matched <- c()
  for (i in 1:nrow(name_map)) {
    if (grepl(name_map$raw[i], txt_low, fixed = TRUE)) {
      matched <- c(matched, name_map$raw[i])
    }
  }
  return(unique(matched))
}

dt[, mentioned := lapply(sentence_lower, find_matches)]
dt <- dt[sapply(mentioned, length) > 0]

# Проверка наличия данных
if (nrow(dt) == 0) {
  stop("Упоминаний не найдено. Проверьте варианты написания имён.")
}


# Шаг 7. Расчёт тональности предложений
dt[, sentiment := vapply(sentence, get_sentiment_afinn, numeric(1))]


# Шаг 8. Разворачивание на персонажей (одно предложение -> несколько строк)
dt_long <- dt[, .(raw_variant = unlist(mentioned),
                  sentiment = rep(sentiment, lengths(mentioned))),
              by = sentence]

dt_long <- merge(dt_long, name_map, by.x = "raw_variant", by.y = "raw", all.x = TRUE)


# Шаг 9. Агрегация результатов по персонажам
result <- dt_long[, .(
  total_sentiment = sum(sentiment),
  mean_sentiment = mean(sentiment),
  mentions = .N
), by = canon]

# Сортировка по убыванию общей тональности
setorder(result, 
         -total_sentiment)


# Шаг 10. Подготовка к визуализации
plot_data <- result[, .(canon = canon, total_sentiment = total_sentiment, mentions = mentions)]
plot_data[, color_fill := ifelse(total_sentiment > 0, "#2B3B60", "#B59D81")]
plot_data[, canon := reorder(canon, total_sentiment)]


# Шаг 11. Подключение шрифтов
tryCatch({
  font_add("Angst", regular = "Angst-Bold.otf")
  font_add_google("Inter", "Inter")
  showtext_auto()
}, error = function(e) {
  message("Не удалось загрузить шрифты: ", e$message)
})


# Шаг 12. Построение графика
character_sentiment_plot <- ggplot(plot_data, aes(x = canon, y = total_sentiment)) +
  geom_col(aes(fill = color_fill),
           color = NA, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.4) +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(-200, 250),
    breaks = seq(-200, 200, by = 50),
    expand = expansion(add = c(0.1, 0.1))
  ) +
  coord_flip() +
  labs(
    x = NULL, 
    y = "Суммарная тональность",
    title = "Тональность персонажей романа «Бесы»",
    subtitle = "По данным словаря AFINN"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#2C2C2C", size = 11),
    axis.text.y = element_text(color = "#2C2C2C", size = 11, hjust = 1),
    axis.title.x = element_text(color = "#2C2C2C", size = 12),
    plot.background = element_rect(fill = "#F2EFE9", color = NA),
    panel.background = element_rect(fill = "#F2EFE9", color = NA),
    plot.title = element_text(
      family = "Inter", size = 16, color = "#2C2C2C",
      hjust = 0.5, face = "bold", margin = margin(b = 5, t = 10)
    ),
    plot.subtitle = element_text(
      family = "Inter", size = 11, color = "#2C2C2C",
      hjust = 0.5, margin = margin(b = 15)
    )
  )

# Шаг 13. Вывод графика
print(character_sentiment_plot)

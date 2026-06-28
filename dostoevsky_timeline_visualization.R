# Визуализация хронологии публикации романов Достоевского

# Шаг 0. Загрузка пакетов
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(showtext)
library(svglite)


# Шаг 1. Подготовка данных о романах
romans_data <- data.frame(
  book = c(
    "«Униженные и оскорбленные»",
    "«Преступление и наказание»",
    "«Игрок»",
    "«Идиот»",
    "«Бесы»",
    "«Подросток»",
    "«Братья Карамазовы»"
  ),
  start_date = ymd(c(
    "1861-01-01", 
    "1866-01-01",
    "1866-05-01", 
    "1868-01-01", 
    "1870-01-01",
    "1875-01-01", 
    "1878-01-01"
  )),
  end_date = ymd(c(
    "1861-12-31", 
    "1866-12-31",
    "1866-10-29", 
    "1869-12-31", 
    "1872-12-31",
    "1875-12-31",
    "1880-12-31"
  )),
  is_great_pentateuch = c(
    FALSE,   # «Униженные и оскорбленные»
    TRUE,    # «Преступление и наказание»
    FALSE,   # «Игрок»
    TRUE,    # «Идиот»
    TRUE,    # «Бесы»
    TRUE,    # «Подросток»
    TRUE     # «Братья Карамазовы»
  )
) |> 
  mutate(
    start_year = year(start_date),
    end_year = year(end_date),
    start_decimal = decimal_date(start_date),
    end_decimal = decimal_date(end_date)
  ) |> 
  arrange(start_decimal) |> 
  mutate(
    book = factor(book, levels = rev(book)),
    y_position = as.numeric(book),
    pentateuch_group = ifelse(
      is_great_pentateuch, 
      "«Великое пятикнижие»", 
      "Остальные романы"
    )
  )


# Шаг 2. Подключение шрифтов
tryCatch({
  font_add_google("Inter", "Inter")
  showtext_auto()
}, error = function(e) {
  message("Не удалось загрузить шрифты: ", e$message)
})


# Шаг 3. Построение временной шкалы
timeline_plot <- ggplot(romans_data) +
  # Основные полосы романов
  geom_rect(
    aes(
      xmin = start_decimal, 
      xmax = end_decimal,
      ymin = y_position - 0.2, 
      ymax = y_position + 0.2,
      fill = pentateuch_group
    ),
    color = NA, 
    alpha = 0.85
  ) +
  # Настройка цветов
  scale_fill_manual(
    values = c(
      "«Великое пятикнижие»" = "#2B3B60", 
      "Остальные романы" = "#B59D81"
    )
  ) +
  # Оси
  scale_x_continuous(
    breaks = seq(1860, 1885, by = 5),
    limits = c(1860, 1885),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    breaks = romans_data$y_position,
    labels = romans_data$book,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Подписи
  labs(
    x = "Год публикации", 
    y = NULL, 
    fill = NULL
  ) +
  # Тема
  theme_minimal(base_family = "Inter") +
  theme(
    # Сетка
    panel.grid = element_blank(),
    # Оси
    axis.text.x = element_text(color = "#2C2C2C", size = 11),
    axis.text.y = element_text(color = "#2C2C2C", size = 11, hjust = 1),
    axis.title.x = element_text(color = "#2C2C2C", size = 12),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    # Фон
    plot.background = element_rect(fill = "#F2EFE9", color = NA),
    panel.background = element_rect(fill = "#F2EFE9", color = NA),
    # Легенда
    legend.position = "none"
  )


# Шаг 4. Сохранение в SVG (в текущую папку)
svg_file <- "dostoevsky_timeline.svg"
svglite(svg_file, width = 8, height = 5, bg = "#F2EFE9")
print(timeline_plot)
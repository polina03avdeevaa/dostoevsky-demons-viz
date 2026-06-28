# Визуализация сообществ персонажей романа «Бесы»

# Шаг 0. Загрузка пакетов
library(readr)
library(dplyr)
library(igraph)
library(svglite)


# Шаг 1. Функция для создания коротких меток (И. Фамилия)
make_label <- function(full_name) {
  # Защита от пустых значений
  if (is.null(full_name) || is.na(full_name) || trimws(full_name) == "") {
    return("?")
  }
  
  parts <- strsplit(trimws(full_name), " +")[[1]]
  
  # Если только одно слово (например, "Тихон")
  if (length(parts) == 1) {
    return(parts[1])
  }
  
  # Формат: "И. Фамилия"
  last_name <- parts[1]
  first_initial <- substr(parts[2], 1, 1)
  return(paste0(first_initial, ". ", last_name))
}


# Шаг 2. Загрузка данных о взаимодействиях
data_file <- "character_relationships_weighted.csv"
if (!file.exists(data_file)) {
  stop("Файл не найден: ", data_file)
}

edges_raw <- read_csv(data_file, 
                      col_types = cols(.default = col_character())) |>
  mutate(Weight = as.numeric(Weight)) |>
  filter(!is.na(Source), !is.na(Target), !is.na(Weight), Weight >= 17)


# Шаг 3. Создание графа
# Направленный граф
g_dir <- graph_from_data_frame(edges_raw, directed = TRUE)

# Неориентированный (для сообществ)
g_undir <- as.undirected(g_dir, 
                         mode = "collapse", 
                         edge.attr.comb = list(Weight = "sum"))


# Шаг 4. Создание коротких меток для вершин
V(g_undir)$label <- sapply(V(g_undir)$name, make_label)

# Специальное исправление для Тихона
if (any(grepl("Тихон", V(g_undir)$name, fixed = TRUE))) {
  idx <- which(grepl("Тихон", V(g_undir)$name, fixed = TRUE))
  V(g_undir)$label[idx] <- "Тихон"
}


# Шаг 5. Обнаружение сообществ (кластеризация)
set.seed(9876)
communities <- cluster_walktrap(g_undir, 
                                weights = E(g_undir)$Weight, 
                                steps = 3)

membership <- membership(communities)
num_communities <- length(unique(membership))


# Шаг 6. Настройка цветов для сообществ
community_palette <- c("#8E9DAE", "#2B3B60", "#B59D81", "#8B7355", 
                       "#7E8B68", "#5B6E4A", "#C28B3E")

if (num_communities > length(community_palette)) {
  community_palette <- rep(community_palette, length.out = num_communities)
}

vertex_colors <- community_palette[membership]
groups <- split(V(g_undir)$name, membership)


# Шаг 7. Компоновка графа (layout)
set.seed(9876)
layout_pos <- layout_with_fr(g_undir, 
                             weights = E(g_undir)$Weight, 
                             niter = 1000)

# Масштабирование и центрирование
scale_factor <- 2.5
layout_pos <- layout_pos * scale_factor
layout_pos[,1] <- layout_pos[,1] - mean(layout_pos[,1])
layout_pos[,2] <- layout_pos[,2] - mean(layout_pos[,2])


# Настройка полей
par(mar = c(1, 1, 2, 1))


# Шаг 8. Построение графа
plot(g_undir,
     layout = layout_pos,
     # Вершины
     vertex.color = vertex_colors,
     vertex.size = 9,
     vertex.frame.color = "#2C2C2C",
     vertex.frame.width = 0.5,
     # Метки
     vertex.label = V(g_undir)$label,
     vertex.label.cex = 0.65,
     vertex.label.color = "#2C2C2C",
     vertex.label.family = "Inter",
     vertex.label.dist = 1,
     vertex.label.degree = -pi/2,
     # Рёбра
     edge.width = E(g_undir)$Weight / max(E(g_undir)$Weight) * 1.8,
     edge.color = "#B59D81",
     edge.arrow.size = 0,
     # Группы (сообщества)
     mark.groups = groups,
     mark.col = adjustcolor(community_palette[1:num_communities], 
                            alpha.f = 0.15),
     mark.border = NA
)

# Добавление заголовка
title("Сообщества персонажей романа «Бесы»", 
      col.main = "#2C2C2C", 
      family = "Inter")

# Шаг 9. Сохранение в SVG (в текущую папку)
svg_file <- "demons_communities.svg"
svglite(svg_file, width = 6, height = 4, bg = "#EFEBE4")

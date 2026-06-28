# Визуализация графа взаимодействий персонажей романа «Бесы»

# Шаг 0. Загрузка пакетов
library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggrepel)
library(svglite)


# Шаг 1. Чтение и предобработка данных 
edges_raw <- read_csv("character_relationships_weighted.csv", 
                      col_types = cols(.default = col_character()))  |> 
  mutate(Weight = as.numeric(Weight))  |> 
  filter(!is.na(Source), !is.na(Target), !is.na(Weight), Weight >= 17)


# Шаг 2. Сокращение имён (только фамилия)
shorten_name <- function(full_name) {
  parts <- strsplit(full_name, " ")[[1]]
  return(parts[1])
}

edges_raw <- edges_raw  |> 
  mutate(Source_short = sapply(Source, shorten_name),
         Target_short = sapply(Target, shorten_name))


# Шаг 3. Построение графа 
nodes <- unique(c(edges_raw$Source_short, edges_raw$Target_short))
g <- graph_from_data_frame(edges_raw[, c("Source_short", 
                                         "Target_short",
                                         "Weight")], 
                           directed = TRUE, vertices = nodes)

E(g)$weight <- edges_raw$Weight
V(g)$total_weight <- strength(g, weights = E(g)$weight, mode = "all")
V(g)$log_total <- log1p(V(g)$total_weight)


# Шаг 4. Группировка и цвета 
main_chars_short <- c("Ставрогин", "Верховенский", "Шатов", "Кириллов", "Ставрогина")
V(g)$group <- ifelse(V(g)$name %in% main_chars_short, V(g)$name, "Другие")

group_colors <- c(
  "Ставрогин"      = "#2B3B60",
  "Верховенский"   = "#2B3B60",
  "Шатов"          = "#2B3B60",
  "Кириллов"       = "#2B3B60",
  "Ставрогина"     = "#2B3B60",
  "Другие"         = "#B59D81"
)


# Шаг 5. Компоновка с сильным разведением 
set.seed(555)
layout_coords <- layout_with_kk(g, weights = E(g)$weight, maxiter = 5000)
scale_factor <- 7.0
layout_coords <- layout_coords * scale_factor
layout_coords[,1] <- layout_coords[,1] - mean(layout_coords[,1])
layout_coords[,2] <- layout_coords[,2] - mean(layout_coords[,2])

node_pos <- data.frame(name = V(g)$name, 
                       x = layout_coords[,1], 
                       y = layout_coords[,2],
                       log_total = V(g)$log_total,
                       group = V(g)$group)


# Шаг 6. Сохранение в SVG (в текущую папку)
svg_file <- "demons_network.svg"
svglite(svg_file, width = 8, height = 6, bg = "#F2EFE9")


# Шаг 7. График 
graph_characters <- ggraph(g, 
                           layout = "manual", 
                           x = node_pos$x, 
                           y = node_pos$y) + 
  geom_edge_fan(aes(width = weight, alpha = weight),
                arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                end_cap = circle(5, "mm"),
                start_cap = circle(4, "mm"),
                color = "#B59D81",       
                lineend = "round") +
  geom_node_point(aes(size = log_total, 
                      fill = group), 
                  shape = 21, 
                  stroke = 0.4, 
                  color = "#2C2C2C") +
  geom_text(data = node_pos,
            aes(x = x, 
                y = y + 0.8, 
                label = name),   
            family = "sans", size = 3.5, color = "#2C2C2C",
            hjust = 0.5,      
            vjust = 0) +    
  scale_size_continuous(range = c(3.5, 7),
                        breaks = round(seq(min(node_pos$log_total), 
                                           max(node_pos$log_total), 
                                           length.out = 4), 1),
                        labels = function(x) round(expm1(x), 0)) +
  scale_edge_width_continuous(range = c(0.2, 0.8), 
                              guide = "none") +
  scale_edge_alpha_continuous(range = c(0.4, 0.7),   
                              guide = "none") +
  scale_fill_manual(values = group_colors, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.3)) +
  scale_y_continuous(expand = expansion(mult = 0.3)) +
  theme_graph(base_family = "sans") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F2EFE9", color = NA),
    panel.background = element_rect(fill = "#F2EFE9", color = NA),
    plot.margin = margin(15, 35, 15, 15)
  )

print(graph_characters)

# Шаг 8. Закрытие устройства (файл сохраняется)
dev.off()
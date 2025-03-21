library(hexSticker)
library(ggplot2)
library(grid)
library(ggforce)
library(ggtext)

inside_color <- "#7fb3d5"
margin_color <- "black"

# Create database symbol
db_symbol <- ggplot() +
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 0.0, ymax = 1.0), fill = inside_color, color = margin_color)+
  geom_ellipse(aes(x0 = 1.5, y0 = 0.0, a = 1.5, b = 0.1, angle = 0), fill = inside_color) +
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 0.0, ymax = 1.0), fill = inside_color) +
  geom_ellipse(aes(x0 = 1.5, y0 = 1.0, a = 1.5, b = 0.1, angle = 0), fill = inside_color, color = margin_color) +
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 1.1, ymax = 2.1), fill = inside_color, color = margin_color)+
  geom_ellipse(aes(x0 = 1.5, y0 = 1.1, a = 1.5, b = 0.1, angle = 0), fill = inside_color) +
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 1.1, ymax = 2.1), fill = inside_color) +
  geom_ellipse(aes(x0 = 1.5, y0 = 2.1, a = 1.5, b = 0.1, angle = 0), fill = inside_color, color = margin_color)+
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 2.2, ymax = 3.2), fill = inside_color, color = margin_color)+
  geom_ellipse(aes(x0 = 1.5, y0 = 2.2, a = 1.5, b = 0.1, angle = 0), fill = inside_color) +
  geom_rect(aes(xmin = 0.0, xmax = 3.0, ymin = 2.2, ymax = 3.2), fill = inside_color) +
  geom_ellipse(aes(x0 = 1.5, y0 = 3.2, a = 1.5, b = 0.1, angle = 0), fill = inside_color, color = margin_color) +
  geom_richtext(
    aes(x = -1.6, y = 2.5, label = "<span style='font-family:fa-solid; color:#ecf0f1;'>&#xf0ad;</span>"),
    fill = NA, label.color = NA, size = 12, hjust = 0
  ) +
  geom_richtext(
    aes(x = 3.5, y = 2.5, label = "<span style='font-family:fa-solid; color:#ecf0f1;'>&#xf013;</span>"),
    fill = NA, label.color = NA, size = 12, hjust = 0
  ) +
  geom_richtext(
    aes(x = 1, y = 3.8, label = "<span style='font-family:fa-solid; color:#ecf0f1;'>&#xf552;</span>"),
    fill = NA, label.color = NA, size = 12, hjust = 0
  ) +
  geom_richtext(
    aes(x = -1.6, y = 0.9, label = "<span style='font-family:fa-solid; color:#ecf0f1;'>&#xf6e3;</span>"),
    fill = NA, label.color = NA, size = 12, hjust = 0
  ) +
  geom_richtext(
    aes(x = 3.5, y = 0.8, label = "<span style='font-family:fa-solid; color:#ecf0f1;'>&#xf54a;</span>"),
    fill = NA, label.color = NA, size = 12, hjust = 0
  )+
  theme_void() +
  coord_fixed() +
  expand_limits(x = 4.2, y = 4)

print(db_symbol)
ggsave("db_symbol.png", plot = db_symbol)

# Create package logo
s <- sticker(
  subplot = db_symbol,
  package = "dbHelpeR",
  p_size = 18,
  p_y = 1.45,
  h_fill = "#7dcea0",
  h_color = "#2980b9",
  h_size = 2,
  s_x = 0.975,
  s_y = .8,
  s_width = 1.4,
  s_height = 1.4,
  url = "https://github.com/TobiasKellner/dbHelpeR",
  u_size = 3.2,
  u_color = "#ffffff",
  filename="inst/figures/logo.png",
  asp = 2
  )

print(s)


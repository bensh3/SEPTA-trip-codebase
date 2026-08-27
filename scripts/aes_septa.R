library(scico)
library(scales)

# SEPTA Planning/Metro palette specification ------------------------------

pal_planning <- c(
  navy = "#222845",
  teal = "#0F9CD8",
  lime = "#ACD154",
  turquoise = "#009BA7",
  orange = "#DF8025",
  skyblue = "#B7DBF4",
  green = "#2BB673",
  yellow = "#FFD24F",
  red = "#CA3E27",
  blue = "#4B67B0"
)

pal_metro <- c(
  septa_blue = "#001391",
  septa_red = "#e40032",
  m_gray = "#333333",
  m_white = "#FFFFFF",
  reg_red = "#D82000",
  reg_gray = "#CCCCCC",
  reg_yellow = "#F7ED00",
  d_pink = "#E64270",
  b_orange = "#F26100",
  g_yellow = "#FFD700",
  l_blue = "#0097D6",
  t_green = "#5A960A",
  m_purple = "#5F249F",
  f_green = "#1b6814",
  f_blue = "#1752ff",
  f_purple = "#dec2ff",
  f_brown = "#672f00",
  rrd_rail = "#45647B",
  freq_red = "#EF3340"
)

get_colors <- function(..., palette) {
  colors <- c(...)

  if (is.null(colors)) {
    return(palette)
  }
  palette[colors]
}

pal_septa_list <- list(
  plan = get_colors(
    "red",
    "blue",
    "green",
    "orange",
    "skyblue",
    "yellow",
    "turquoise",
    "lime",
    "teal",
    palette = pal_planning
  ),
  metro = get_colors(
    "septa_blue",
    "septa_red",
    "l_blue",
    "b_orange",
    "t_green",
    "g_yellow",
    "d_pink",
    "m_purple",
    "reg_red",
    "f_brown",
    "rrd_rail",
    "freq_red",
    palette = pal_metro
  ),
  mdark = get_colors("m_gray", "rrd_rail", "f_blue", "f_green", "f_brown", "m_purple", palette = pal_metro)
)


# scale_*_septa aesthetics ------------------------------------------------

scale_color_septa <- function(..., pal_list = pal_septa_list, palette = "plan", discrete = T, reverse = F, named = F) {
  pal <- pal_list[[palette]]

  if (!named) {
    names(pal) <- NULL
  }

  if (reverse) {
    pal <- rev(pal)
  }
  if (discrete) {
    scale_color_manual(..., values = pal)
  } else {
    scale_color_gradientn(..., colors = pal)
  }
}

scale_fill_septa <- function(..., pal_list = pal_septa_list, palette = "plan", discrete = T, reverse = F, named = F) {
  pal <- pal_list[[palette]]

  if (!named) {
    names(pal) <- NULL
  }

  if (reverse) {
    pal <- rev(pal)
  }
  if (discrete) {
    scale_fill_manual(..., values = pal)
  } else {
    scale_fill_gradientn(..., colors = pal)
  }
}


# Custom SEPTA ggplot theme based on minimal ------------------------------

theme_septa <- function(
  ...,
  font = c("Roboto", "Roboto"),
  base_size = 12,
  title_align = "panel",
  legend_pos = "right"
) {
  theme_minimal() %+replace%
    theme(
      axis.ticks = element_line(color = "grey92"),
      legend.position = legend_pos,
      title = element_text(size = rel(1.25), face = 'bold', family = font[1], ),
      text = element_text(size = base_size, family = font[2], color = "#444444"),
      strip.text = element_text(size = base_size, family = font[2], hjust = 0),
      panel.grid.minor.y = element_blank(),
      plot.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
      plot.title.position = title_align,
      plot.caption.position = "plot",
      plot.margin = margin(12, 24, 12, 24),
      plot.title = element_text(hjust = 1, margin = margin(0, 0, 6, 0)),
      plot.subtitle = element_text(size = base_size, family = font[2], hjust = 1, margin = margin(0, 0, 12, 0)),
      plot.caption = element_text(size = rel(0.5), family = font[2], hjust = 1),
      legend.title = element_text(size = base_size),
      axis.title = element_text(size = rel(0.75), vjust = -1),
      axis.text = element_text(size = rel(0.75), margin = margin(0, 0, 3, 0))
    )
}


# Regional Rail line codes ------------------------------------------------

codes <- c(
  "AIR" = "Airport",
  "CHE" = "Chestnut Hill East",
  "CHW" = "Chestnut Hill West",
  "CYN" = "Cynwyd",
  "FOX" = "Fox Chase",
  "LAN" = "Lansdale/Doylestown",
  "MED" = "Media/Wawa",
  "NOR" = "Manayunk/Norristown",
  "PAO" = "Paoli/Thorndale",
  "TRE" = "Trenton",
  "WAR" = "Warminster",
  "WIL" = "Wilmington/Newark",
  "WTR" = "West Trenton"
)

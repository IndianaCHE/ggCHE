tfcs_colors <- list(
  nine = rgb(254, 184, 34, maxColorValue = 255),
  ten = rgb(255, 224, 83, maxColorValue = 255),
  eleven = rgb(179, 246, 139, maxColorValue = 255),
  twelve = rgb(114, 206, 231, maxColorValue = 255),
  neutral = rgb(232, 226, 214, maxColorValue = 255),
  neutral2 = rgb(113, 113, 113, maxColorValue = 255),
  plan = rgb(179, 8, 56, maxColorValue = 255),
  prepare = rgb(247, 142, 30, maxColorValue = 255),
  pay = rgb(115, 193, 103, maxColorValue = 255),
  primary = rgb(81, 97, 172, maxColorValue = 255),
  primary2 = rgb(35, 62, 153, maxColorValue = 255)
  )

tfcs_color_values <- c(
  nine = tfcs_colors$nine,
  `9` = tfcs_colors$nine,
  `09` = tfcs_colors$nine,
  ssp_01 = tfcs_colors$nine,
  "Create a Graduation Plan" = tfcs_colors$nine,
  "Grad Plan" = tfcs_colors$nine,
  ssp_02 = tfcs_colors$nine,
  "Participate in an Extracurricular or Service Activity" = tfcs_colors$nine,
  "Extracurricular" = tfcs_colors$nine,
  ssp_03 = tfcs_colors$nine,
  "Watch \"Paying for College 101\"" = tfcs_colors$nine,
  "Paying 101" = tfcs_colors$nine,

  ten = tfcs_colors$ten,
  `10` = tfcs_colors$ten,
  ssp_04 = tfcs_colors$ten,
  "Take a Career Interests Assessment" = tfcs_colors$ten,
  "Career Interests" = tfcs_colors$ten,
  ssp_05 = tfcs_colors$ten,
  "Get Workplace Experience" = tfcs_colors$ten,
  "Workplace" = tfcs_colors$ten,
  ssp_06 = tfcs_colors$ten,
  "Estimate the Costs of College" = tfcs_colors$ten,
  "Estimate Costs" = tfcs_colors$ten,

  eleven = tfcs_colors$eleven,
  `11` = tfcs_colors$eleven,
  ssp_07 = tfcs_colors$eleven,
  "Visit a College Campus" = tfcs_colors$eleven,
  "Campus Visit" = tfcs_colors$eleven,
  ssp_08 = tfcs_colors$eleven,
  "Take a College Entrance Exam (ACT/SAT)" = tfcs_colors$eleven,
  "Entrance Exam" = tfcs_colors$eleven,
  ssp_09 = tfcs_colors$eleven,
  "Search for Scholarships" = tfcs_colors$eleven,
  "Scholarships" = tfcs_colors$eleven,

  twelve = tfcs_colors$twelve,
  `12` = tfcs_colors$twelve,
  ssp_10 = tfcs_colors$twelve,
  "Submit Your College Application" = tfcs_colors$twelve,
  "Application" = tfcs_colors$twelve,
  ssp_11 = tfcs_colors$twelve,
  "Watch \"College Success 101\"" = tfcs_colors$twelve,
  "Success 101" = tfcs_colors$twelve,
  ssp_12 = tfcs_colors$twelve,
  "File Your FAFSA" = tfcs_colors$twelve,
  "FAFSA" = tfcs_colors$twelve,

  Neutral = tfcs_colors$Neutral,
  Neutral2 = tfcs_colors$Neutral2,
  Plan = tfcs_colors$Plan,
  Prepare = tfcs_colors$Prepare,
  Pay = tfcs_colors$Pay,
  Primary = tfcs_colors$Primary,
  Primary2 = tfcs_colors$Primary2,

  # Unnamed for matching:
  tfcs_colors$Primary,
  # tfcs_colors$Primary2,
  tfcs_colors$nine,
  tfcs_colors$ten,
  tfcs_colors$eleven,
  tfcs_colors$twelve,
  tfcs_colors$Plan,
  tfcs_colors$Prepare,
  tfcs_colors$Pay
  )

scale_color_tfcs <- function(...){
  scale_color_manual(
    values = tfcs_color_values,
    na.value = tfcs_colors$primary,
    ...
    )
}

scale_fill_tfcs <- function(...){
  scale_fill_manual(
    values = tfcs_color_values,
    na.value = tfcs_colors$primary,
    ...
    )
}

theme_tfcs <- function(base_size = 12, base_family = "Gotham HTF"){
  half_line <- base_size / 2
  qtr_line <- base_size / 4
  preferred_rel <- 0.75
  line_size = 0.5
  theme(
    line = element_line(colour = "#717173",
      size = line_size,
      linetype = 1,
      lineend = "butt"
      ),
    rect = element_rect(
      fill = "#E8E2D6",
      colour = "transparent",
      size = 0,
      linetype = 0
      ),
    text = element_text(family = base_family,
      face = "plain",
      colour = "#717173",
      size = base_size,
      lineheight = 1,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
      ),
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(preferred_rel)),
    axis.text.x = NULL,
    axis.text.x.top = NULL,
    axis.text.y = NULL,
    axis.text.y.right = NULL,
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = half_line), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0),
    axis.title.y = element_text(
      angle = 90, margin = margin(r = half_line), vjust = 1
      ),
    axis.title.y.right = element_text(
      angle = -90, margin = margin(l = half_line), vjust = 0
      ),
    legend.background = element_blank(),
    legend.spacing = unit(0, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0, 0, qtr_line, qtr_line, "pt"),
    legend.key = element_blank(),
    legend.key.size = unit(base_size, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(preferred_rel)),
    legend.text.align = 1,
    legend.title = NULL,
    legend.title.align = NULL,
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "right",
    legend.box = "vertical",
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.box.background = element_blank(),
    legend.box.just = 1,
    legend.box.spacing = unit(0.0, "pt"),
    panel.background = NULL,
    panel.border = element_blank(),
    panel.grid.major = NULL,
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_line(size = line_size / 2),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "#717173"),
    strip.text = element_text(
      color = "#E8E2D6",
      size = rel(preferred_rel),
      margin = margin(qtr_line, qtr_line, qtr_line, qtr_line, "pt")
      ),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(half_line, "pt"),
    strip.switch.pad.wrap = unit(half_line, "pt"),
    plot.background = element_blank(),
    plot.title = element_text(size = rel(1/preferred_rel),
      hjust = 0, vjust = 1,
      margin = margin(b = half_line)
      ),
    plot.subtitle = element_text(margin = margin(b = qtr_line)),
    plot.caption = NULL,
    plot.margin = margin(qtr_line, qtr_line, qtr_line, qtr_line),
    complete = TRUE,
    validate = TRUE
    )
}

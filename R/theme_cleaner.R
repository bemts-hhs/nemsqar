#' Theme Cleaner
#'
#' A customizable ggplot2 theme function for creating polished and minimalistic plots. The `theme_cleaner` function offers flexible options to control various plot elements, including font styles, sizes, colors, alignment, and the visibility of axes and grid lines.
#'
#' @param base_size Numeric. Default font size for plot elements.
#' @param base_family Character. Font family used for text in the plot.
#' @param base_color Character. Hex color code for primary plot elements.
#' @param base_color_title Character. Hex color code for plot title and legend title.
#' @param title_text_size Numeric. Font size for plot title text.
#' @param subtitle_text_size Numeric. Font size for plot subtitle text.
#' @param caption_color Character. Hex color code for plot caption text.
#' @param legend_position Character. Legend position on the plot (e.g., "top", "bottom", "left", or "right").
#' @param vjust_title Numeric. Vertical justification of the plot title.
#' @param vjust_subtitle Numeric. Vertical justification of the plot subtitle.
#' @param hjust_title Numeric. Horizontal justification of the plot title.
#' @param hjust_subtitle Numeric. Horizontal justification of the plot subtitle.
#' @param axis_lines Logical. If TRUE, axis lines are drawn in `base_color`; otherwise, they are hidden.
#' @param facets Logical. If TRUE, formatting for facet plots is applied.
#' @param facet_text_size Numeric. If facets = TRUE, size formatting for strip.text is applied.
#' @param draw_panel_border Logical. If TRUE, a border is drawn around panels in facet plots.
#' @param ... Additional arguments passed to `ggplot2::theme` for further customization.
#'
#' @return A `ggplot2` theme object.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
theme_cleaner <-
  function(base_size = 12,
           base_family = "Work Sans",
           base_color = "#70C8B8",
           base_color_title = "#03617A",
           title_text_size = ceiling(base_size * 1.1),
           subtitle_text_size = ceiling(base_size * 1.05),
           caption_color = "#19405B",
           legend_position = "top",
           vjust_title = 0,
           vjust_subtitle = 0,
           hjust_title = 0,
           hjust_subtitle = 0,
           axis_lines = FALSE,
           facets = FALSE,
           facet_text_size = base_size,
           draw_panel_border = FALSE,
           ...) {
    if (!facets) {
      theme(
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        complete = TRUE,
        text = element_text(
          family = base_family,
          face = "bold",
          color = base_color
        ),
        # Adjust font color
        
        axis.text = element_text(color = base_color, size = base_size),
        axis.title = element_text(color = base_color_title, size = base_size),
        legend.title = element_text(color = base_color_title, size = base_size),
        legend.text = element_text(color = base_color, size = base_size),
        plot.title = element_text(
          color = base_color_title,
          size = title_text_size,
          face = "bold",
          hjust = hjust_title,
          vjust = vjust_title
        ),
        plot.subtitle = element_text(
          color = base_color,
          size = subtitle_text_size,
          hjust = hjust_subtitle,
          vjust = vjust_subtitle
        ),
        plot.caption = element_text(
          size = base_size,
          color = caption_color,
          hjust = 0
        ),
        legend.position = legend_position,
        legend.justification = "center",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = if_else(
          axis_lines == TRUE, base_color, "transparent"
        )),
        ...
      )
    } else if (facets) {
      theme(
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          color = if_else(draw_panel_border == F, "transparent", base_color),
          fill = NA
        ),
        plot.background = element_blank(),
        complete = TRUE,
        text = element_text(
          family = base_family,
          face = "bold",
          color = base_color
        ),
        # Adjust font color
        
        axis.text = element_text(color = base_color, size = base_size),
        axis.title = element_text(color = base_color_title, size = base_size),
        legend.title = element_text(color = base_color_title, size = base_size),
        legend.text = element_text(color = base_color, size = base_size),
        plot.title = element_text(
          color = base_color_title,
          size = title_text_size,
          face = "bold",
          hjust = hjust_title,
          vjust = vjust_title
        ),
        plot.subtitle = element_text(
          color = base_color,
          size = subtitle_text_size,
          hjust = hjust_subtitle,
          vjust = vjust_subtitle
        ),
        plot.caption = element_text(
          size = base_size,
          color = caption_color,
          hjust = 0
        ),
        legend.position = legend_position,
        legend.justification = "center",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = if_else(
          axis_lines == TRUE, base_color, "transparent"
        )),
        strip.background = element_rect(fill = "#2A6357", color = "#B9E1DA"),
        strip.text = element_text(color = "ghostwhite", size = facet_text_size),
        ...
      )
      
    }
  }


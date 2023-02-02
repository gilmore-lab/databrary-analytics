# Graphic theme elements
ln_size <- 3
base_size <- 14
color_orange <- "#ec7751"
color_teal <- "#4CAE99"
color_purple <-"#AB00FF"

databrary_theme <- 
  ggplot2::theme_classic(base_size = base_size) +
  ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
  ggplot2::theme(legend.position="none", 
                 axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = "black")) +
  ggplot2::theme(axis.line = ggplot2::element_blank()) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
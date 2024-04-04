# Aesthetics and sh*t

#websafe_bluepal <- c("#ccccff", "#6666ff", "#000099")
#bluepal3        <- c("#006ad1", "#1e90ff", "#6bb6ff")
#websafe_pinkpal <- c("#660066" ,"#ff66ff", "#ffc4ff")
#websafe_greenpal <- c("#003333", "#336633", "#66cc99")

#websafe_greenpal2 <- c("#00cc00", "#006600", "#66cc99")

#websafe_redpal <- c("#ffb6a9", "#ff6347", "#e42300")


#depth_palette = c("#000099", "#0d0887" )




theme_bjc <- function (base_size = 11, base_family = "Arial") {
  theme_minimal() %+replace% 
    theme(
      plot.background = element_rect(fill = "white", colour = "transparent"),
      legend.title = element_text(color = "black", size = 10),
      legend.text = element_text(color = "black", size = 9),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9)
    )
}


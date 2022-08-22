library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggrepel)


make_PA_graph <- function(tbl, ID="No ID given") {
  graph <- ggplot(data=tbl$mean_pm2.5, aes(x=with_tz(datetime, tzone = "Europe/London"), y=average_pm2.5)) +
    geom_line(color="#327da8", size=1.5) +
    geom_hline(yintercept = 25, color="#b04633", linetype="dashed", size=1) +
    xlab("Date and time") +
    ylab("PM2.5") +
    ylim(0, NA) +
    ggtitle(ID) +
    theme_minimal() +
    theme(text=element_text(size=18, family="Calibri Light"))
  return(graph)
}

generate_viz <- function(datum) {
  if (datum < 10) {
    min_x <- 10
  } else {
    min_x <- datum
  }
  datum <- data.frame(x=c(datum), min_x=c(min_x))
  min_width <- max(c(datum$x +20, 100))
  plt <- ggplot(datum, aes()) +
    xlim(0, min_width) +
    ylim(0,110) +
    # coord_cartesian(clip = "off") +
    geom_rect(xmin=0, ymin=0, xmax=12.0, ymax=100, fill="green4") +
    geom_rect(xmin=12.0, ymin=0, xmax=35.4, ymax=100, fill="yellow2") +
    geom_rect(xmin=35.4, ymin=0, xmax=55.4, ymax=100, fill="orange2") +
    geom_rect(xmin=55.4, ymin=0, xmax=150.4, ymax=100, fill="red") +
    geom_rect(xmin=150.4, ymin=0, xmax=250.4, ymax=100, fill="darkorchid4") +
    geom_rect(xmin=250.4, ymin=0, xmax=500, ymax=100, fill="brown") +
    geom_label_repel(x=6,y=5, label="Good", family="Calibri", lineheight=1, direction="y", size=4) +
    geom_label_repel(x=((35.4-12)/2)+12,y=5, label="Moderate", family="Calibri", lineheight=1, direction="y", size=4) +
    geom_label_repel(x=((55.4-35.4)/2)+35.4,y=5, label="Unhealthy for\nsensitive groups", family="Calibri", lineheight=1, direction="y", size=4) +
    geom_label_repel(x=((150.4-55.4)/2)+55.4,y=5, label="Unhealthy", family="Calibri", lineheight=1, direction="y", size=4) +
    geom_label_repel(x=((250.4-150.4)/2)+150.4,y=5, label="Very unhealthy", family="Calibri", lineheight=1, direction="y", size=4, xlim=c(150,250)) +
    geom_label_repel(x=((500-250.4)/2)+250.4,y=5, label="Hazardous", family="Calibri", lineheight=1, direction="y", size=4, xlim=c(250,Inf)) +
    # geom_rect(xmin=datum$x - 2, xmax=datum$x + 2, ymin=35, ymax=40, fill="white", color="grey22") +
    # geom_rect(xmin=datum$x - 20, xmax=datum$x + 30, ymin=40, ymax=70, color="grey22", fill="white") +
    # geom_tile(aes(x=min_x, y=40, height=15, width=1), fill="white", color="black") +
    geom_polygon(data=tibble(x=c(datum$min_x, datum$min_x-5, datum$min_x+5), y=c(30,50,50)) , mapping=aes(x,y), fill="white", color="black") +
    geom_label(aes(x=min_x, y=50, label=paste("Your home:\n", x, "\u00B5g/m\u00B3")),  family="Calibri", size=10, lineheight=1, label.padding=unit(0.5, "lines")) +
    theme_minimal() +
    theme(axis.title=element_blank(), panel.grid = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size=10))
  plt
  return(plt)
}

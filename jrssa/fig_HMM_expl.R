## 

library(dplyr)
library(stringr)
library(magick); par(mar = rep(0, 4))
setwd("C:/Users/michels/sciebo/Konferenzen/NESSIS 2025/Talk")
tracking_kc_az = read.csv("tracking_kc.csv")
defender_data = read.csv("kc_defender_data.csv")
players = read.csv("players.csv")

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

pitch_colors = c("#a4e86f", "#7eb356")
team_colors = c("#97233F", "white")#"blue4")
los = 19

clubs = levels(as.factor(tracking_kc_az$club))[levels(as.factor(tracking_kc_az$club)) != "football"]

xlim = round(c(min(tracking_kc_az$x) - 8, 
               max(tracking_kc_az$x)+5), - 1)
xlim[2] = 30
# ylim = round(c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1), -1)
ylim = c(0, field_width)
asp_ratio =  diff(xlim) / diff(ylim)

xlabs = c("10", "20", "30", "40", "50", "40", "30", "20", "10")

imInd = 1


t = unique(tracking_kc_az$time)[10] 
# Daten für den aktuellen Zeitpunkt filtern
current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
current_data = current_data |> 
  left_join(players %>% select(nflId, displayName, position), by = "nflId")

current_data = current_data |> filter(position != "QB", position != "C", 
                                      position != "G", position != "T",
                                      position != "DT", position != "DE",
                                      position != "NT", position != "OLB",
                                      position != "SS") 

current_data = current_data |> filter(displayName.x == "Marco Wilson" | club == "KC")

# Vertauschte Limits (Spielfeld wird gedreht)
ylim <- c(0, 30)  # vorher xlim
xlim <- c(0, 53.3) # vorher ylim (field_width Bereich)

# Spielfeld zeichnen (gedreht)
plot(
  NA,
  xlim = xlim,
  ylim = c(ylim[1]-2, ylim[2]+10),
  xlab = "", ylab = "",
  xaxt = "n", yaxt = "n",
  bty = "n",
  xpd = TRUE
)

# draw the endzones if necessary
if(xlim[1] < 10){
  rect(0, ylim[1], field_width, 10, 
       col = colorspace::lighten(team_colors[1], 0.3),
       border = NA)
  text(field_width/2, 5, "ARIZONA", #srt = 90, 
       col = team_colors[1], cex = 2.25)
}

# Spielfeld Hintergrund zeichnen (gekippt)
i <- 10
k <- 1
while(i < ylim[2]){
  if(i < ylim[2]){
    top <- i + 5
  } else{
    top <- ylim[2]
  }
  
  rect(xlim[1], i, xlim[2], top,
       col = colorspace::lighten(pitch_colors[i %% 2 + 1], 0.6),
       border = NA)
  
  if(i >= 20 & i %% 10 == 0 & i < 100){
    text(xlim[2] - 2, i+1, xlabs[k], col = "#00000080", lwd = 2, cex = 0.9)
    k <- k + 1
  }
  
  i <- i + 5
}


text(xlim[2] - 2, ylim[2]+3, "y", col = "#00000080", lwd = 2, cex = 0.9)

# Spielfeld-Border
segments(0, ylim[1], 0, ylim[2]+10, lwd = 2)
segments(field_width, ylim[1], field_width, ylim[2]+10, lwd = 2)
segments(0, ylim[1], field_width, ylim[1], lwd = 2)
segments(0, ylim[2]+5, field_width, ylim[2]+5, lwd = 2)

# Line of Scrimmage (LOS) - gedreht
segments(ylim[1], los, field_width, los, col = "#00000080", lwd = 2)

players_kc <- current_data[which(current_data$club == "KC"),]
players_nkc <- current_data[which(current_data$club != "KC"),]

dichte = abs(players_kc$y - players_nkc$y[1])
dichte = exp(-0.4*dichte)+0.08
dichte = dichte/max(dichte)

color12 <- c("#FF7F00", "#377EB8", "#4DAF4A", "#984EA3", "#CA2430")
# Normalverteilungen entlang der X-Achse zeichnen (weil gedreht)
for(i in 1:nrow(players_kc)){
  # X-Werte für Normalverteilung, zentriert auf Spielerposition
  x_vals <- seq(field_width - players_kc$y[i] - 5, field_width - players_kc$y[i] + 5,
                length.out = 10000)
  
  # Dichtewerte berechnen
  y_offset <- dnorm(x_vals, mean = field_width - players_kc$y[i], sd = 2)
  
  # Skalieren für Sichtbarkeit
  y_offset <- y_offset * 10
  
  # Normalverteilung "gekippt": y + Offset, x = x_vals
  lines(x_vals, ylim[2] + 5 + y_offset, 
        col = scales::alpha("black", dichte[i]),
        lwd = 2)
  
  segments(field_width - current_data$y[which(current_data$club == "KC")][i],
           current_data$x[which(current_data$club == "KC")][i],
           field_width - current_data$y[which(current_data$club == "KC")][i], 
           ylim[2]+5, lwd = 2, col = "#00000040", lty = 2)
  
  # Spieler-Positionen anpassen: x <-> y tauschen
  points(field_width - current_data$y[which(current_data$club == "KC")][i],
         ylim[2]+5,#rep(ylim[2]+ 5, 5),
         pch = c(16),#, 18, 16)[as.factor(current_data$club)[1:5]],
         #col = scales::alpha("black", dichte[i]),
         cex = 1)
  
}

# Spieler-Positionen anpassen: x <-> y tauschen
points(field_width - current_data$y[which(current_data$club != "KC")][1],
       ylim[2]+5,#rep(ylim[2]+ 5, 5),
       pch = c(16),#, 18, 16)[as.factor(current_data$club)[1:5]],
       col = "#97233F",
       cex = 1)

segments(field_width - current_data$y[which(current_data$club != "KC")],
         current_data$x[which(current_data$club != "KC")],
         field_width - current_data$y[which(current_data$club != "KC")], 
         ylim[2]+5, lwd = 1, col = "#97233F", lty = 2)

# Spieler-Positionen anpassen: x <-> y tauschen
points(field_width - current_data$y[which(current_data$club == "KC")],
       current_data$x[which(current_data$club == "KC")],
       pch = c(16, 18, 16)[as.factor(current_data$club)[1:5]],
       col = "white", cex = 1.5)

points(field_width - current_data$y[which(current_data$club != "KC")],
       current_data$x[which(current_data$club != "KC")],
       pch = c(16, 18, 16)[as.factor(current_data$club)[6]],
       col = "#97233F", cex = 1.5)

# KC Spieler hervorheben
points(field_width - current_data$y[which(current_data$club == "KC")],
       current_data$x[which(current_data$club == "KC")],
       pch = 1, lwd = 1.5,
       col = "#CA2430", cex = 1.5)



# points(field_width - current_data$y[which(current_data$club != "KC")],
#        rep(ylim[2]+ 5, sum(current_data$club != "KC")),
#        pch = c(16, 18, 16)[as.factor(current_data$club)[6]],
#        cex = 1.5)

# KC Spieler hervorheben
# points(field_width - current_data$y[which(current_data$club == "KC")],
#        rep(ylim[2], 5),
#        pch = 1, lwd = 1.5,
#        cex = 1.5)


# for (i in 1:6) {
#   text((i-1)*10, ylim[2]+2, (i-1)*10, col = "#00000080", lwd = 1, cex = 0.9)
# }


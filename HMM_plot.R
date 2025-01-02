library(dplyr)
library(stringr)
tracking_kc_az = read.csv("tracking_kc.csv")
defender_data = read.csv("kc_defender_data.csv")

# Spielfeld-Abmessungen (angepasst an Football)
field_length <- 120
field_width <- 53.3

team_colors = c("blue4", "black", "red")
los = 19
# Animation: Spielerbewegungen
for (t in unique(tracking_kc_az$time)) {
  # Daten für den aktuellen Zeitpunkt filtern
  current_data <- tracking_kc_az[tracking_kc_az$time == t, ]
  
  # Spielfeld zeichnen
  plot(
    NA, 
    xlim = c(min(tracking_kc_az$x)-1, max(tracking_kc_az$x)+1),#c(0, field_length), 
    ylim = c(min(tracking_kc_az$y)-1, max(tracking_kc_az$y)+1),#c(0, field_width),
    xlab = "Spielfeld-Länge", ylab = "Spielfeld-Breite",
    main = paste("Spielerbewegungen - Zeit:", t), asp = 1
  )
  grid() # Raster für das Spielfeld
  
  # Spielerpositionen hinzufügen
  points(current_data$x, current_data$y, pch = 19, 
         col = team_colors[as.factor(current_data$club)], cex = 2)
  abline(v = los)
  #text(current_data$x, current_data$y, labels = current_data$nflId, pos = 3, col = "black")
  
  # Verbindungslinien basierend auf Wahrscheinlichkeiten hinzufügen
  defender_probs <- defender_data[defender_data$time == t, ]#[[t %% length(defender_data) + 1]] # Beispiel: Wahrscheinlichkeiten für diesen Zeitpunkt
  for (i in 1:nrow(defender_probs)) {
    defender <- defender_probs$nflId[i]
    # Maximale Wahrscheinlichkeit für diesen Verteidiger
    max_prob <- which.max(defender_probs[i, 21:25])
    attacker <- as.numeric(str_sub(names(defender_probs)[max_prob + 20], 2, 6))
    
    current_data2 = current_data %>% filter(!is.na(nflId))
    # Koordinaten von Verteidiger und Angreifer finden
    defender_pos <- current_data[which(current_data2$nflId == defender), c("x", "y")]
    attacker_pos <- current_data[which(current_data2$nflId == attacker), c("x", "y")]
    
    # Linie zeichnen, falls beide gefunden wurden
    if (nrow(defender_pos) > 0 && nrow(attacker_pos) > 0) {
      lines(
        x = c(defender_pos$x, attacker_pos$x),
        y = c(defender_pos$y, attacker_pos$y),
        col = "darkgray", lwd = 2
      )
    }
  }
  # Pause für Animation
  Sys.sleep(0.1) # Wartezeit in Sekunden
}

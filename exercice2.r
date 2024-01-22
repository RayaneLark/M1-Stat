# Charger les données
data_2012 <- read.table("company2012.dat", header = FALSE)
data_2017 <- read.table("company2017.dat", header = FALSE)
data_2022 <- read.table("company2022.dat", header = FALSE)

# Nommer les colonnes si nécessaire (à ajuster en fonction de vos données)
colnames(data_2012) <- c("performances")
colnames(data_2017) <- c("performances")
colnames(data_2022) <- c("performances")

# Créer un histogramme pour chaque année et les superposer
hist(data_2012$performances, col = "blue", main = "Distribution des performances", xlab = "Performances", ylab = "Densité", probability = TRUE)
hist(data_2017$performances, col = "green", add = TRUE, probability = TRUE)
hist(data_2022$performances, col = "red", add = TRUE, probability = TRUE)

# Ajouter les courbes de densité correspondantes
lines(density(data_2012$performances), col = "blue", lwd = 2)
lines(density(data_2017$performances), col = "green", lwd = 2)
lines(density(data_2022$performances), col = "red", lwd = 2)

# Légende
legend("topright", legend = c("2012", "2017", "2022"), fill = c("blue", "green", "red"))

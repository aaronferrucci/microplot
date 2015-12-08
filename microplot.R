library(ggplot2)
data <- read.csv("cities.csv", colClasses = c("character", "integer", "character", "character", "factor"))
p <- ggplot(data, aes(x=hw, y=sw, size=points,label=city)) +
  xlab("hardware rev") +
  ylab("software rev") +
  ggtitle("microcorruption") +
  geom_point(color="white", fill="red", shape=21) +
  geom_text(size=4) +
  scale_size_area(max_size=50) +
  theme(legend.position="none",panel.grid.major = element_blank())
print(p)

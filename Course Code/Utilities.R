# Saving Files 
# For PNG file (Run entire block at once)
png(filename= "C:/Users/Sarah/Desktop/R Projects/Ex02_06a.png",  # Open device
    width = 888,
    height = 571)
par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(feeds[order(feeds)],  # Create the chart
        horiz  = TRUE,
        las    = 1,  # Orientation of axis labels
        col    = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,  # No borders on bars
        main   = "Frequencies of Different Feeds\nin chickwts Dataset",
        xlab   = "Number of Chicks")
dev.off()  # Close device (run in same block)

# OR this one for PDF file (Run entire block at once)
pdf("C:/Users/Sarah/Desktop/R Projects/Ex02_06a.pdf",
    width = 9,
    height = 6)
par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(feeds[order(feeds)],  # Create the chart
        horiz  = TRUE,
        las    = 1,  # Orientation of axis labels
        col    = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,  # No borders on bars
        main   = "Frequencies of Different Feeds\nin chickwts Dataset",
        xlab   = "Number of Chicks")
dev.off()  # Close device (run in same block)

# The easy Way: With RStudio "Export"

par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(feeds[order(feeds)],  # Create the chart
        horiz  = TRUE,
        las    = 1,  # Orientation of axis labels
        col    = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,  # No borders on bars
        main   = "Frequencies of Different Feeds\nin chickwts Dataset",
        xlab   = "Number of Chicks")

# Colors
# Built-in palettes
# rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
# heat.colors(n, alpha = 1)  # Yellow through red
# terrain.colors(n, alpha = 1)  # Gray through green
# topo.colors(n, alpha = 1)  # Purple through tan
# cm.colors(n, alpha = 1)  # Blues and pinks
help(package = colorspace)
palette()
# Change default palette
palette(terrain.colors(6))

# the color brewer
require("RColorBrewer")

# Show all of the palettes in a graphics window
display.brewer.all()

display.brewer.pal(4, "Spectral")



# Barplots
# Can save palette as vector or call in function
blues <- brewer.pal(6, "Blues")
barplot(x, col = blues)
barplot(x, col = brewer.pal(6, "Greens"))
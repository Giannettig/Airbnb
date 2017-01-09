data(nw.atlantic)
atl <- as.bathy(nw.atlantic)
library(lattice)
wireframe(unclass(atl), shade = TRUE, aspect = c(1/2, 0.1))

plot(atl, xlim = c(-70, -52),
     deep = c(-5000, 0), shallow = c(0, 0), step = c(1000, 0),
     col = c("lightgrey", "black"), lwd = c(0.8, 1),
     lty = c(1, 1), draw = c(FALSE, FALSE))
belt <- get.box(atl, x1 = -68.6, x2 = -53.7, y1 = 42.4, y2 = 32.5,
                width = 3, col = "red")
wireframe(belt, shade = TRUE, zoom = 1.1,
          aspect = c(1/4, 0.1),
          screen = list(z = -60, x = -55),
          par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = rgb(0, 0, 0, 0.1)))

https://rpubs.com/nickbearman/r-google-map-making
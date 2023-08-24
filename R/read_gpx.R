#' Read GPX File
#'
#' Reads data in GPX form.
#' Examples of tourist routes saved in this format can be downloaded from
#' mapa-turystyczna.pl.
#'
#' @param path path the the gpx file with information about the route
#' @param name name of the route
#' @param uniform if TRUE then route will be converted into a uniform grid of points
#' @param dx if uniform is TRUE then dx is the grid size
#' @param span if uniform is TRUE then span is smoothing parameter
#' @param x routs to be plotted
#' @param color names of colors for lines
#' @param type what should be plotted? 'profile' for profiles, 'difference' for derivative, 'boxplot' absolute derivative
#' @param ... other parameters
#'
#' @author
#' Przemyslaw Biecek
#' @importFrom stats loess predict
#'
#' @rdname read_gpx
#' @export
read_gpx <- function(path, name = NULL, uniform = TRUE, dx = 25, span = 0.1) {
   if (is.null(name)) {
      name <- gsub(path, pattern = ".gpx", replacement = "")
   }
   pfile <- XML::htmlTreeParse(path, error = function (...) {}, useInternalNodes = T)
   # Get all elevations, times and coordinates via the respective xpath
   elevations <- as.numeric(XML::xpathSApply(pfile, path = "//trkpt/ele", XML::xmlValue))
   coords <- XML::xpathSApply(pfile, path = "//trkpt", XML::xmlAttrs)
   # Extract latitude and longitude from the coordinates
   lats <- as.numeric(coords["lat",])
   lons <- as.numeric(coords["lon",])
   # convert to geo points
   points <- sp::SpatialPoints(data.frame(lons, lats))
   mat_dists <- raster::pointDistance(points, lonlat = TRUE)
   cons_dists <- diag(mat_dists[-1,])

   df <- data.frame(lon = lons, lat = lats,
              ele = elevations, ele0 = elevations - elevations[1],
              dist = cumsum(c(0,cons_dists)),
              d_ele = c(0, diff(elevations)), d_dist = c(0,cons_dists),
              dd = c(0, diff(elevations))/c(0,cons_dists),
              name = name)
   class(df) = c("gpx_file", "data.frame")

   if (uniform) {
      dist_seq <- seq(min(df$dist), max(df$dist), by = dx)
      ele_seq <- predict(loess(ele~dist, data = df, span = span, degree = 1),
                         data.frame(dist = dist_seq))
      df <- data.frame(ele = ele_seq, ele0 = ele_seq - ele_seq[1],
                 dist = dist_seq,
                 d_ele = c(0, diff(ele_seq)), d_dist = c(0,diff(dist_seq)),
                 dd = c(0, diff(ele_seq))/c(0,diff(dist_seq)),
                 name = df$name[1])
      class(df) = c("gpx_file", "data.frame")
   }

   df
}

#' @rdname read_gpx
#' @export
plot.gpx_file <- function(x, ...,
                          type = "profile",
                          color = "magenta") {
   for (sx in list(...)) {
      if ("gpx_file" %in% class(sx)) {
         x <- rbind(x, sx)
      }
   }
   dist = ele = dd = name = route = NULL
   # one plot or more?
   if (length(unique(x$name)) > 1) {
      x$name <- factor(x$name, levels = unique(x$name))
      x$route <- as.numeric(x$name)
      # more than one profile
      if (type == "profile") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(dist, ele, color = route, group = name)) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            DALEX::theme_ema() +
            ggplot2::ylab("Wysokosc n.p.m. [m]") +
            ggplot2::xlab("Odleglosc od poczatku szlaku [m]") +
            ggplot2::ggtitle(paste0(unique(x$name), collapse = ", ")) +
            ggplot2::scale_color_gradient(low = color, high = "black")
      }
      if (type == "difference") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(dist, dd, color = route, group = name)) +
            ggplot2::geom_smooth(se= FALSE, span= 0.2, method = "loess", formula = y~x) +
            DALEX::theme_ema() +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::ylab("Zmiana wysokosci [m/m]") +
            ggplot2::xlab("Odleglosc od poczatku szlaku [m]") +
            ggplot2::ggtitle(paste0(unique(x$name), collapse = ", ")) +
            ggplot2::scale_color_gradient(low = color, high = "black")
      }
      if (type == "boxplot") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(abs(dd), color = route, group = name)) +
            ggplot2::geom_boxplot(size=2,alpha=.20) +
            DALEX::theme_ema_vertical() +
            ggplot2::ylab("") +
            ggplot2::xlab("Bezwzgledne nachylenie szlaku [m/m]") +
            ggplot2::scale_y_continuous("", breaks = NULL) +
            ggplot2::scale_color_gradient(low = color, high = "black") +
            ggplot2::facet_wrap(~name, ncol=1) +
            ggplot2::ggtitle("")
      }
   } else {
      # just one plot
      if (type == "profile") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(dist, ele)) +
            ggplot2::geom_line(color = color) +
            ggplot2::geom_point(color = color) +
            DALEX::theme_ema() +
            ggplot2::ylab("Wysokosc n.p.m. [m]") +
            ggplot2::xlab("Odleglosc od poczatku szlaku [m]") +
            ggplot2::ggtitle(paste0(unique(x$name)))
      }
      if (type == "difference") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(dist, dd)) +
            ggplot2::geom_smooth(se= FALSE, span= 0.2, method = "loess", formula = y~x) +
            DALEX::theme_ema() +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::ylab("Zmiana wysokosci [m/m]") +
            ggplot2::xlab("Odleglosc od poczatku szlaku [m]") +
            ggplot2::ggtitle(paste0(unique(x$name)))
      }
      if (type == "boxplot") {
         pl <- ggplot2::ggplot(x, ggplot2::aes(abs(dd))) +
            ggplot2::geom_boxplot(color = color, size=2,alpha=.20) +
            DALEX::theme_ema_vertical() +
            ggplot2::ylab("") +
            ggplot2::xlab("Bezwzgledne nachylenie szlaku [m/m]") +
            ggplot2::scale_y_continuous("", breaks = NULL) +
            ggplot2::ggtitle(paste0(unique(x$name)))
      }
   }
   pl
}


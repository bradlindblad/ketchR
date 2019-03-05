
#################
## Footprint ####
## Polygons #####
#################

# To generate the footprint_polys data
footprint_poly <- function() {

  #' Returns object of class SpatialPolygons of the AgC footprint.
  #' Utilizes the Tigris:: package.


  require(tigris)
  require(sf)
  # Get County Polygons
  states.raw <- tigris::states()

  states <- states.raw[states.raw@data$STUSPS %in% c("CA", "OR", "WA"),]

  states <- sf::st_as_sfc(states)
  states <- sf::st_union(states)
  states <- as(states, 'Spatial')

  return(states)


}


#################
## Create #######
## Dataset ######
#################

# branches <- read.csv("O:\\exchange\\branches.csv", header = T)
# save(branches, file = "data/branches.RData")

#################
## Query ########
## Database #####
#################

get_db <- function(query = "SELECT TOP 10 * FROM datas.dbo.Customers") {

  #' Pull data from g23 database
  #' @param query: enter a SQL query; Microsoft SQL syntax please

  require(odbc)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "datas",
                   Database = "dataserver",
                   UID = "user",
                   PWD = rstudioapi::askForSecret("password"),
                   Port = 6969)



  z <- odbc::dbGetQuery(con, query)

  return(z)

  odbc::dbDisconnect(con)

}


get_customers <- function() {

  #' Pull most recent customer data from G23 - datascience.agc_Customers

  require(odbc)
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = "datas",
                   Database = "dataserver",
                   UID = "user",
                   PWD = rstudioapi::askForSecret("password"),
                   Port = 6969)


  query1 <- "SELECT * FROM datas.dbo.Customers"
z <- odbc::dbGetQuery(con, query1)

return(z)

odbc::dbDisconnect(con)


}

#################
## Color ########
## Pallette #####
#################


# Palette main colors
ketch.styles <- c(
  `salmon` = "#F16876",
  `light_blue`= "#00A7E6",
  `light_grey` = "#E8ECF8",
  `brown`  = "#796C68")

# Fn to extract them by hex codes
styles <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (ketch.styles)

  ketch.styles[cols]
}

# Create separate palettes
ketch.palettes <- list(
  `main`  = styles('salmon','light_blue', 'brown', 'light_grey'),

  `cool`  = styles('light_blue', 'light_grey')
)

# Fn to access them
ketch_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ketch.palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# Fn for customer scale
scale_color_ketch <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ketch_pal(palette = palette, reverse = reverse)

  #' Scale color using AgC color palette.
  #' @param palette: main, greens or greys
  #' @param discrete: T or F
  #' @param reverse: reverse the direction of the color scheme

  if (discrete) {
    discrete_scale("colour", paste0("ketch_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_ketch <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  #' Scale fill using AgC color palette.
  #' @param palette: main, greens or greys
  #' @param discrete: T or F
  #' @param reverse: reverse the direction of the color scheme

  pal <- ketch_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("ketch_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



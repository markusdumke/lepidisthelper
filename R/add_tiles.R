#' Add tiles to leaflet map
#'
#' @param .map Leaflet map object.
#' @param .position Character value. One of "topleft", "topright", "bottomleft", or "bottomright".
#'
#' @export
#' @import leaflet
#' @importFrom magrittr "%>%"
add_tiles <- function(.map, .position = "bottomright") {
  .map %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellit") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Gelaende") %>%
    # addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
    #          group = "geo.admin Schweiz") %>%
    addLayersControl(
      baseGroups = c("Gelaende", "Satellit", "OSM"),
      position = .position,
      options = layersControlOptions(collapsed = TRUE))
}


#' Add search OSM to leaflet map
#'
#' @param .map Leaflet map object.
#' @param .position Character value. One of "topleft", "topright", "bottomleft", or "bottomright".
#'
#' @export
#' @import leaflet leaflet.extras
#' @importFrom magrittr "%>%"
add_search_osm <- function(.map, .position = "topleft") {
  .map %>%
    addSearchOSM(
    options = searchOptions(
      position = .position,
      zoom = 14,
      textPlaceholder = "Suche ...", # FIXME: placeholder does not work -> BUG?
      textErr = "Ort nicht gefunden",
      textCancel = "Abbrechen",
      autoType = FALSE,
      collapsed = TRUE,
      hideMarkerOnCollapse = TRUE,
      autoCollapse = TRUE
    )
  )
}

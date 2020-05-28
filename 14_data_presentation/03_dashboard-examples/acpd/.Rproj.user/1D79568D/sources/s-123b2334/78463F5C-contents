make_crime_map <- function(crime_type = "Drunkenness") {
  mapname <- paste0(make.names(crime_type), "_map")
  mapfile <- paste0(make.names(mapname), ".RDS")

  if (exists(mapname)) {
    print("map exists")
    #browser()
    get(mapname)
  } else if (file.exists(mapfile)) {
    print("file exists")
    assign(mapname, readRDS(mapfile), envir = .GlobalEnv)
    #browser()
    get(mapname)
  } else {
    crime_tp <- crime_type
    crime_years <- c(2015, 2016, 2017, 2018)

    # Load census_blocks
    if (!file.exists("census_blocks.RDS")) {
      conn <- get_db_conn(db_name = "acpd", db_user = "acpd_user", db_pass = "acpd")
      census_blocks <-
        st_read(dsn = conn, layer = "arlington_census_blocks")
      dbDisconnect(conn)
      saveRDS(census_blocks, "census_blocks.RDS")
    } else
      census_blocks <- readRDS("census_blocks.RDS")

    # Load or create map_pnts_sf (police_incidents)
    if (!file.exists("police_incidents.RDS")) {
      conn <- get_db_conn(db_name = "acpd", db_user = "acpd_user", db_pass = "acpd")
      police_incidents <- dbReadTable(conn = conn,
                                      name = "incidents_filtered") %>%
        dt_select(
          id,
          description,
          start,
          crime_category,
          nearby,
          day,
          hour,
          nightlife,
          longitude,
          latitude
        ) %>%
        dt_mutate(crime_year = year(x = start)) %>%
        st_as_sf(coords = c("longitude", "latitude")) %>%
        st_set_crs(value = st_crs(x = census_blocks))
      dbDisconnect(conn)
      map_pnts_sf <- police_incidents
      saveRDS(police_incidents, "map_pnts_sf.RDS")
    } else map_pnts_sf <- readRDS("map_pnts_sf.RDS")

    # Load or create map_polys_sf
    if (!file.exists("map_polys_sf.RDS")) {
      skeleton <-
        expand.grid(na.omit(object = unique(x = census_blocks$fullblockid)),
                    na.omit(object = unique(x = police_incidents$crime_year)),
                    na.omit(object = unique(x = police_incidents$crime_category))) %>%
        data.table() %>%
        setNames(nm = c("geoid10", "crime_year", "crime_category"))
      st_join(x = census_blocks,
              y = police_incidents) %>%
        dt_select(fullblockid, crime_year, crime_category, id) %>%
        setnames(old = "fullblockid", new = "geoid10") %>%
        merge(y = skeleton, all.y = TRUE) %>%
        dt_mutate(count := sum(x = !is.na(x = id)),
                  by = c("geoid10", "crime_year", "crime_category")) %>%
        dt_select("geoid10", "crime_year", "crime_category", "count") %>%
        unique() %>%
        dcast(geoid10 + crime_year ~ crime_category, value.var = "count") %>%
        merge(
          y = census_blocks %>%
            dt_select(fullblockid, geometry) %>%
            setnames(old = "fullblockid", new = "geoid10"),
          by = "geoid10"
        ) %>%
        dt_arrange(geoid10, crime_year) %>%
        st_as_sf() -> tmp

      map_polys_sf <- rmapshaper::ms_simplify(input = as(tmp, 'Spatial'), keep = .001) %>%
        st_as_sf()

      saveRDS(map_polys_sf, "map_polys_sf.RDS")
    } else
      map_polys_sf <- readRDS("map_polys_sf.RDS")

    # load or create restauants
    if (!file.exists("restaurants.RDS")) {
      conn <- get_db_conn(db_name = "acpd", db_user = "acpd_user", db_pass = "acpd")
      restaurants <- dbReadTable(conn = conn,
                                 name = 'vabc_arlington_restaurants') %>%
        setDT() %>%
        dt_mutate(priv = str_detect(string = priv_desc,
                                    pattern = "(?i)(wine|beer)")) %>%
        dt_filter(priv) %>%
        dt_filter(lic_status_status_desc %in% "Active") %>%
        st_as_sf(coords = c("x", "y"))
      dbDisconnect(conn)
      saveRDS(restaurants, "restaurants.RDS")
    } else
      restaurants <- readRDS("restaurants.RDS")

    # Create pnts_2_sf (restaurant points)
    pnts_2_sf <- restaurants %>%
      dt_select(key, restaurant, address, ari, ask_angela, geometry) %>%
      distinct(.keep_all = T)

    skeleton <-
      expand.grid(address = na.omit(object = unique(x = pnts_2_sf$address)),
                  year = na.omit(object = unique(x = crime_years))) %>% setDT()

    pnts_2_sf <- merge(pnts_2_sf, skeleton, by = 'address')


    # Load or create map_pnts_2_sf (restaurant violations)
    # bring in violations data and filter to violations in past month
    if (!file.exists("map_pnts_2_sf.RDS")) {
      conn <- get_db_conn(db_name = "acpd", db_user = "acpd_user", db_pass = "acpd")
      violations <- dbReadTable(conn = conn,
                                name = 'abc_violations') %>%
        data.table()
      dbDisconnect(conn)

      violations_by_rest <-
        violations %>%  dt_mutate(year = year(violation_date)) %>%
        group_by(year, licensee_name, physical_address) %>%
        arrange(physical_address, licensee_name, year) %>%
        summarise(
          all_charges  = paste(unique(charges), collapse = "; "),
          total_charges = sum(number_of_charges)
        )  %>%
        arrange(physical_address,
                licensee_name,
                year,
                desc(total_charges),
                all_charges)

      violations_complete <- violations_by_rest %>%
        data.table() %>%
        dt_mutate(key = paste(substr(tolower(
          licensee_name
        ), 1, 5), substr(tolower(
          physical_address
        ), 1, 5)) %>%
          str_remove_all(pattern = "\\s"))

      pnts_2_sf_violations <-
        merge(
          pnts_2_sf,
          violations_complete,
          by = c('key', 'year'),
          all.x = TRUE,
          fill = TRUE
        )
      pnts_2_sf_violations$total_charges[is.na(pnts_2_sf_violations$total_charges)] <-
        0
      pnts_2_sf_violations <-
        pnts_2_sf_violations %>% data.table() %>% st_as_sf()


      # Prepare Second Point Dataset for Mapping
      within_circle <- function(lon, lat, ctr_pnt = 402.336) {
        distm(x = c(-77.09523, 38.8871),
              y = c(lon, lat)) < ctr_pnt
      }
      . <-
        cbind(pnts_2_sf_violations,
              sf::st_coordinates(pnts_2_sf_violations))
      .$in_circle <- mapply(within_circle, .$X, .$Y)
      map_pnts_2_sf <- .
      saveRDS(map_pnts_2_sf, "map_pnts_2_sf.RDS")
    } else
      map_pnts_2_sf <- readRDS("map_pnts_2_sf.RDS")

    # Map Polygons and Points
    # color palette function
    pal <- leaflet::colorBin(
      palette = "viridis",
      bins = c(1, 2, 4, 6, 8, 10, 12, 14, 18),
      reverse = TRUE
    )
    pal2 <- leaflet::colorFactor(c("gray17", "darkblue"),
                                 map_pnts_2_sf$ari)


    # Create map
    print("Building Map...")
    m <-
      leaflet::leaflet(options = leafletOptions(zoomControl = FALSE))
    m <- htmlwidgets::onRender(m, "function(el, x) {
                                      L.control.zoom({ position: 'bottomleft' }).addTo(this)
                                  }")

    m <- leaflet::setView(m, -77.09500, 38.88700, 17)
    m <- leaflet::addTiles(m)
    #m <- addProviderTiles(m, providers$Stamen.Toner)
    m <- leaflet::addMapPane(m, "base_layers", zIndex = 410)
    m <- leaflet::addMapPane(m, "boundaries", zIndex = 420)
    m <- leaflet::addMapPane(m, "under_places", zIndex = 405)
    m <- leaflet::addMapPane(m, "places", zIndex = 440)
    #m <- addMapboxGL(m, style = "mapbox://styles/mapbox/streets-v9")

    # add polygon data layers
    print("Adding Polygon Layers...")
    for (c in crime_years) {
      plydt <- map_polys_sf %>%
        data.table() %>%
        dt_filter(crime_year == c & get(make.names(crime_tp)) > 0) %>%
        st_as_sf()

      labels <- lapply(
        paste(
          "<strong>Year:</strong>",
          c,
          "<br />",
          "<strong/>County:</strong>",
          substr(plydt$geoid10, 3, 5),
          "<br />",
          "<strong>Tract:</strong>",
          substr(plydt$geoid10, 6, 11),
          "<br />",
          "<strong>Block Group:</strong>",
          substr(plydt$geoid10, 12, 12),
          "<br />",
          "<strong>Crime Type:</strong>",
          crime_tp,
          "<br />",
          "<strong>Measure:</strong> count
          <br />",
          "<strong>Value:</strong>",
          plydt[, make.names(crime_tp)][[1]]
        ),
        htmltools::HTML
      )
      m <- leaflet::addPolygons(
        m,
        data = plydt,
        stroke = TRUE,
        weight = .8,
        color = "Black",
        smoothFactor = 0.2,
        fillOpacity = .8,
        fillColor = ~ pal(get(make.names(crime_tp))),
        label = labels,
        group = as.character(c),
        options = leaflet::pathOptions(pane = "base_layers")
      )
    }

    # add point data layers
    print("Adding Point Layers...")
    for (c in crime_years) {
      pnt_dt <-
        dplyr::filter(map_pnts_sf, crime_year == c &
                        crime_category == crime_tp)

      labels <- lapply(
        paste(
          "<strong>Crime Description:</strong>",
          pnt_dt$description,
          "<br />",
          "<strong>Crime Timestamp:</strong>",
          pnt_dt$start,
          "<br />"
        ),
        htmltools::HTML
      )

      m <- leaflet::addCircleMarkers(
        m,
        data = pnt_dt,
        label = labels,
        radius = 3,
        color = "black",
        group = as.character(c),
        clusterOptions = leaflet::markerClusterOptions(),
        options = leaflet::pathOptions(pane = "places")
      )
    }

    # add study circle
    m <- leaflet::addCircles(
      m,
      lng = -77.09523,
      lat = 38.8871,
      weight = 5,
      stroke = TRUE,
      color = "Black",
      fillColor = "Black",
      fillOpacity = .1,
      radius = 402.336,
      group = "study circle",
      options = leaflet::pathOptions(pane = "under_places")
    )


    print("Adding Marker Layers...")
    ari_tf <- map_pnts_2_sf$ari
    getColor <- function(aritf) {
      sapply(aritf, function(ARI) {
        if (ARI == TRUE) {
          "darkblue"
        } else {
          "lightblue"
        }
      })
    }

    icons <- leaflet::awesomeIcons(
      icon = 'fa-cutlery',
      library = 'fa',
      markerColor = getColor(ari_tf)
    )

    # add restaurant tooltips
    for (c in crime_years) {
      data <- dplyr::filter(map_pnts_2_sf, year == c)

      rest_label <- lapply(
        paste(
          "<strong>Year:</strong>",
          data$year,
          "<br />",
          "<strong>Address:</strong>",
          data$address,
          "<br />",
          "<strong>Restaurant:</strong>",
          data$restaurant,
          "<br />",
          "<strong>Ask Angela:</strong>",
          data$ask_angela,
          "<br />",
          "<strong>Total Charges:</strong>",
          data$total_charges,
          "<br />",
          "<strong>Charges Types:</strong>",
          data$all_charges
        ),
        htmltools::HTML
      )

      m <- leaflet::addAwesomeMarkers(
        m,
        data = map_pnts_2_sf,
        group = as.character(c),
        icon = icons,
        label = rest_label,
        options = leaflet::pathOptions(pane = "places")
      )
    }

    # add Layer Control
    print("Building Controls...")
    m <- leaflet::addLayersControl(
      m,
      baseGroups = as.character(crime_years),
      overlayGroups = c("study circle"),
      options = leaflet::layersControlOptions(collapsed = F)
    )

    m <- leaflet::showGroup(m, crime_years[1])

    # add Legend
    m <- leaflet::addLegend(
      m,
      position = "bottomright",
      pal = pal,
      values = c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18),
      title = "Crime Count",
      opacity = 1
    )
    saveRDS(m, mapfile)

    print("Launching Map...")
    m
  }
}

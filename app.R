# app.R — Kelantan Spatial Kit (KSK) v2 — deployment-safe
# Simple district-level communicable disease analysis for non-coders
# Pages:
# 1 Landing + upload + ANALYSE button
# 2 Data checking
# 3 Descriptive analysis + epid week trend
# 4 Point map
# 5 Incidence map
# 6 Summary report + Word-compatible download

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(sf)
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(ggplot2)
  library(leaflet)
  library(DT)
  library(htmltools)
})

# Deployment-safe Word-compatible report uses HTML saved as .doc.
# This avoids officer/flextable and prevents heavy build dependencies.

# -----------------------------
# Helper functions
# -----------------------------
read_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xlsx") {
    readxl::read_xlsx(path) |> janitor::clean_names()
  } else {
    readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names()
  }
}

read_boundary <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "gpkg") return(sf::st_read(path, quiet = TRUE))
  if (ext == "zip") {
    td <- tempfile("ksk_shp_")
    dir.create(td)
    unzip(path, exdir = td)
    shp <- list.files(td, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    validate(need(length(shp) >= 1, "No .shp file found inside the zip file."))
    return(sf::st_read(shp[1], quiet = TRUE))
  }
  validate(need(FALSE, "Boundary must be a zipped shapefile (.zip) or GeoPackage (.gpkg)."))
}

first_present <- function(candidates, cols) {
  idx <- match(tolower(candidates), tolower(cols))
  if (any(!is.na(idx))) cols[stats::na.omit(idx)[1]] else NA_character_
}

to_upper_squish <- function(x) {
  x |> as.character() |> stringr::str_squish() |> stringr::str_to_upper(locale = "en")
}

find_col <- function(cols, candidates) {
  lc <- tolower(cols)
  for (cand in candidates) {
    idx <- match(tolower(cand), lc, nomatch = 0L)
    if (idx > 0) return(cols[idx])
  }
  for (cand in candidates) {
    hit <- which(grepl(cand, lc, perl = TRUE))
    if (length(hit) > 0) return(cols[hit[1]])
  }
  NA_character_
}

safe_text <- function(x, default = "Not available") {
  if (length(x) == 0 || all(is.na(x))) default else as.character(x[1])
}



html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("<p>No data available.</p>")
  df <- as.data.frame(df)
  header <- paste0("<tr>", paste0("<th>", htmltools::htmlEscape(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- apply(df, 1, function(row) {
    paste0("<tr>", paste0("<td>", htmltools::htmlEscape(as.character(row)), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table>", header, paste(rows, collapse = ""), "</table>")
}
# Light simplification for web maps. EPSG:4326 degrees; 0.001 ~ 110m.
simplify_for_web <- function(gdf, tolerance = 0.001) {
  gdf <- sf::st_make_valid(gdf)
  gdf <- suppressWarnings(sf::st_collection_extract(gdf, "POLYGON"))
  gdf <- sf::st_zm(gdf, drop = TRUE, what = "ZM")
  gdf <- sf::st_simplify(gdf, dTolerance = tolerance, preserveTopology = TRUE)
  sf::st_make_valid(gdf)
}

standardize_boundary <- function(gdf, tolerance = 0.001) {
  if (is.na(sf::st_crs(gdf))) {
    sf::st_crs(gdf) <- 4326
  } else {
    gdf <- sf::st_transform(gdf, 4326)
  }

  nm <- names(gdf)
  mukim_candidates <- c(
    "map_subdist", "mukim", "mukim_name", "nama_mukim", "namamukim",
    "subdistrict", "sub_district", "sub-district", "subdist", "name", "nama", "namobj"
  )
  district_candidates <- c("district", "daerah", "jajahan", "nama_daerah", "namadaerah")

  mukim_col <- first_present(mukim_candidates, nm)
  if (is.na(mukim_col)) mukim_col <- nm[1]
  district_col <- first_present(district_candidates, nm)

  gdf |>
    mutate(
      MAP_SUBDIST = to_upper_squish(.data[[mukim_col]]),
      DISTRICT_STD = if (!is.na(district_col)) to_upper_squish(.data[[district_col]]) else "KELANTAN"
    ) |>
    select(MAP_SUBDIST, DISTRICT_STD, geometry) |>
    simplify_for_web(tolerance = tolerance)
}

clean_disease <- function(df) {
  sub_col  <- find_col(names(df), c("subdistrict", "mukim", "mukim_zon", "zon", "sub_district", "sub-district", "lokaliti"))
  dist_col <- find_col(names(df), c("district", "daerah", "jajahan"))
  year_col <- find_col(names(df), c("year", "epid_year", "epid_tahun.*", "tahun"))
  week_col <- find_col(names(df), c("epid_week", "epid_minggu.*", "minggu", "week"))
  lon_col  <- find_col(names(df), c("longitude", "long", "lon", "lng", "xcoord", "x"))
  lat_col  <- find_col(names(df), c("latitude", "lat", "ycoord", "y"))

  validate(need(!is.na(sub_col),  "Disease subdistrict/mukim column not detected."))
  validate(need(!is.na(year_col), "Disease year column not detected."))
  validate(need(!is.na(lon_col),  "Disease longitude column not detected."))
  validate(need(!is.na(lat_col),  "Disease latitude column not detected."))

  sex_col  <- find_col(names(df), c("sex", "jantina", "gender"))
  eth_col  <- find_col(names(df), c("ethnicity", "keturunan", "bangsa", "kaum"))
  age_col  <- find_col(names(df), c("age", "umur"))
  diag_col <- find_col(names(df), c("diagnosis", "diagnosis_status", "final_diagnosis", "diagnosa", "classification", "klasifikasi"))

  tibble(
    district = if (!is.na(dist_col)) to_upper_squish(df[[dist_col]]) else NA_character_,
    subdistrict = to_upper_squish(df[[sub_col]]),
    year = suppressWarnings(as.integer(df[[year_col]])),
    week = if (!is.na(week_col)) suppressWarnings(as.integer(df[[week_col]])) else NA_integer_,
    longitude = suppressWarnings(as.numeric(df[[lon_col]])),
    latitude = suppressWarnings(as.numeric(df[[lat_col]])),
    age = if (!is.na(age_col)) suppressWarnings(as.numeric(df[[age_col]])) else NA_real_,
    sex = if (!is.na(sex_col)) as.character(df[[sex_col]]) else NA_character_,
    ethnicity = if (!is.na(eth_col)) as.character(df[[eth_col]]) else NA_character_,
    diagnosis = if (!is.na(diag_col)) as.character(df[[diag_col]]) else NA_character_
  ) |>
    filter(!is.na(year), !is.na(longitude), !is.na(latitude))
}

clean_population <- function(pop) {
  pop_sub <- find_col(names(pop), c("subdistrict", "mukim", "sub_district", "sub-district"))
  validate(need(!is.na(pop_sub), "Population subdistrict/mukim column not detected."))
  pop <- pop |> rename(.sub = all_of(pop_sub))

  year_col <- find_col(names(pop), c("year", "tahun"))
  pop_col  <- find_col(names(pop), c("population", "pop", "penduduk"))

  if (!is.na(year_col) && !is.na(pop_col) && year_col != pop_col) {
    pop <- pop |> rename(.year = all_of(year_col), .pop = all_of(pop_col))
  } else {
    year_cols <- names(pop)[str_detect(names(pop), "(?i)^(x)?20[0-9]{2}$")]
    if (length(year_cols) == 0) year_cols <- names(pop)[str_detect(names(pop), "20[0-9]{2}")]
    validate(need(length(year_cols) > 0, "Population must be long format or have year columns such as 2021, 2022, 2023."))
    pop <- pop |> pivot_longer(all_of(year_cols), names_to = ".year", values_to = ".pop")
  }

  pop |>
    transmute(
      subdistrict = to_upper_squish(.sub),
      year = suppressWarnings(as.integer(str_extract(as.character(.year), "20[0-9]{2}"))),
      population = suppressWarnings(as.numeric(.pop))
    ) |>
    filter(!is.na(subdistrict), !is.na(year), !is.na(population), population > 0)
}

make_descriptive_table <- function(df) {
  df |>
    group_by(year) |>
    summarise(
      cases = n(),
      median_age = if (all(is.na(age))) NA_real_ else median(age, na.rm = TRUE),
      male = sum(str_to_upper(sex) %in% c("MALE", "LELAKI", "M"), na.rm = TRUE),
      female = sum(str_to_upper(sex) %in% c("FEMALE", "PEREMPUAN", "F"), na.rm = TRUE),
      subdistricts_affected = n_distinct(subdistrict),
      .groups = "drop"
    ) |>
    mutate(
      male_percent = ifelse(cases > 0, round(male / cases * 100, 1), NA_real_),
      female_percent = ifelse(cases > 0, round(female / cases * 100, 1), NA_real_)
    ) |>
    arrange(year)
}

make_incidence <- function(df, pop, boundary) {
  df |>
    count(subdistrict, year, name = "cases") |>
    left_join(pop, by = c("subdistrict", "year")) |>
    mutate(incidence_per_1000 = cases / population * 1000) |>
    right_join(st_drop_geometry(boundary) |> distinct(MAP_SUBDIST, DISTRICT_STD), by = c("subdistrict" = "MAP_SUBDIST"))
}

report_text <- function(filtered_df, desc, inc, selected_district, selected_years) {
  total_cases <- nrow(filtered_df)
  years_txt <- paste(sort(unique(filtered_df$year)), collapse = ", ")
  district_txt <- if (identical(selected_district, "All districts")) "all selected districts" else selected_district
  peak_year <- desc |> arrange(desc(cases)) |> slice(1)
  peak_week <- filtered_df |>
    filter(!is.na(week), week >= 1, week <= 53) |>
    count(year, week, name = "cases") |>
    arrange(desc(cases)) |>
    slice(1)
  top_sub <- filtered_df |>
    count(subdistrict, name = "cases") |>
    arrange(desc(cases)) |>
    slice_head(n = 5)
  high_inc <- inc |>
    filter(!is.na(incidence_per_1000)) |>
    arrange(desc(incidence_per_1000)) |>
    slice_head(n = 5)

  p1 <- paste0("This KSK analysis covers ", total_cases, " cases in ", district_txt,
               " for year(s): ", years_txt, ".")
  p2 <- if (nrow(peak_year) > 0) {
    paste0("The highest number of reported cases was in ", peak_year$year[1],
           " with ", peak_year$cases[1], " cases.")
  } else "The highest case year could not be determined."
  p3 <- if (nrow(peak_week) > 0) {
    paste0("The highest weekly count was observed in epidemiological week ", peak_week$week[1],
           " of ", peak_week$year[1], " with ", peak_week$cases[1], " cases.")
  } else "Epidemiological week information was not available or not valid."
  p4 <- if (nrow(top_sub) > 0) {
    paste0("The main affected subdistricts by case count were ",
           paste0(top_sub$subdistrict, " (", top_sub$cases, ")", collapse = ", "), ".")
  } else "Subdistrict ranking could not be produced."
  p5 <- if (nrow(high_inc) > 0) {
    paste0("For incidence mapping, the highest incidence areas were ",
           paste0(high_inc$subdistrict, " (", round(high_inc$incidence_per_1000, 2), " per 1,000)", collapse = ", "), ".")
  } else "Incidence could not be calculated for some areas because population matching was incomplete."

  c(p1, p2, p3, p4, p5)
}

# -----------------------------
# UI
# -----------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#212529",
    primary = "#343a40",
    secondary = "#6c757d",
    success = "#343a40",
    base_font = font_google("Inter")
  ),
  tags$head(tags$style(HTML("
    body{background:#f4f5f6;color:#212529;}
    .nav-tabs{border-bottom:2px solid #343a40;}
    .nav-tabs .nav-link{color:#343a40;font-weight:600;}
    .nav-tabs .nav-link.active{background:#343a40 !important;color:#ffffff !important;border-color:#343a40 #343a40 #343a40;}
    .ksk-hero{background:#ffffff;border-radius:18px;padding:26px;margin-bottom:18px;border:1px solid #d9dde1;box-shadow:0 4px 14px rgba(0,0,0,0.06);}
    .ksk-card{background:#ffffff;border-radius:14px;padding:18px;border:1px solid #d9dde1;margin-bottom:14px;box-shadow:0 2px 8px rgba(0,0,0,0.04);}
    .ksk-logo-wrap{text-align:center;margin-bottom:16px;}
    .ksk-logo{max-width:260px;width:80%;height:auto;}
    .small-note{font-size:12px;color:#555}
    .btn-success,.btn-dark{background:#343a40 !important;border-color:#343a40 !important;color:#ffffff !important;}
    .form-control,.form-select{border-color:#adb5bd;}
    .ok{color:#198754}.warn{color:#d39e00}.bad{color:#dc3545}
  "))),
  titlePanel("Kelantan Spatial Kit (KSK)"),

  navset_tab(
    id = "main_tabs",

    nav_panel(
      "Homepage",
      div(class = "ksk-hero",
          div(class = "ksk-logo-wrap",
              tags$img(src = "ksk_logo.png", class = "ksk-logo", alt = "Kelantan Spatial Kit logo")
          ),
          h2("Communicable Disease District Analysis Made Simple"),
          p("KSK helps district health teams analyse communicable disease data without coding. Upload the disease line list, population file, and Kelantan boundary map, then click ANALYSE."),
          fluidRow(
            column(4, div(class = "ksk-card", h4("1. Descriptive analysis"), p("Summarises cases by year, age, sex, affected subdistricts, and weekly trend. Useful for monthly, outbreak, and annual reporting."))),
            column(4, div(class = "ksk-card", h4("2. Case point map"), p("Plots case locations on a map to help officers visually detect clustering, repeated affected areas, and spatial concentration across one or multiple years."))),
            column(4, div(class = "ksk-card", h4("3. Incidence map"), p("Calculates cases per 1,000 population for each mukim/subdistrict. This helps compare risk between areas fairly, because areas with larger population naturally have more cases.")))
          )
      ),
      fluidRow(
        column(4, fileInput("disease_file", "Disease line list (.xlsx/.csv)", accept = c(".xlsx", ".csv"))),
        column(4, fileInput("pop_file", "Population file (.xlsx/.csv)", accept = c(".xlsx", ".csv"))),
        column(4, fileInput("map_file", "Kelantan boundary map (.zip/.gpkg)", accept = c(".zip", ".gpkg")))
      ),
      fluidRow(
        column(4, textInput("disease_name", "Disease name for report", value = "HFMD")),
        column(4, sliderInput("simplify_tol", "Boundary simplification", min = 0.0001, max = 0.005, value = 0.001, step = 0.0001)),
        column(4, br(), actionButton("analyse", "ANALYSE", class = "btn btn-dark btn-lg"))
      ),
      verbatimTextOutput("analysis_status")
    ),

    nav_panel(
      "Data checking",
      fluidRow(column(4, uiOutput("filter_year_check")), column(4, uiOutput("filter_district_check"))),
      h4("First 10 rows after cleaning"),
      DTOutput("tbl_check")
    ),

    nav_panel(
      "Descriptive & trend",
      fluidRow(column(4, uiOutput("filter_year_desc")), column(4, uiOutput("filter_district_desc"))),
      h4("A. Descriptive result by available year"),
      DTOutput("tbl_desc"),
      br(),
      h4("B. Epid week line graph"),
      plotOutput("plot_week", height = "380px")
    ),

    nav_panel(
      "Point map",
      fluidRow(column(4, uiOutput("filter_year_point")), column(4, uiOutput("filter_district_point")), column(4, checkboxInput("clip_point", "Keep points inside selected district", TRUE))),
      leafletOutput("map_point", height = "650px")
    ),

    nav_panel(
      "Incidence map",
      fluidRow(column(4, uiOutput("filter_year_inc")), column(4, uiOutput("filter_district_inc"))),
      leafletOutput("map_inc", height = "650px"),
      br(),
      h4("Incidence table"),
      DTOutput("tbl_inc")
    ),

    nav_panel(
      "Summary report",
      fluidRow(column(4, uiOutput("filter_year_report")), column(4, uiOutput("filter_district_report")), column(4, br(), downloadButton("download_doc", "Download Word-compatible report"))),
      h4("Auto-generated report text"),
      uiOutput("summary_report"),
      br(),
      h4("Maps included in report preview"),
      fluidRow(
        column(6, h5("A. Point map of cases"), leafletOutput("map_report_point", height = "430px")),
        column(6, h5("B. Incidence map"), leafletOutput("map_report_inc", height = "430px"))
      ),
      br(),
      h4("Tables included in report"),
      DTOutput("tbl_report_desc")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  analysed <- eventReactive(input$analyse, {
    req(input$disease_file, input$pop_file, input$map_file)
    disease_raw <- read_tabular(input$disease_file$datapath)
    pop_raw <- read_tabular(input$pop_file$datapath)
    map_raw <- read_boundary(input$map_file$datapath)

    boundary <- standardize_boundary(map_raw, tolerance = input$simplify_tol)
    disease <- clean_disease(disease_raw)
    pop <- clean_population(pop_raw)

    # If disease data has no district column, assign district using spatial join to boundary.
    pts <- st_as_sf(disease, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    pts_tag <- suppressWarnings(st_join(pts, boundary[, c("MAP_SUBDIST", "DISTRICT_STD")], join = st_intersects, left = TRUE))
    disease$district <- ifelse(is.na(disease$district), pts_tag$DISTRICT_STD, disease$district)
    disease$subdistrict <- ifelse(!is.na(pts_tag$MAP_SUBDIST), pts_tag$MAP_SUBDIST, disease$subdistrict)

    list(disease = disease, population = pop, boundary = boundary)
  })

  output$analysis_status <- renderText({
    if (input$analyse == 0) return("Upload all three files, then click ANALYSE.")
    x <- analysed()
    paste0("Analysis ready. Cases loaded: ", nrow(x$disease),
           "; population rows: ", nrow(x$population),
           "; boundary areas: ", nrow(x$boundary), ".")
  })

  years_all <- reactive({ req(analysed()); sort(unique(analysed()$disease$year)) })
  districts_all <- reactive({
    req(analysed())
    d <- sort(unique(na.omit(c(analysed()$boundary$DISTRICT_STD, analysed()$disease$district))))
    c("All districts", d)
  })

  year_choices <- function(id, label = "Year") {
    selectInput(id, label, choices = c("All years", years_all()), selected = "All years", multiple = TRUE)
  }
  district_choices <- function(id, label = "District") {
    selectInput(id, label, choices = districts_all(), selected = "All districts")
  }

  output$filter_year_check <- renderUI(year_choices("year_check"))
  output$filter_district_check <- renderUI(district_choices("district_check"))
  output$filter_year_desc <- renderUI(year_choices("year_desc"))
  output$filter_district_desc <- renderUI(district_choices("district_desc"))
  output$filter_year_point <- renderUI(year_choices("year_point"))
  output$filter_district_point <- renderUI(district_choices("district_point"))
  output$filter_year_inc <- renderUI(year_choices("year_inc"))
  output$filter_district_inc <- renderUI(district_choices("district_inc"))
  output$filter_year_report <- renderUI(year_choices("year_report"))
  output$filter_district_report <- renderUI(district_choices("district_report"))

  filter_df <- function(year_input, district_input) {
    req(analysed())
    df <- analysed()$disease
    if (!is.null(year_input) && !("All years" %in% year_input)) df <- df |> filter(year %in% as.integer(year_input))
    if (!is.null(district_input) && district_input != "All districts") df <- df |> filter(district == district_input)
    df
  }

  filter_boundary <- function(district_input) {
    req(analysed())
    b <- analysed()$boundary
    if (!is.null(district_input) && district_input != "All districts") b <- b |> filter(DISTRICT_STD == district_input)
    b
  }

  filtered_check <- reactive(filter_df(input$year_check, input$district_check))
  filtered_desc <- reactive(filter_df(input$year_desc, input$district_desc))
  filtered_point <- reactive(filter_df(input$year_point, input$district_point))
  filtered_inc_cases <- reactive(filter_df(input$year_inc, input$district_inc))
  filtered_report <- reactive(filter_df(input$year_report, input$district_report))

  output$tbl_check <- renderDT({
    req(filtered_check())
    datatable(head(filtered_check(), 10), options = list(pageLength = 10, scrollX = TRUE))
  })

  desc_table <- reactive({ req(filtered_desc()); make_descriptive_table(filtered_desc()) })
  output$tbl_desc <- renderDT({ req(desc_table()); datatable(desc_table(), options = list(pageLength = 10, scrollX = TRUE)) })

  output$plot_week <- renderPlot({
    df <- filtered_desc()
    validate(need(nrow(df) > 0, "No data after filtering."))
    validate(need(!all(is.na(df$week)), "No valid epidemiological week column detected."))
    trend <- df |>
      filter(!is.na(week), week >= 1, week <= 53) |>
      count(year, week, name = "cases")
    ggplot(trend, aes(x = week, y = cases, group = year, colour = factor(year))) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      scale_x_continuous(breaks = seq(1, 53, by = 4)) +
      labs(x = "Epidemiological week", y = "Cases", colour = "Year", title = paste(input$disease_name, "weekly trend")) +
      theme_minimal(base_size = 13)
  })

  output$map_point <- renderLeaflet({
    df <- filtered_point()
    b <- filter_boundary(input$district_point)
    validate(need(nrow(df) > 0, "No cases after filtering."))
    pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

    if (isTRUE(input$clip_point) && nrow(b) > 0) {
      inside <- lengths(st_intersects(pts, b)) > 0
      pts <- pts[inside, ]
    }
    validate(need(nrow(pts) > 0, "No points remain after clipping."))

    pal <- colorFactor("Set2", domain = pts$year)
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, group = "Default") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolygons(data = b, fill = FALSE, color = "#444", weight = 1, group = "Boundary", label = ~MAP_SUBDIST) |>
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, stroke = FALSE, fillOpacity = 0.75, color = ~pal(year),
        popup = ~paste0("Year: ", year, "<br>Week: ", week, "<br>District: ", district, "<br>Subdistrict: ", subdistrict),
        group = "Cases"
      ) |>
      addLegend("bottomright", pal = pal, values = pts$year, title = "Year") |>
      addLayersControl(baseGroups = c("Default", "Satellite"), overlayGroups = c("Boundary", "Cases"), options = layersControlOptions(collapsed = FALSE))
  })

  incidence_data <- reactive({
    req(analysed())
    make_incidence(filtered_inc_cases(), analysed()$population, filter_boundary(input$district_inc))
  })

  output$map_inc <- renderLeaflet({
    req(incidence_data())
    b <- filter_boundary(input$district_inc)
    inc <- incidence_data()
    mapdat <- b |> left_join(inc, by = c("MAP_SUBDIST" = "subdistrict", "DISTRICT_STD"))
    validate(need(any(!is.na(mapdat$incidence_per_1000)), "Incidence could not be calculated. Check population matching."))
    pal <- colorBin("YlOrRd", domain = mapdat$incidence_per_1000, bins = 5, na.color = "#dddddd")
    leaflet(mapdat) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Default") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolygons(
        fillColor = ~pal(incidence_per_1000), fillOpacity = 0.75,
        color = "#555", weight = 1,
        label = ~paste0(MAP_SUBDIST, ": ", round(incidence_per_1000, 2), " per 1,000"),
        popup = ~paste0("Subdistrict: ", MAP_SUBDIST, "<br>Cases: ", cases, "<br>Population: ", population, "<br>Incidence: ", round(incidence_per_1000, 2), " per 1,000")
      ) |>
      addLegend("bottomright", pal = pal, values = ~incidence_per_1000, title = "Incidence per 1,000") |>
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE))
  })

  output$tbl_inc <- renderDT({
    datatable(
      incidence_data() |>
        select(subdistrict, DISTRICT_STD, year, cases, population, incidence_per_1000) |>
        arrange(desc(incidence_per_1000)),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  report_desc <- reactive({ req(filtered_report()); make_descriptive_table(filtered_report()) })
  report_inc <- reactive({ req(analysed()); make_incidence(filtered_report(), analysed()$population, filter_boundary(input$district_report)) })
  report_paragraphs <- reactive({
    report_text(filtered_report(), report_desc(), report_inc(), input$district_report, input$year_report)
  })

  output$summary_report <- renderUI({
    req(report_paragraphs())
    tagList(
      lapply(report_paragraphs(), function(x) p(x)),
      p(strong("Suggested reporting interpretation: "), "Areas with high case counts and high incidence should be prioritised for field verification, health education, environmental investigation, and targeted prevention activities.")
    )
  })
  output$tbl_report_desc <- renderDT({ req(report_desc()); datatable(report_desc(), options = list(pageLength = 10, scrollX = TRUE)) })

  # Summary report maps
  # These maps use the same filters as the Summary Report tab.
  # If multiple years are selected, the point map displays all selected-year cases,
  # while the incidence map summarises the selected period as cases per 1,000 population-years.
  report_inc_map <- reactive({
    inc <- report_inc()
    inc |>
      filter(!is.na(subdistrict)) |>
      group_by(subdistrict, DISTRICT_STD) |>
      summarise(
        cases = sum(cases, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        incidence_per_1000 = ifelse(population > 0, cases / population * 1000, NA_real_),
        .groups = "drop"
      )
  })

  report_boundary_map <- reactive({
    b <- filter_boundary(input$district_report)
    b |> left_join(report_inc_map(), by = c("MAP_SUBDIST" = "subdistrict", "DISTRICT_STD"))
  })

  output$map_report_point <- renderLeaflet({
    df <- filtered_report()
    b <- filter_boundary(input$district_report)
    validate(need(nrow(df) > 0, "No cases after filtering."))

    pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    if (nrow(b) > 0) {
      inside <- lengths(st_intersects(pts, b)) > 0
      pts <- pts[inside, ]
    }
    validate(need(nrow(pts) > 0, "No point map available after district clipping."))

    pal <- colorFactor("Set2", domain = pts$year)
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, group = "Default") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolygons(data = b, fill = FALSE, color = "#444", weight = 1, group = "Boundary", label = ~MAP_SUBDIST) |>
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, stroke = FALSE, fillOpacity = 0.75, color = ~pal(year),
        popup = ~paste0("Year: ", year, "<br>Week: ", week, "<br>District: ", district, "<br>Subdistrict: ", subdistrict),
        group = "Cases"
      ) |>
      addLegend("bottomright", pal = pal, values = pts$year, title = "Year") |>
      addLayersControl(baseGroups = c("Default", "Satellite"), overlayGroups = c("Boundary", "Cases"), options = layersControlOptions(collapsed = TRUE))
  })

  output$map_report_inc <- renderLeaflet({
    mapdat <- report_boundary_map()
    validate(need(any(!is.na(mapdat$incidence_per_1000)), "Incidence could not be calculated. Check population matching."))

    pal <- colorBin("YlOrRd", domain = mapdat$incidence_per_1000, bins = 5, na.color = "#dddddd")
    leaflet(mapdat) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Default") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolygons(
        fillColor = ~pal(incidence_per_1000), fillOpacity = 0.75,
        color = "#555", weight = 1,
        label = ~paste0(MAP_SUBDIST, ": ", round(incidence_per_1000, 2), " per 1,000"),
        popup = ~paste0("Subdistrict: ", MAP_SUBDIST, "<br>Cases: ", cases, "<br>Population: ", population, "<br>Incidence: ", round(incidence_per_1000, 2), " per 1,000")
      ) |>
      addLegend("bottomright", pal = pal, values = ~incidence_per_1000, title = "Incidence per 1,000") |>
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = TRUE))
  })

  output$download_doc <- downloadHandler(
    filename = function() paste0("KSK_report_", input$disease_name, "_", Sys.Date(), ".doc"),
    content = function(file) {
      paragraphs <- report_paragraphs()
      desc_html <- html_table(report_desc())

      top_inc <- report_inc() |>
        filter(!is.na(incidence_per_1000)) |>
        arrange(desc(incidence_per_1000)) |>
        select(subdistrict, DISTRICT_STD, year, cases, population, incidence_per_1000) |>
        head(10)

      inc_html <- html_table(top_inc)

      html <- paste0(
        "<html><head><meta charset='UTF-8'>",
        "<style>",
        "body{font-family:Arial, sans-serif;font-size:12pt;}",
        "h1,h2{color:#343a40;}",
        "table{border-collapse:collapse;width:100%;margin-bottom:16px;}",
        "th,td{border:1px solid #999;padding:6px;text-align:left;}",
        "th{background:#eeeeee;}",
        "</style></head><body>",
        "<h1>Kelantan Spatial Kit Report: ", htmltools::htmlEscape(input$disease_name), "</h1>",
        "<p><b>Generated on:</b> ", Sys.Date(), "</p>",
        "<h2>Summary</h2>",
        paste0("<p>", htmltools::htmlEscape(paragraphs), "</p>", collapse = ""),
        "<p><b>Suggested reporting interpretation:</b> Areas with high case counts and high incidence should be prioritised for field verification, health education, environmental investigation, and targeted prevention activities.</p>",
        "<h2>Descriptive analysis by year</h2>",
        desc_html,
        "<h2>Highest incidence areas</h2>",
        inc_html,
        "<p><i>Note: Interactive point and incidence maps are available in the KSK Summary Report tab. This deployment-safe Word-compatible file contains text and tables only.</i></p>",
        "</body></html>"
      )

      writeLines(html, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)

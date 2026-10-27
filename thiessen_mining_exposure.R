# =====================================================================
# Mining area around polling stations using Thiessen polygons
# =====================================================================
# Project: Electoral consequences of mining, Colombia
# Author:  Maria Jose Aranguren Echeverri
#
# Builds the mining exposure variables used in the regressions. Each
# polling station gets a Thiessen (Voronoi) polygon clipped to its
# municipality, and the script measures hectares of mining inside it,
# split into legal and illegal according to valid mining titles.
#
# Inputs
#   puestos_reg_final_match_revisado.dta   polling stations, corrected coords
#   Municipios.shp                         municipal boundaries
#   Grillas_EVOA_*.shp                     UNODC mining grids, 2016 and 2019
#   data_for_reg_comimogrids.dta           CoMiMo / Dynamic World grids
#   Titulos_vigentes_25112021.shp          valid mining titles
#
# Outputs
#   puestos_reg_final_match_update.dta        analysis dataset
#   puestos_sin_thiessen_diagnostico.xlsx     stations without a polygon
#   area_thiessen_ha and {unodc,comimo}_{total,legal,illegal}_ha_{2016,2019}
#
# Notes
#   Rewritten in R from an earlier Python notebook that had computation
#   errors (it processed only 2016, filtered the overlay by municipality
#   code, and built grids from centroids).
#   Stations on the municipal boundary are kept with an intersection test
#   plus a tolerance buffer instead of a strict within test. Stations just
#   outside are snapped to the boundary if within SNAP_THRESHOLD_M.
#   Mining area is weighted by the share of each grid cell inside the
#   polygon, so cells crossing several polygons are not double counted.
#   This version uses terra instead of sf. API mapping:
#     st_read -> vect, st_transform -> project, st_union -> aggregate,
#     st_buffer -> buffer, st_intersects -> relate(..., "intersects"),
#     st_voronoi -> voronoi(pts, bnd), st_intersection -> intersect,
#     st_area -> expanse, st_coordinates -> crds, st_crop -> crop
# =====================================================================

rm(list = ls())

# 1) PATHS (set mainpath before running)
mainpath <- "path/to/project"

setwd(mainpath)

# 2) PACKAGES
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(terra, tidyverse, haven, magrittr, writexl)

# 3) INPUT FILES
puestos_reg_path <- file.path(mainpath, "CreatedData/Temporary/puestos_reg_final_match_revisado.dta")
muni_path        <- file.path(mainpath, "RawData/Municipios/Municipios.shp")
unodc_path       <- file.path(mainpath, "RawData/UNODC/Grillas_EVOA_2014_2016_2018_2019.shp")
comimo_dta_path  <- file.path(mainpath, "RawData/data_for_reg_comimogrids.dta")
titulos_path     <- file.path(mainpath, "RawData/titles/Titulos_vigentes_25112021.shp")

# Parameters
SNAP_THRESHOLD_M   <- 2500   # max distance to snap a station to the municipal boundary (meters)
BUFFER_TOLERANCE_M <- 10     # tolerance buffer for the intersection test (meters)

# =====================================================================
# 4) LOAD DATA
# =====================================================================

cat("Loading input data...\n")

# Municipalities
muni_all <- project(vect(muni_path), "EPSG:32618")
muni_all$codmpio <- as.numeric(muni_all$MPIO_CCDGO)

# Polling stations: read as table, then convert to points
polls_raw <- read_dta(puestos_reg_path) %>%
  mutate(
    codmpio = as.numeric(codmpio),
    id_2015 = as.character(id_2015),
    id_2019 = as.character(id_2019)
  )

lat_col <- if (all(c("lat", "lon") %in% names(polls_raw))) "lat" else "Latitud"
lon_col <- if (all(c("lat", "lon") %in% names(polls_raw))) "lon" else "Longitud"

polls_raw <- polls_raw %>%
  mutate(
    lat_coord = as.numeric(.data[[lat_col]]),
    lon_coord = as.numeric(.data[[lon_col]])
  )

n_sin_coord <- sum(is.na(polls_raw$lat_coord) | is.na(polls_raw$lon_coord))
cat("  Polling stations in file:", nrow(polls_raw), "\n")
cat("  Dropped, missing coordinates:", n_sin_coord, "\n")

polls_coord <- polls_raw %>% filter(!is.na(lat_coord), !is.na(lon_coord))
polls_all   <- project(
  vect(polls_coord, geom = c("lon_coord", "lat_coord"), crs = "EPSG:4326"),
  "EPSG:32618"
)
# vect() drops geometry columns, so re-attach coordinates as attributes
polls_all$lon_coord <- polls_coord$lon_coord
polls_all$lat_coord <- polls_coord$lat_coord

cat("  Stations with coordinates:", nrow(polls_all), "\n")
cat("  Distinct municipalities:", length(unique(polls_all$codmpio)), "\n")

# UNODC: one layer per year, then stack
unodc_raw <- project(vect(unodc_path), "EPSG:32618")
unodc_df  <- as.data.frame(unodc_raw)

n_rows <- nrow(unodc_raw)

unodc_df_16 <- data.frame(
  grid_id_unodc = seq_len(n_rows),
  period        = "2016",
  illegal_ha    = replace_na(as.numeric(unodc_df$EVOA2016), 0),
  stringsAsFactors = FALSE
)
unodc_2016 <- unodc_raw
values(unodc_2016) <- unodc_df_16

unodc_df_19 <- data.frame(
  grid_id_unodc = seq_len(n_rows),
  period        = "2019",
  illegal_ha    = replace_na(as.numeric(unodc_df$EVOA2019), 0),
  stringsAsFactors = FALSE
)
unodc_2019 <- unodc_raw
values(unodc_2019) <- unodc_df_19

unodc_all <- rbind(unodc_2016, unodc_2019)
unodc_all$area_grid_ha <- expanse(unodc_all) * 1e-4

cat("  UNODC filas totales:", nrow(unodc_all), "| CRS:", crs(unodc_all, describe = TRUE)$code, "\n")
cat("  EVOA2016 grids > 0:",
    sum(as.data.frame(unodc_all)$illegal_ha[as.data.frame(unodc_all)$period == "2016"] > 0, na.rm = TRUE),
    "| EVOA2019 grids > 0:",
    sum(as.data.frame(unodc_all)$illegal_ha[as.data.frame(unodc_all)$period == "2019"] > 0, na.rm = TRUE), "\n")

# CoMiMo: rebuild square cells from id_grid as WKT
cat("Cargando grillas COMIMO (desde DTA)...\n")

COMIMO_X_MIN   <- -640851
COMIMO_Y_MIN   <- -513198
COMIMO_GRID_SZ <- 540

comimo_raw <- read_dta(comimo_dta_path) %>%
  filter(year %in% c(2016, 2019)) %>%
  select(id_grid, year, Area_total_ha)

comimo_16 <- comimo_raw %>% filter(year == 2016) %>%
  transmute(id_grid, comimo_ha_2016 = Area_total_ha)
comimo_19 <- comimo_raw %>% filter(year == 2019) %>%
  transmute(id_grid, comimo_ha_2019 = Area_total_ha)

comimo_data <- full_join(comimo_16, comimo_19, by = "id_grid") %>%
  replace_na(list(comimo_ha_2016 = 0, comimo_ha_2019 = 0)) %>%
  mutate(
    id_comimo = as.character(id_grid),
    x_center  = COMIMO_GRID_SZ * (id_grid %/% 1e4 - 1000) + COMIMO_X_MIN + COMIMO_GRID_SZ / 2,
    y_center  = COMIMO_GRID_SZ * ((id_grid %% 1e4 - 1000) - 1000) + COMIMO_Y_MIN + COMIMO_GRID_SZ / 2
  )

# 540x540m square polygon per grid cell
half <- COMIMO_GRID_SZ / 2
comimo_data$geometry <- sprintf(
  "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
  comimo_data$x_center - half, comimo_data$y_center - half,
  comimo_data$x_center + half, comimo_data$y_center - half,
  comimo_data$x_center + half, comimo_data$y_center + half,
  comimo_data$x_center - half, comimo_data$y_center + half,
  comimo_data$x_center - half, comimo_data$y_center - half
)

comimo_polys <- vect(comimo_data, geom = "geometry", crs = "EPSG:32618")
comimo_polys$area_grid_ha <- expanse(comimo_polys) * 1e-4

cat("  Grillas COMIMO:", nrow(comimo_polys), "\n")
comimo_df_tmp <- as.data.frame(comimo_polys)
cat("  With mining 2016:", sum(comimo_df_tmp$comimo_ha_2016 > 0),
    "| mean (where mining > 0):",
    round(mean(comimo_df_tmp$comimo_ha_2016[comimo_df_tmp$comimo_ha_2016 > 0]), 3), "ha\n")
cat("  With mining 2019:", sum(comimo_df_tmp$comimo_ha_2019 > 0),
    "| mean (where mining > 0):",
    round(mean(comimo_df_tmp$comimo_ha_2019[comimo_df_tmp$comimo_ha_2019 > 0]), 3), "ha\n")
rm(comimo_df_tmp)

# Mining titles, used to split legal from illegal
cat("Loading mining titles...\n")

titulos_all <- project(vect(titulos_path), "EPSG:32618")
titulos_df  <- as.data.frame(titulos_all)
# Compute year columns on the data frame, then attach
titulos_df$year_inicio  <- as.integer(substr(titulos_df$FECHA_DE_I, 1, 4))
titulos_df$year_term    <- as.integer(substr(titulos_df$FECHA_TERM, 1, 4))
titulos_all$year_inicio <- titulos_df$year_inicio
titulos_all$year_term   <- titulos_df$year_term

idx_t2016 <- !is.na(titulos_df$year_inicio) & titulos_df$year_inicio <= 2016 &
             !is.na(titulos_df$year_term)   & titulos_df$year_term   >= 2016
idx_t2019 <- !is.na(titulos_df$year_inicio) & titulos_df$year_inicio <= 2019 &
             !is.na(titulos_df$year_term)   & titulos_df$year_term   >= 2019

titulos_2016 <- aggregate(titulos_all[idx_t2016, ])
titulos_2019 <- aggregate(titulos_all[idx_t2019, ])

cat("  Titles valid in 2016:", sum(idx_t2016),
    "| 2019:", sum(idx_t2019), "\n")


# =====================================================================
# 5) BUILD THIESSEN POLYGONS, MUNICIPALITY BY MUNICIPALITY
# =====================================================================
# voronoi() preserves point order, so no nearest() step is needed
# =====================================================================

polls_all_df       <- as.data.frame(polls_all)
municipios_con_puestos <- unique(polls_all_df$codmpio)
n_muni                 <- length(municipios_con_puestos)
thiessen_nacional_list <- list()
n_fallback             <- 0
n_snapped_in_loop      <- 0

cat("Building Thiessen polygons for", n_muni, "municipalities...\n")

for (i in seq_along(municipios_con_puestos)) {
  m <- municipios_con_puestos[i]
  cat(paste0("\r  Municipality ", i, "/", n_muni, " (code: ", m, ")        "))
  utils::flush.console()

  muni_vect  <- muni_all[muni_all$codmpio == m, ]
  polls_vect <- polls_all[polls_all$codmpio == m, ]

  if (nrow(muni_vect) == 0 || nrow(polls_vect) == 0) next

  # Dissolve municipality and repair topology
  muni_union <- tryCatch(
    makeValid(aggregate(muni_vect)),
    error = function(e) aggregate(muni_vect)
  )

  # ---- Step 1: intersection test with tolerance buffer ----
  muni_buffered <- buffer(muni_union, width = BUFFER_TOLERANCE_M)
  inside_mat    <- relate(polls_vect, muni_buffered, "intersects")
  inside_idx    <- apply(inside_mat, 1, any)

  # ---- Step 2: snap stations falling just outside the boundary ----
  outside_idx <- which(!inside_idx)
  if (length(outside_idx) > 0) {
    # geom() is more reliable than crds() for SpatVector
    polls_geom <- geom(polls_vect)
    polls_crds_x <- polls_geom[, "x"]
    polls_crds_y <- polls_geom[, "y"]

    for (j in outside_idx) {
      pt_j   <- polls_vect[j, ]
      dist_j <- as.numeric(distance(pt_j, muni_union))

      if (dist_j <= SNAP_THRESHOLD_M) {
        pt_snapped        <- snap(pt_j, muni_union, tolerance = SNAP_THRESHOLD_M * 2)
        snp_geom          <- geom(pt_snapped)
        polls_crds_x[j]  <- snp_geom[1, "x"]
        polls_crds_y[j]  <- snp_geom[1, "y"]
        inside_idx[j]    <- TRUE
        n_snapped_in_loop <- n_snapped_in_loop + 1
      }
    }

    # Rebuild with snapped coordinates
    pv_df        <- as.data.frame(polls_vect)
    pv_df$x_snp <- polls_crds_x
    pv_df$y_snp <- polls_crds_y
    polls_vect   <- vect(pv_df, geom = c("x_snp", "y_snp"), crs = "EPSG:32618")
  }

  polls_inside <- polls_vect[inside_idx, ]
  if (nrow(polls_inside) == 0) next

  # ---- Deduplicate exact coordinates ----
  # geom() is more reliable than crds() for SpatVector
  inside_geom <- geom(polls_inside)
  xy_str      <- paste(round(inside_geom[, "x"], 2), round(inside_geom[, "y"], 2), sep = "_")
  dup_mask    <- duplicated(xy_str)
  polls_dedup <- polls_inside[!dup_mask, ]

  # ---- Step 3: Voronoi, with fallback ----
  thiessen_muni <- tryCatch({

    if (nrow(polls_dedup) == 1) {
      # Single station: assign the whole municipality
      result_vect <- muni_union[rep(1L, nrow(polls_inside)), ]
      values(result_vect) <- as.data.frame(polls_inside)
      result_vect

    } else {
      # voronoi(pts, bnd) clips to the bbox only, so intersect with the municipality
      vor_polys <- voronoi(polls_dedup, bnd = muni_union)
      vor_polys <- tryCatch(
        terra::intersect(vor_polys, muni_union),
        error = function(e) vor_polys   # si falla el clip, conservar sin recortar
      )

      # Match each station, duplicates included, to its dedup point by nearest coords
      geom_inside <- geom(polls_inside)
      geom_dedup  <- geom(polls_dedup)
      crds_inside <- cbind(geom_inside[, "x"], geom_inside[, "y"])
      crds_dedup  <- cbind(geom_dedup[, "x"],  geom_dedup[, "y"])

      match_idx <- sapply(seq_len(nrow(polls_inside)), function(k) {
        dists <- sqrt(
          (crds_dedup[, 1] - crds_inside[k, 1])^2 +
          (crds_dedup[, 2] - crds_inside[k, 2])^2
        )
        which.min(dists)
      })

      # Keep the Voronoi geometry of the dedup point and the station attributes
      result_vect <- vor_polys[match_idx, ]
      values(result_vect) <- as.data.frame(polls_inside)
      result_vect
    }

  }, error = function(e) {
    n_fallback <<- n_fallback + 1
    message("\n  [FALLBACK] Voronoi failed in municipality ", m, ": ", conditionMessage(e),
            " — asignando municipio completo a cada puesto interior")
    result_vect <- muni_union[rep(1L, nrow(polls_inside)), ]
    values(result_vect) <- as.data.frame(polls_inside)
    result_vect
  })

  thiessen_muni$codmpio_final <- m
  thiessen_nacional_list[[as.character(m)]] <- thiessen_muni
}

cat("\n\nThiessen construction summary:\n")
cat("  Municipios con fallback Voronoi:", n_fallback, "\n")
cat("  Puestos snapped al borde (v3):", n_snapped_in_loop, "\n")

cat("Stacking polygons into one national layer...\n")
# Reduce(rbind, ...) avoids the deparse.level argument issue of do.call(rbind, ...)
thiessen_nacional_vect <- Reduce(rbind, thiessen_nacional_list)
thiessen_nacional_vect$area_thiessen_ha <- expanse(thiessen_nacional_vect) * 1e-4

n_puestos <- nrow(thiessen_nacional_vect)
cat("  Total Thiessen polygons:", n_puestos, "\n")

# =====================================================================
# 6) MAIN LOOP, MINING AREA INSIDE EACH THIESSEN POLYGON
# =====================================================================

# Overlap check on a sample of polygons
unodc_all_df  <- as.data.frame(unodc_all)
unodc16_pos   <- unodc_all[unodc_all_df$period == "2016" & unodc_all_df$illegal_ha > 0, ]
unodc19_pos   <- unodc_all[unodc_all_df$period == "2019" & unodc_all_df$illegal_ha > 0, ]
th_sample     <- thiessen_nacional_vect[seq_len(min(50L, n_puestos)), ]

cat("Overlap check (first 50 polygons):\n")
cat("  UNODC 2016 grids with value > 0:", nrow(unodc16_pos), "\n")
cat("  UNODC 2019 grids with value > 0:", nrow(unodc19_pos), "\n")
n_overlap16 <- sum(apply(relate(th_sample, unodc16_pos, "intersects"), 1, any))
n_overlap19 <- sum(apply(relate(th_sample, unodc19_pos, "intersects"), 1, any))
cat("  Polygons intersecting UNODC 2016 > 0:", n_overlap16, "of", nrow(th_sample), "\n")
cat("  Polygons intersecting UNODC 2019 > 0:", n_overlap19, "of", nrow(th_sample), "\n")

# ---- Helper: split mining area into legal and illegal ----
# Takes grid cells already intersected with a polygon, plus the titles layer
calc_legal_illegal <- function(u_vect, titulos_vect, col_ha, grid_id) {
  u_df     <- as.data.frame(u_vect)
  total_ha <- sum(u_df[[col_ha]] * u_df$prop, na.rm = TRUE)

  u_legal <- tryCatch(
    terra::intersect(u_vect, titulos_vect),
    error = function(e) NULL
  )

  if (is.null(u_legal) || nrow(u_legal) == 0) {
    return(list(total = total_ha, legal = 0, illegal = total_ha))
  }

  u_legal_df               <- as.data.frame(u_legal)
  u_legal_df$area_legal_ha <- expanse(u_legal) * 1e-4

  legal_by_grid <- u_legal_df %>%
    group_by(.data[[grid_id]]) %>%
    summarise(area_legal_ha = sum(area_legal_ha, na.rm = TRUE), .groups = "drop")

  u_aug <- u_df %>%
    left_join(legal_by_grid, by = grid_id) %>%
    mutate(
      area_legal_ha = replace_na(area_legal_ha, 0),
      prop_legal    = pmin(area_legal_ha / area_grid_ha, prop),
      prop_illegal  = prop - prop_legal,
      w_legal       = .data[[col_ha]] * prop_legal,
      w_illegal     = .data[[col_ha]] * prop_illegal
    )

  list(
    total   = total_ha,
    legal   = sum(u_aug$w_legal,   na.rm = TRUE),
    illegal = sum(u_aug$w_illegal, na.rm = TRUE)
  )
}

# ---- Group polygons by municipality and pre-filter grids ----
thiessen_vect_df   <- as.data.frame(thiessen_nacional_vect)
thiessen_by_muni   <- split(seq_len(n_puestos), thiessen_vect_df$codmpio_final)

unodc_results  <- vector("list", n_puestos)
comimo_results <- vector("list", n_puestos)

cat("Computing mining area per polling station (UNODC and CoMiMo)...\n")

muni_names <- names(thiessen_by_muni)
for (mi in seq_along(muni_names)) {
  m     <- muni_names[mi]
  idx_m <- thiessen_by_muni[[m]]

  if (mi %% 50 == 0) {
    cat(paste0("\r  Municipio ", mi, "/", length(muni_names),
               " (puestos procesados: ",
               sum(!sapply(unodc_results, is.null)), "/", n_puestos, ")        "))
    utils::flush.console()
  }

  # Pre-filter grids to municipality extent + 2000m
  e_m     <- ext(thiessen_nacional_vect[idx_m, ])
  e_m_buf <- ext(
    e_m$xmin - 2000, e_m$xmax + 2000,
    e_m$ymin - 2000, e_m$ymax + 2000
  )

  unodc_m  <- tryCatch(crop(unodc_all,   e_m_buf), error = function(e) unodc_all[0, ])
  comimo_m <- tryCatch(crop(comimo_polys, e_m_buf), error = function(e) comimo_polys[0, ])

  for (i in idx_m) {
    th <- thiessen_nacional_vect[i, ]

    # --- UNODC ---
    empty_unodc <- tibble(
      unodc_total_ha_2016 = 0, unodc_legal_ha_2016 = 0, unodc_illegal_ha_2016 = 0,
      unodc_total_ha_2019 = 0, unodc_legal_ha_2019 = 0, unodc_illegal_ha_2019 = 0
    )

    u_vect <- if (nrow(unodc_m) > 0) {
      tryCatch(terra::intersect(th, unodc_m), error = function(e) unodc_m[0, ])
    } else {
      unodc_m[0, ]
    }

    if (nrow(u_vect) == 0) {
      unodc_results[[i]] <- empty_unodc
    } else {
      u_df          <- as.data.frame(u_vect)
      u_df$area_int_ha <- expanse(u_vect) * 1e-4
      u_df$prop     <- u_df$area_int_ha / u_df$area_grid_ha
      u_df$unodc_ha <- u_df$illegal_ha
      values(u_vect) <- u_df

      u2016 <- u_vect[as.data.frame(u_vect)$period == "2016", ]
      u2019 <- u_vect[as.data.frame(u_vect)$period == "2019", ]

      r2016 <- if (nrow(u2016) > 0) calc_legal_illegal(u2016, titulos_2016, "unodc_ha", "grid_id_unodc") else list(total=0, legal=0, illegal=0)
      r2019 <- if (nrow(u2019) > 0) calc_legal_illegal(u2019, titulos_2019, "unodc_ha", "grid_id_unodc") else list(total=0, legal=0, illegal=0)

      unodc_results[[i]] <- tibble(
        unodc_total_ha_2016   = r2016$total,
        unodc_legal_ha_2016   = r2016$legal,
        unodc_illegal_ha_2016 = r2016$illegal,
        unodc_total_ha_2019   = r2019$total,
        unodc_legal_ha_2019   = r2019$legal,
        unodc_illegal_ha_2019 = r2019$illegal
      )
    }

    # --- COMIMO ---
    empty_comimo <- tibble(
      comimo_total_ha_2016 = 0, comimo_legal_ha_2016 = 0, comimo_illegal_ha_2016 = 0,
      comimo_total_ha_2019 = 0, comimo_legal_ha_2019 = 0, comimo_illegal_ha_2019 = 0
    )

    uc_vect <- if (nrow(comimo_m) > 0) {
      tryCatch(terra::intersect(th, comimo_m), error = function(e) comimo_m[0, ])
    } else {
      comimo_m[0, ]
    }

    if (nrow(uc_vect) == 0) {
      comimo_results[[i]] <- empty_comimo
    } else {
      uc_df          <- as.data.frame(uc_vect)
      uc_df$area_int_ha <- expanse(uc_vect) * 1e-4
      uc_df$prop     <- uc_df$area_int_ha / uc_df$area_grid_ha
      values(uc_vect) <- uc_df

      rc2016 <- calc_legal_illegal(uc_vect, titulos_2016, "comimo_ha_2016", "id_comimo")
      rc2019 <- calc_legal_illegal(uc_vect, titulos_2019, "comimo_ha_2019", "id_comimo")

      comimo_results[[i]] <- tibble(
        comimo_total_ha_2016   = rc2016$total,
        comimo_legal_ha_2016   = rc2016$legal,
        comimo_illegal_ha_2016 = rc2016$illegal,
        comimo_total_ha_2019   = rc2019$total,
        comimo_legal_ha_2019   = rc2019$legal,
        comimo_illegal_ha_2019 = rc2019$illegal
      )
    }
  }
}

cat("\n\nMerging mining results...\n")

thiessen_attr <- as.data.frame(thiessen_nacional_vect) %>%
  select(codmpio = codmpio_final, id_2015, id_2019, area_thiessen_ha)

final_results_unodc <- bind_rows(unodc_results) %>%
  bind_cols(thiessen_attr)

final_results_comimo <- bind_rows(comimo_results) %>%
  bind_cols(thiessen_attr %>% select(codmpio, id_2015, id_2019))

cat("  UNODC listo.")
cat("\n  Total 2016 > 0:", sum(final_results_unodc$unodc_total_ha_2016   > 0))
cat("\n  Legal 2016 > 0:", sum(final_results_unodc$unodc_legal_ha_2016   > 0))
cat("\n  Ilegal 2016 > 0:", sum(final_results_unodc$unodc_illegal_ha_2016 > 0))
cat("\n  Total 2019 > 0:", sum(final_results_unodc$unodc_total_ha_2019   > 0))
cat("\n  Legal 2019 > 0:", sum(final_results_unodc$unodc_legal_ha_2019   > 0))
cat("\n  Ilegal 2019 > 0:", sum(final_results_unodc$unodc_illegal_ha_2019 > 0), "\n")

cat("\n  COMIMO listo.\n")
cat("  Total 2016 > 0:", sum(final_results_comimo$comimo_total_ha_2016   > 0), "\n")
cat("  Legal 2016 > 0:", sum(final_results_comimo$comimo_legal_ha_2016   > 0), "\n")
cat("  Ilegal 2016 > 0:", sum(final_results_comimo$comimo_illegal_ha_2016 > 0), "\n")
cat("  Total 2019 > 0:", sum(final_results_comimo$comimo_total_ha_2019   > 0), "\n")
cat("  Legal 2019 > 0:", sum(final_results_comimo$comimo_legal_ha_2019   > 0), "\n")
cat("  Ilegal 2019 > 0:", sum(final_results_comimo$comimo_illegal_ha_2019 > 0), "\n")

# =====================================================================
# 7) MERGE RESULTS AND SAVE
# =====================================================================

base_original <- read_dta(puestos_reg_path) %>%
  mutate(codmpio = as.numeric(codmpio), id_2019 = as.character(id_2019)) %>%
  select(-any_of("area_thiessen_ha"))  # drop stale column so fresh computation is used

base_consolidada <- base_original %>%
  left_join(final_results_unodc,  by = c("codmpio", "id_2019"), suffix = c("", "_unodc")) %>%
  left_join(final_results_comimo, by = c("codmpio", "id_2019"), suffix = c("", "_comimo"))

base_final <- base_consolidada %>%
  mutate(
    id_2019 = as.numeric(id_2019),
    id_2015 = as.numeric(id_2015)
  ) %>%
  select(-ends_with(".y")) %>%
  rename_with(~str_replace(., "\\.x$", ""), ends_with(".x")) %>%
  select(
    codmpio, id_2019, id_2015,
    area_thiessen_ha,
    unodc_total_ha_2016, unodc_legal_ha_2016, unodc_illegal_ha_2016,
    unodc_total_ha_2019, unodc_legal_ha_2019, unodc_illegal_ha_2019,
    comimo_total_ha_2016, comimo_legal_ha_2016, comimo_illegal_ha_2016,
    comimo_total_ha_2019, comimo_legal_ha_2019, comimo_illegal_ha_2019
  ) %>%
  mutate(
    # Illegal mining as share of Thiessen polygon area
    # = ha of illegal mining / ha of Thiessen polygon
    unodc_illegal_share_2016  = unodc_illegal_ha_2016  / area_thiessen_ha,
    unodc_illegal_share_2019  = unodc_illegal_ha_2019  / area_thiessen_ha,
    comimo_illegal_share_2016 = comimo_illegal_ha_2016 / area_thiessen_ha,
    comimo_illegal_share_2019 = comimo_illegal_ha_2019 / area_thiessen_ha
  )

output_path <- file.path(mainpath, "CreatedData/Temporary/puestos_reg_final_match_update.dta")
write_dta(base_final, output_path)
cat("Base guardada en:", output_path, "\n")

# =====================================================================
# 8) CONSISTENCY CHECKS
# =====================================================================
cat("\n=====================================================================\n")
cat("CONSISTENCY CHECKS\n")
cat("=====================================================================\n")

n_con_thiessen    <- sum(!is.na(base_final$area_thiessen_ha))
n_sin_thiessen    <- sum( is.na(base_final$area_thiessen_ha))
n_sin_coordenadas <- n_sin_coord

cat("Puestos totales en base original:         ", nrow(base_original), "\n")
cat("Puestos sin coordenadas (excluidos s4):   ", n_sin_coordenadas, "\n")
cat("Puestos con Thiessen calculado:           ", n_con_thiessen, "\n")
cat("Puestos con area_thiessen_ha == NA:       ", n_sin_thiessen, "\n")
cat("Puestos snapped al borde (v3):            ", n_snapped_in_loop, "\n")

if (n_sin_thiessen == n_sin_coordenadas) {
  cat("CHECK conteo: OK — todos los NAs corresponden a puestos sin coordenadas\n")
} else {
  cat("CHECK conteo: REVISAR —", n_sin_thiessen - n_sin_coordenadas,
      "puestos con coordenadas no tienen Thiessen calculado\n")
  base_final %>%
    filter(is.na(area_thiessen_ha)) %>%
    left_join(
      polls_raw %>%
        select(codmpio, id_2019, lat_coord, lon_coord) %>%
        mutate(id_2019 = as.numeric(id_2019)),
      by = c("codmpio", "id_2019")
    ) %>%
    filter(!is.na(lat_coord)) %>%
    select(codmpio, id_2019, lat_coord, lon_coord) %>%
    print()
}

cat("\n--- Thiessen area against municipal area ---\n")
muni_sub      <- muni_all[muni_all$codmpio %in% unique(thiessen_vect_df$codmpio_final), ]
area_muni_lookup <- data.frame(
  codmpio      = muni_sub$codmpio,
  area_muni_ha = expanse(muni_sub) * 1e-4
)

check_areas <- thiessen_vect_df %>%
  group_by(codmpio_final) %>%
  summarise(suma_thiessen_ha = sum(area_thiessen_ha, na.rm = TRUE), .groups = "drop") %>%
  left_join(area_muni_lookup, by = c("codmpio_final" = "codmpio")) %>%
  mutate(
    diff_ha  = abs(suma_thiessen_ha - area_muni_ha),
    diff_pct = diff_ha / area_muni_ha * 100
  )

cat("Municipalities where the two areas differ by more than 1%:\n")
check_problemas <- filter(check_areas, diff_pct > 1)
if (nrow(check_problemas) == 0) {
  cat("  None, geometry is consistent.\n")
} else {
  print(check_problemas)
}

check_descomp <- function(df, col_total, col_legal, col_illegal, fuente) {
  df %>%
    filter(!is.na(area_thiessen_ha)) %>%
    mutate(
      diff_2016 = abs(.data[[paste0(col_total, "_2016")]] -
                        (.data[[paste0(col_legal, "_2016")]] + .data[[paste0(col_illegal, "_2016")]])),
      diff_2019 = abs(.data[[paste0(col_total, "_2019")]] -
                        (.data[[paste0(col_legal, "_2019")]] + .data[[paste0(col_illegal, "_2019")]]))
    ) %>%
    {
      cat(paste0("\n--- Consistencia legal + ilegal = total (", fuente, ") ---\n"))
      cat("  Max diff 2016:", round(max(.$diff_2016, na.rm = TRUE), 6), "ha\n")
      cat("  Max diff 2019:", round(max(.$diff_2019, na.rm = TRUE), 6), "ha\n")
      if (max(.$diff_2016, na.rm = TRUE) < 0.001 & max(.$diff_2019, na.rm = TRUE) < 0.001) {
        cat("  CHECK: OK\n")
      } else {
        cat("  CHECK: REVISAR puestos con diferencia > 0.001 ha\n")
        filter(., diff_2016 > 0.001 | diff_2019 > 0.001) %>%
          select(codmpio, id_2019, diff_2016, diff_2019) %>% print()
      }
    }
}

check_descomp(base_final, "unodc_total_ha",  "unodc_legal_ha",  "unodc_illegal_ha",  "UNODC")
check_descomp(base_final, "comimo_total_ha", "comimo_legal_ha", "comimo_illegal_ha", "COMIMO")

# --- CoMiMo check ---
cat("\n--- CoMiMo check (source file) ---\n")
comimo_pol_df <- as.data.frame(comimo_polys)
cat("  Grillas CoMiMo cargadas:", nrow(comimo_polys), "\n")
cat("  Con comimo_ha > 0 en 2016:", sum(comimo_pol_df$comimo_ha_2016 > 0), "\n")
cat("  Con comimo_ha > 0 en 2019:", sum(comimo_pol_df$comimo_ha_2019 > 0), "\n")
cat("  Max comimo_ha 2016:", round(max(comimo_pol_df$comimo_ha_2016), 3), "ha\n")

cat("\n--- National summary of mined hectares (stations with a polygon) ---\n")
resumen <- base_final %>% filter(!is.na(area_thiessen_ha)) %>% summarise(
  unodc_total_2016   = sum(unodc_total_ha_2016,    na.rm = TRUE),
  unodc_legal_2016   = sum(unodc_legal_ha_2016,    na.rm = TRUE),
  unodc_ilegal_2016  = sum(unodc_illegal_ha_2016,  na.rm = TRUE),
  unodc_total_2019   = sum(unodc_total_ha_2019,    na.rm = TRUE),
  unodc_legal_2019   = sum(unodc_legal_ha_2019,    na.rm = TRUE),
  unodc_ilegal_2019  = sum(unodc_illegal_ha_2019,  na.rm = TRUE),
  comimo_total_2016  = sum(comimo_total_ha_2016,   na.rm = TRUE),
  comimo_legal_2016  = sum(comimo_legal_ha_2016,   na.rm = TRUE),
  comimo_ilegal_2016 = sum(comimo_illegal_ha_2016, na.rm = TRUE),
  comimo_total_2019  = sum(comimo_total_ha_2019,   na.rm = TRUE),
  comimo_legal_2019  = sum(comimo_legal_ha_2019,   na.rm = TRUE),
  comimo_ilegal_2019 = sum(comimo_illegal_ha_2019, na.rm = TRUE)
)
print(t(round(resumen, 2)))

# =====================================================================
# RECUENTO FINAL
# =====================================================================
cat("\n=====================================================================\n")
cat("RECUENTO FINAL v3\n")
cat("=====================================================================\n")

n_total        <- nrow(base_final)
n_con_thiessen <- sum(!is.na(base_final$area_thiessen_ha))
n_na_thiessen  <- sum( is.na(base_final$area_thiessen_ha))

cat("Puestos totales:                 ", n_total,         "\n")
cat("Con Thiessen calculado:          ", n_con_thiessen,  "\n")
cat("Sin Thiessen (NA):               ", n_na_thiessen,   "\n")
cat("Snapped al borde (v3):           ", n_snapped_in_loop, "\n")
cat("  → NAs restantes = puestos fuera > 1000m o sin municipio en shapefile\n\n")

if (n_na_thiessen > 0) {
  detalle_na <- base_final %>%
    filter(is.na(area_thiessen_ha)) %>%
    left_join(
      polls_raw %>%
        select(codmpio, id_2019, lat_coord, lon_coord) %>%
        mutate(id_2019 = as.numeric(id_2019)),
      by = c("codmpio", "id_2019")
    ) %>%
    select(codmpio, id_2019, lat_coord, lon_coord)
  cat("Detalle de puestos con NA:\n")
  print(detalle_na, n = Inf)
}

# =====================================================================
# DIAGNOSTICS: STATIONS WITHOUT A POLYGON
# =====================================================================

polls_all_geom <- geom(polls_all)
polls_all_df2  <- as.data.frame(polls_all)

# CASE 1: duplicated coordinates within the same municipality
coords_df <- polls_all_df2 %>%
  mutate(
    lon_r     = round(polls_all_geom[, "x"], 1),
    lat_r     = round(polls_all_geom[, "y"], 1),
    coord_str = paste(lon_r, lat_r, sep = "_")
  )

caso1_ids <- coords_df %>%
  group_by(codmpio, coord_str) %>%
  filter(n() > 1) %>%
  slice(-1) %>%
  ungroup() %>%
  mutate(caso = "Case 1 - duplicated coordinates within the municipality")

comparte_con <- coords_df %>%
  group_by(codmpio, coord_str) %>%
  filter(n() > 1) %>%
  summarise(ids_grupo = paste(id_2019, collapse = ", "), .groups = "drop")

caso1_final <- caso1_ids %>%
  left_join(comparte_con, by = c("codmpio", "coord_str")) %>%
  mutate(detalle = paste0("Comparte coordenada con id_2019: ", ids_grupo)) %>%
  select(codmpio, any_of(c("puesnombre_2019", "puesnombre_2015")),
         any_of(c("Latitud", "Longitud", "lat", "lon")),
         id_2019, any_of("fix"), id_2015, coord_str, caso, detalle)

cat("Caso 1 (coord duplicada dentro municipio):", nrow(caso1_final), "\n")

# CASE 2: coordinates outside the municipal polygon
cat("Verificando puestos fuera de su municipio...\n")

dentro_flag <- sapply(seq_len(nrow(polls_all)), function(ii) {
  p   <- polls_all[ii, ]
  cod <- polls_all_df2$codmpio[ii]
  mun <- muni_all[muni_all$codmpio == cod, ]
  if (nrow(mun) == 0) return(FALSE)
  as.vector(relate(p, aggregate(mun), "intersects"))[1]
})

caso2_ids <- polls_all_df2 %>%
  mutate(dentro = dentro_flag) %>%
  filter(!dentro) %>%
  mutate(
    caso    = "Case 2 - coordinates outside the municipal polygon",
    detalle = "Coord dentro de Colombia pero fuera del municipio asignado"
  ) %>%
  select(codmpio, id_2019, id_2015, caso, detalle)

cat("Caso 2 (fuera del municipio):", nrow(caso2_ids), "\n")

# CONSOLIDADO
coords_orig <- polls_raw %>%
  select(codmpio, id_2019, id_2015, any_of(c("puesnombre_2019", "puesnombre_2015")),
         lat_coord, lon_coord) %>%
  mutate(id_2019 = as.character(id_2019), id_2015 = as.character(id_2015))

todos <- bind_rows(
  caso1_final %>% mutate(id_2019 = as.character(id_2019), id_2015 = as.character(id_2015)),
  caso2_ids   %>% mutate(id_2019 = as.character(id_2019), id_2015 = as.character(id_2015))
) %>%
  left_join(coords_orig, by = c("codmpio", "id_2019", "id_2015")) %>%
  arrange(caso, codmpio, id_2019) %>%
  select(codmpio, id_2019, id_2015,
         any_of(c("puesnombre_2019", "puesnombre_2015")),
         lat_coord, lon_coord, caso, detalle)

en_ambos <- inner_join(
  caso1_final %>% select(codmpio, id_2019) %>% mutate(id_2019 = as.character(id_2019)),
  caso2_ids   %>% select(codmpio, id_2019) %>% mutate(id_2019 = as.character(id_2019)),
  by = c("codmpio", "id_2019")
)
cat("En ambos casos:", nrow(en_ambos), "\n")
cat("Distinct stations flagged:", n_distinct(paste(todos$codmpio, todos$id_2019)), "\n")

# EXPORTAR
output_diag <- file.path(mainpath, "CreatedData/Temporary/puestos_sin_thiessen_diagnostico.xlsx")
write_xlsx(
  list(
    "Todos"                    = todos,
    "Caso 1 — Coord duplicada" = todos %>% filter(str_starts(caso, "Caso 1")),
    "Caso 2 — Fuera municipio" = todos %>% filter(str_starts(caso, "Caso 2"))
  ),
  output_diag
)
cat("Guardado en:", output_diag, "\n")
cat("\n=== FIN Thiessen_polys_all_munis_v3.R ===\n")

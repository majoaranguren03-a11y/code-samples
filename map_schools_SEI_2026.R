# ══════════════════════════════════════════════════════════════
# School fieldwork map, SEI approval status 2026
# ──────────────────────────────────────────────────────────────
# Project : Discovering and Supporting Hidden Talents through Math
#           Olympiads in Colombia (randomized controlled trial,
#           Universidad del Rosario)
# Author  : Maria Jose Aranguren Echeverri
#
# Purpose
#   The 2026 survey round required visiting public schools across the
#   country, and each school first had to approve the visit through the
#   data collection firm (SEI). This script merges the study sample with
#   the firm's approval list and produces an interactive map that the
#   field team uses to see, school by school, which visits are cleared.
#
# What it does
#   1. Loads the study sample (Stata file with school coordinates,
#      treatment status and cluster assignment) and the list of
#      Universidad Antonio Narino campuses used as cluster centers.
#   2. Cleans identifiers, pads municipality codes to five digits and
#      maps the first two digits to department names.
#   3. Reads the approval list provided by SEI.
#   4. Matches schools to that list in three cascading steps, since the
#      two sources do not share a reliable key: first by ICFES school
#      code, then by normalized name plus municipality, and finally by
#      normalized name when it is unique in the list. The step that
#      produced each match is stored, so unmatched or weakly matched
#      schools can be reviewed by hand.
#   5. Classifies each school by approval status (approved, no reply,
#      rejected, unmatched).
#   6. Saves the merged dataset and prints match diagnostics.
#   7. Exports the data as JSON and writes a self-contained HTML map
#      (Leaflet) with filters by cluster and by department.
#
# Inputs
#   CreatedData/Temporary/MapsSurvey2026.dta   study sample
#   RawData/Sedes UAN.xlsx                     cluster campuses
#   ListadoColegiosSEI.xlsx                    approval list from SEI
#
# Outputs
#   MapsSurveySEI2026.dta   merged dataset with approval status
#   MapaSEI2026.html        interactive map, opens in any browser
#
# Note
#   The map interface is in Spanish because it is used by the field
#   team in Colombia.
# ══════════════════════════════════════════════════════════════

library(haven)
library(dplyr)
library(jsonlite)
library(openxlsx)
library(stringr)

# ── Paths (set project_root and sei_path before running) ──────
project_root <- "path/to/project"
outdir_sei   <- file.path(project_root, "CreatedData/Temporary/Maps/MapasFW2026")
sei_path     <- file.path(project_root, "RawData/ListadoColegiosSEI.xlsx")
main         <- project_root
dir.create(outdir_sei, recursive = TRUE, showWarnings = FALSE)

# ══════════════════════════════════════════════════════════════
# ── 1. Load data
# ══════════════════════════════════════════════════════════════
df        <- read_dta(file.path(main, "CreatedData/Temporary/MapsSurvey2026.dta"))
sedes_raw <- read.xlsx(file.path(main, "RawData/Sedes UAN.xlsx"))

# ══════════════════════════════════════════════════════════════
# ── 2. Clean identifiers, build coordinates and department names
# ══════════════════════════════════════════════════════════════
df <- df %>%
  mutate(
    codmpio_str = str_pad(as.integer(codmpio), 5, pad = "0"),
    sede_str    = str_pad(as.integer(codmpio_sede_uan), 5, pad = "0"),
    depto_cod   = substr(codmpio_str, 1, 2),
    lat         = as.numeric(latitud),
    lon         = as.numeric(longitud),
    treat       = as.integer(treat),
    clust_final = as.integer(clust_final),
    nopart24_25 = as.integer(nopart2425_clust)
  ) %>%
  filter(!is.na(lat), !is.na(lon))

sedes <- sedes_raw %>%
  mutate(cod = str_pad(gsub("[^0-9]", "", CODMPIO), 5, pad = "0")) %>%
  rename(sede_nombre = SEDE_UAN, sede_depto = DEPTO)

df <- df %>%
  left_join(sedes %>% select(cod, sede_nombre, sede_depto),
            by = c("sede_str" = "cod"))

depto_map <- c(
  "05" = "Antioquia",           "08" = "Atlántico",
  "11" = "Bogotá D.C.",    "13" = "Bolívar",
  "15" = "Boyacá",         "17" = "Caldas",
  "19" = "Cauca",               "20" = "Cesar",
  "25" = "Cundinamarca",        "27" = "Chocó",
  "41" = "Huila",               "44" = "La Guajira",
  "47" = "Magdalena",           "50" = "Meta",
  "52" = "Nariño",         "54" = "Norte de Santander",
  "63" = "Quindío",        "66" = "Risaralda",
  "68" = "Santander",           "73" = "Tolima",
  "76" = "Valle del Cauca"
)
df$departamento <- depto_map[df$depto_cod]

# ══════════════════════════════════════════════════════════════
# ── 3. Load the approval list provided by SEI
sei_raw <- read.xlsx(sei_path, startRow = 2, sep.names = " ")
# ══════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════
# ── 4. Match schools to the approval list
#      Three cascading rules: ICFES code, name + municipality, unique name
# ══════════════════════════════════════════════════════════════

# Normalizes school and municipality names so they can be compared
# across sources: upper case, accents removed, punctuation dropped.
normalize_txt <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

sei <- sei_raw %>%
  mutate(
    cod_icfes = as.numeric(cod_icfes),
    nom_norm  = normalize_txt(`nombre colegio`),
    muni_norm = normalize_txt(municipio)
  ) %>%
  filter(!is.na(cod_icfes))             

sei_by_cod <- sei %>%
  select(cod_icfes, reporteSEI_cod = reporteSEI)

sei_by_nombre_muni <- sei %>%
  select(nom_norm, muni_norm, reporteSEI_nm = reporteSEI)

# Only names that appear once in the list are safe to match on name alone
nombres_unicos <- sei %>% count(nom_norm) %>% filter(n == 1) %>% pull(nom_norm)
sei_by_nombre <- sei %>%
  filter(nom_norm %in% nombres_unicos) %>%
  distinct(nom_norm, reporteSEI_n = reporteSEI)

# ══════════════════════════════════════════════════════════════
# ── 5. Merge the study sample with the approval list
# ══════════════════════════════════════════════════════════════

df_sei <- df %>%
  mutate(
    cole_codigo_icfes = as.numeric(cole_codigo_icfes),
    nom_norm  = normalize_txt(cole_nombre_establecimiento),
    muni_norm = normalize_txt(municipio)
  ) %>%
  left_join(sei_by_cod,         by = c("cole_codigo_icfes" = "cod_icfes")) %>%
  left_join(sei_by_nombre_muni, by = c("nom_norm", "muni_norm")) %>%
  left_join(sei_by_nombre,      by = "nom_norm") %>%
  mutate(
    reporteSEI     = coalesce(reporteSEI_cod, reporteSEI_nm, reporteSEI_n),
    # Record which rule produced the match, for manual review
    sei_match_tipo = case_when(
      !is.na(reporteSEI_cod) ~ "codigo",
      !is.na(reporteSEI_nm)  ~ "nombre_municipio",
      !is.na(reporteSEI_n)   ~ "nombre",
      TRUE                    ~ "sin_cruce"
    )
  ) %>%
  select(-reporteSEI_cod, -reporteSEI_nm, -reporteSEI_n, -nom_norm, -muni_norm)

# ══════════════════════════════════════════════════════════════
# ── 6. Classify approval status into map colors
# ══════════════════════════════════════════════════════════════
df_sei <- df_sei %>%
  mutate(
    sei_categoria = case_when(
      reporteSEI == "APROBADO"                    ~ "verde",
      reporteSEI == "SIN RESPUESTA DE APROBACIÓN"  ~ "amarillo",
      reporteSEI == "RECHAZO"                      ~ "rojo",
      TRUE                                          ~ "sin_clasificar"
    )
  )

# ══════════════════════════════════════════════════════════════
# ── 7. Save the merged dataset and print match diagnostics
# ══════════════════════════════════════════════════════════════
write_dta(df_sei, file.path(outdir_sei, "MapsSurveySEI2026.dta"))
cat("\n\nDataset saved as: MapsSurveySEI2026.dta\n")
cat("Total schools:", nrow(df_sei), "\n")
cat("Schools by SEI approval status:\n")
print(table(df_sei$reporteSEI, useNA = "ifany"))
cat("Schools by match rule used:\n")
print(table(df_sei$sei_match_tipo, useNA = "ifany"))

# ══════════════════════════════════════════════════════════════
# ── 8. Export the data the map needs as JSON
# ══════════════════════════════════════════════════════════════
schools_sei_out <- df_sei %>%
  select(
    nombre      = cole_nombre_establecimiento,
    cod_dane    = cole_cod_dane,
    lat, lon,
    reporteSEI, sei_categoria,
    sede_cod    = sede_str,
    sede_nombre,
    departamento
  )

schools_sei_json <- toJSON(schools_sei_out, na = "null", auto_unbox = TRUE)

sedes_sei_tbl <- df_sei %>%
  distinct(cod = sede_str, nombre = sede_nombre) %>%
  filter(!is.na(nombre)) %>%
  arrange(nombre)

sedes_sei_json  <- toJSON(sedes_sei_tbl, auto_unbox = TRUE)
deptos_sei_json <- toJSON(sort(unique(na.omit(df_sei$departamento))))

# ══════════════════════════════════════════════════════════════
# ── 9. Build the HTML map (Leaflet, filters by cluster and department)
# ══════════════════════════════════════════════════════════════

html_sei <- paste0(
'<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Mapa SEI Colegios 2026</title>
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css">
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<style>
  body { background:#f8f9fa; }
  #map { height: 70vh; width: 100%; border-radius: 8px; box-shadow: 0 2px 12px rgba(0,0,0,.18); }
  #statsBar { font-size:.85rem; color:#555; }
  .nav-pills .nav-link.active { background:#343a40; }
  .filter-row { min-height: 44px; }
  .legend-grid { display:flex; gap:32px; flex-wrap:wrap; margin-top:12px; font-size:.87rem; }
  .legend-col h6 { font-size:.82rem; font-weight:700; margin-bottom:6px; color:#374151; }
  .legend-item { display:flex; align-items:center; gap:7px; margin-bottom:4px; }
  .leaflet-div-icon { background:none; border:none; }
</style>
</head>
<body>
<div class="container-fluid px-4 py-3">

  <div class="d-flex align-items-center gap-3 mb-3">
    <h4 class="mb-0">Colegios SEI &mdash; 2026</h4>
    <span class="badge bg-secondary" id="totalBadge"></span>
  </div>

  <!-- Nav pills -->
  <ul class="nav nav-pills mb-2" id="viewTabs">
    <li class="nav-item">
      <button class="nav-link active" onclick="setView(\'all\',this)">Colombia completa</button>
    </li>
    <li class="nav-item">
      <button class="nav-link" onclick="setView(\'sede\',this)">Por Clusters</button>
    </li>
    <li class="nav-item">
      <button class="nav-link" onclick="setView(\'depto\',this)">Por Departamento</button>
    </li>
  </ul>

  <!-- Filter dropdowns -->
  <div class="filter-row mb-2">
    <div id="sedePanel" style="display:none">
      <select id="sedeSelect" class="form-select form-select-sm d-inline-block w-auto"
              onchange="applyFilter()">
        <option value="">&#8212; Seleccionar Cluster &#8212;</option>
      </select>
    </div>
    <div id="deptoPanel" style="display:none">
      <select id="deptoSelect" class="form-select form-select-sm d-inline-block w-auto"
              onchange="applyFilter()">
        <option value="">&#8212; Seleccionar Departamento &#8212;</option>
      </select>
    </div>
  </div>

  <!-- Stats -->
  <div id="statsBar" class="mb-2">&nbsp;</div>

  <!-- Map -->
  <div id="map"></div>

  <!-- Legend -->
  <div class="legend-grid ms-1">
    <div class="legend-col">
      <h6>&#9711; Estado reporte SEI</h6>
      <div class="legend-item">
        <svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill="#15803D" stroke="#14532D" stroke-width="1.5"/></svg>
        Aprobado
      </div>
      <div class="legend-item">
        <svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill="#EAB308" stroke="#A16207" stroke-width="1.5"/></svg>
        Sin respuesta de aprobación
      </div>
      <div class="legend-item">
        <svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill="#DC2626" stroke="#7F1D1D" stroke-width="1.5"/></svg>
        Rechazo
      </div>
      <div class="legend-item">
        <svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill="#9CA3AF" stroke="#6B7280" stroke-width="1.5"/></svg>
        Sin clasificar / sin cruce
      </div>
    </div>
  </div>

</div><!-- /container -->

<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script>
// ── Data injected from R ──────────────────────────────────────
const SCHOOLS = ', schools_sei_json, ';
const SEDES   = ', sedes_sei_json, ';
const DEPTOS  = ', deptos_sei_json, ';

// ── Category configuration ───────────────────────────────────
const CAT = {
  verde:          { fill:"#15803D", border:"#14532D", label:"Aprobado" },
  amarillo:       { fill:"#EAB308", border:"#A16207", label:"Sin respuesta de aprobaci&oacute;n" },
  rojo:           { fill:"#DC2626", border:"#7F1D1D", label:"Rechazo" },
  sin_clasificar: { fill:"#9CA3AF", border:"#6B7280", label:"Sin clasificar / sin cruce" },
};

// ── Map ──────────────────────────────────────────────────────
const map = L.map("map").setView([4.57, -74.29], 6);
L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
  attribution: "&copy; OpenStreetMap contributors",
  maxZoom: 18
}).addTo(map);

// ── Helpers ──────────────────────────────────────────────────
function makeMarker(s) {
  const cfg = CAT[s.sei_categoria] || CAT["sin_clasificar"];
  return L.circleMarker([s.lat, s.lon], {
    radius: 7, fillColor: cfg.fill, color: cfg.border,
    weight: 1.5, fillOpacity: 0.85, opacity: 1
  });
}

function popupHtml(s) {
  const cfg = CAT[s.sei_categoria] || CAT["sin_clasificar"];
  return "<b>" + (s.nombre || "&#8212;") + "</b><br>" +
    "Cluster: " + (s.sede_nombre || "&#8212;") + "<br>" +
    "Departamento: " + (s.departamento || "&#8212;") + "<br>" +
    "Reporte SEI: <b>" + (s.reporteSEI || "Sin cruce") + "</b><br>" +
    "Estado: " + cfg.label;
}

// ── Create markers ───────────────────────────────────────────
const markers = [];
SCHOOLS.forEach(s => {
  if (s.lat == null || s.lon == null) return;
  const m = makeMarker(s);
  m.bindPopup(popupHtml(s), { maxWidth: 300 });
  m._schoolData = s;
  m.addTo(map);
  markers.push(m);
});

document.getElementById("totalBadge").textContent = SCHOOLS.length + " colegios";

// ── Populate dropdowns ───────────────────────────────────────
const sedeSelect  = document.getElementById("sedeSelect");
const deptoSelect = document.getElementById("deptoSelect");

SEDES.forEach(s => {
  const o = document.createElement("option");
  o.value = s.cod; o.textContent = s.nombre;
  sedeSelect.appendChild(o);
});

DEPTOS.forEach(d => {
  const o = document.createElement("option");
  o.value = d; o.textContent = d;
  deptoSelect.appendChild(o);
});

// ── State and filters ────────────────────────────────────────
let currentView = "all";

function setView(view, btn) {
  currentView = view;
  document.querySelectorAll("#viewTabs .nav-link")
    .forEach(b => b.classList.remove("active"));
  btn.classList.add("active");
  document.getElementById("sedePanel").style.display  = view === "sede"  ? "" : "none";
  document.getElementById("deptoPanel").style.display = view === "depto" ? "" : "none";
  sedeSelect.value  = "";
  deptoSelect.value = "";
  applyFilter();
}

function applyFilter() {
  let filtered = markers;
  if (currentView === "sede" && sedeSelect.value) {
    const cod = sedeSelect.value;
    filtered = markers.filter(m => m._schoolData.sede_cod === cod);
  } else if (currentView === "depto" && deptoSelect.value) {
    const dep = deptoSelect.value;
    filtered = markers.filter(m => m._schoolData.departamento === dep);
  }

  markers.forEach(m => map.removeLayer(m));
  const bounds = [];
  let verdeN = 0, amarilloN = 0, rojoN = 0, sinN = 0;

  filtered.forEach(m => {
    m.addTo(map);
    bounds.push([m._schoolData.lat, m._schoolData.lon]);
    const cat = m._schoolData.sei_categoria;
    if (cat === "verde") verdeN++;
    else if (cat === "amarillo") amarilloN++;
    else if (cat === "rojo") rojoN++;
    else sinN++;
  });

  if (bounds.length > 0) {
    if (currentView === "all") {
      map.setView([4.57, -74.29], 6);
    } else {
      map.fitBounds(bounds, { padding: [40, 40] });
    }
  }

  const total = verdeN + amarilloN + rojoN + sinN;
  document.getElementById("statsBar").innerHTML = total === 0
    ? "<em>Selecciona una opci&oacute;n del men&uacute;</em>"
    : "Mostrando <b>" + total + "</b> colegios &mdash; "
      + "<span style=\'color:#15803D\'>&#9679; <b>" + verdeN + "</b> Aprobado</span>"
      + " &nbsp;|&nbsp; "
      + "<span style=\'color:#EAB308\'>&#9679; <b>" + amarilloN + "</b> Sin respuesta</span>"
      + " &nbsp;|&nbsp; "
      + "<span style=\'color:#DC2626\'>&#9679; <b>" + rojoN + "</b> Rechazo</span>"
      + " &nbsp;|&nbsp; "
      + "<span style=\'color:#6B7280\'>&#9679; <b>" + sinN + "</b> Sin clasificar</span>";

  document.getElementById("totalBadge").textContent =
    (currentView === "all" ? SCHOOLS.length : total) + " colegios";
}

applyFilter();
</script>
</body>
</html>'
)

# ══════════════════════════════════════════════════════════════
# ── 10. Write the map to disk
# ══════════════════════════════════════════════════════════════
out_file_sei <- file.path(outdir_sei, "MapaSEI2026.html")
writeLines(html_sei, con = out_file_sei, useBytes = FALSE)
cat("\nMap saved to:", out_file_sei, "\n")
cat("Total schools:", nrow(df_sei), "\n")
cat("\nSchools by map color category:\n")
print(table(df_sei$sei_categoria, useNA = "ifany"))

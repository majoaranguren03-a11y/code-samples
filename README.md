# Coding samples

R code written by María José Aranguren Echeverri, research assistant at Universidad del Rosario, Bogotá.

These are two scripts from research projects I work on, included as coding samples for job and predoctoral applications. File paths have been replaced with placeholders and the data are not public, so the scripts are meant to be read rather than run. Comments and documentation are in English. Some interface text in the second script is in Spanish because the people who use it are Colombian field staff.

## thiessen_mining_exposure.R

Builds the spatial mining exposure variables used in the regressions of a project on the electoral consequences of mining in Colombia.

Each polling station is assigned a Thiessen (Voronoi) polygon clipped to its municipality, and the script measures hectares of mining inside that polygon, split into legal and illegal according to the registry of valid mining titles. Results are produced for 2016 and 2019 and for two independent mining sources, the UNODC grids and the CoMiMo / Dynamic World grids.

Points worth looking at.

- The script was rewritten in R from an earlier Python notebook that contained computation errors. That version processed only one year, filtered the overlay by municipality code, and built the grids from centroids.
- - Stations on a municipal boundary are kept using an intersection test plus a tolerance buffer rather than a strict within test, and stations just outside are snapped to the boundary when they fall within a distance threshold.
  - - Mining area is weighted by the share of each grid cell that falls inside the polygon, so cells crossing several polygons are not counted twice.
    - - The geometry runs on terra rather than sf. The header documents the API mapping between the two.
      - - A diagnostic file lists the stations for which no polygon could be built, so they can be reviewed by hand instead of disappearing silently.
       
        - Around 800 lines.
       
        - ## map_schools_SEI_2026.R
       
        - Field operations tool for a cluster-randomized controlled trial of a math olympiad program across nearly eight hundred public schools.
       
        - The 2026 survey round required visiting schools across the country, and each school first had to approve the visit through the data collection firm. The script merges the study sample with the firm's approval list and writes a self-contained interactive map, built with Leaflet, that the field team uses to see school by school which visits are cleared.
       
        - Points worth looking at.
       
        - - The two sources share no reliable key, so schools are matched in three cascading steps. First by ICFES school code, then by normalized name plus municipality, and last by normalized name when it is unique in the list.
          - - The step that produced each match is stored, so unmatched and weakly matched schools can be reviewed by hand rather than assumed correct.
            - - Match diagnostics are printed on every run.
              - - The output is a single HTML file with filters by cluster and by department, which matters because the people using it work from the field and cannot run R.
               
                - Around 460 lines.
               
                - ## Tools
               
                - R, with terra and sf for spatial work, leaflet for the map, and haven and readxl for the Stata and Excel inputs. I also work in Stata, Python, and LaTeX.
               
                - Contact: majoaranguren03@gmail.com
                - 

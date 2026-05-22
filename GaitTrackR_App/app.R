# app.R — Gait features from paw prints (FL/FR/HL/HR)
# FL/FR = Front, HL/HR = Hind
# Measures: (1) step length, (2) front–hind distance, (3) perpendicular deviation
# Upload Excel, define genotype/treatment (from columns OR manual), choose plot + color-by

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(writexl)
library(DT)
library(ggprism)
library(scales)
library(ggpattern)

source("image_module_ui.R")
source("image_module_server.R")


# --------- default palettes (editable in UI) ----------
default_geno_palette <- c(wt="#4D4D4D", het="#a08679", ko="#D95F02")
default_treat_palette <- c(`NA`="#999999", vehicle="#1B9E77", drug="#E41A1C")
paw_cols <- c(FL="#e31a1c", FR="#fb9a99", HL="#1f78b4", HR="#a6cee3")

palette_to_text <- function(p) {
  paste(paste0(names(p), " = ", unname(p)), collapse = "\n")
}

parse_palette_text <- function(txt){
  lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  
  kv <- strsplit(lines, "=", fixed = TRUE)
  nm  <- trimws(vapply(kv, function(x) x[1], character(1)))
  val <- trimws(vapply(kv, function(x) paste(x[-1], collapse="="), character(1)))
  
  ok <- nzchar(nm) & nzchar(val)
  nm <- nm[ok]
  val <- val[ok]
  
  # keep first occurrence if duplicates, preserving order
  keep <- !duplicated(nm)
  nm <- nm[keep]
  val <- val[keep]
  
  pal <- val
  names(pal) <- nm
  
  list(
    levels = nm,   # <- ordered levels come from line order
    colors = pal
  )
}

parse_exclude_ids <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(character(0))
  ids <- unlist(strsplit(txt, "[,\\s]+"))  # split on commas OR whitespace
  ids <- trimws(ids)
  ids <- ids[nzchar(ids)]
  unique(ids)
}

# ---- geno_trt ordering: genotype first, then treatment ----
make_geno_trt_levels <- function(geno_levels, treat_levels) {
  eg <- expand.grid(
    genotype = geno_levels,
    treatment = treat_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  paste0(eg$genotype, "_", eg$treatment)
}

make_geno_trt_factor <- function(genotype, treatment, geno_levels, treat_levels) {
  gt <- paste0(as.character(genotype), "_", as.character(treatment))
  lv <- make_geno_trt_levels(geno_levels, treat_levels)
  factor(gt, levels = lv)
}

# ---- geno_trt appearance: geno color + treatment pattern (if available) ----
# If ggpattern is installed, we can do real diagonal stripes; otherwise fallback to alpha-tinted fills.
has_ggpattern <- function() requireNamespace("ggpattern", quietly = TRUE)

# map treatment -> pattern (you can tweak these)
treatment_to_pattern <- function(trt) {
  # common choices: "none", "stripe", "crosshatch", "circle", ...
  dplyr::case_when(
    trt %in% c("NA", "vehicle") ~ "none",
    TRUE ~ "stripe"
  )
}

# When ggpattern isn't available: same geno color but different alpha by treatment
# (keeps "same color family" idea without extra dependency)
geno_trt_fill_fallback <- function(geno_trt_levels, geno_pal, treat_levels) {
  # alpha per treatment (ordered)
  # first treatment most opaque, later ones more transparent
  a <- seq(1.0, 0.35, length.out = max(1, length(treat_levels)))
  names(a) <- treat_levels
  
  # build fills: base geno color + alpha(treatment)
  # geno_trt_levels are "geno / trt" in the correct order
  fills <- vapply(geno_trt_levels, function(gt) {
    parts <- strsplit(gt, "_", fixed = TRUE)[[1]]
    g <- parts[1]; t <- parts[2]
    base <- unname(geno_pal$colors[g])
    if (is.na(base) || !nzchar(base)) base <- "#999999"
    scales::alpha(base, alpha = a[[t]] %||% 0.75)
  }, character(1))
  names(fills) <- geno_trt_levels
  fills
}

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x






# --------- small helpers for features ----------
straighten_mouse <- function(df){
  dfc <- df %>% filter(!is.na(x), !is.na(y))
  if(nrow(dfc) < 2) return(df %>% mutate(x_along_cm=NA_real_, y_perp_cm=NA_real_))
  m <- lm(y ~ x, data=dfc); b <- coef(m)[2]
  uF <- c(1,b)/sqrt(1+b^2); uP <- c(-b,1)/sqrt(1+b^2)
  mx <- mean(dfc$x); my <- mean(dfc$y)
  xc <- df$x - mx; yc <- df$y - my
  x_along <- xc*uF[1] + yc*uF[2]
  y_perp  <- xc*uP[1] + yc*uP[2]
  s <- unique(df$pixels_per_cm); if(length(s)!=1 || is.na(s)) s <- 1
  df %>% mutate(x_along_cm = x_along/s, y_perp_cm = y_perp/s)
}

assign_steps <- function(df_side, max_gap_cm = 2){
  F <- df_side %>% filter(segment=="Front") %>% arrange(x_along_cm)
  H <- df_side %>% filter(segment=="Hind")  %>% arrange(x_along_cm)
  nF <- nrow(F); nH <- nrow(H)
  if(nF==0 && nH==0) return(tibble(dot_id=integer(), true_step_id=integer()))
  
  if(nF>0 && nH>0 && nF==nH){
    return(bind_rows(
      F %>% mutate(true_step_id=row_number()) %>% select(dot_id,true_step_id),
      H %>% mutate(true_step_id=row_number()) %>% select(dot_id,true_step_id)
    ))
  }
  
  i <- 1; j <- 1; step <- 1; out <- list()
  while(i<=nF || j<=nH){
    fx <- if(i<=nF) F$x_along_cm[i] else Inf
    hx <- if(j<=nH) H$x_along_cm[j] else Inf
    if(is.finite(fx) && is.finite(hx) && abs(fx-hx) <= max_gap_cm){
      out[[length(out)+1]] <- tibble(dot_id=c(F$dot_id[i], H$dot_id[j]), true_step_id=step)
      i <- i+1; j <- j+1
    } else if(fx < hx){
      if(is.finite(fx)) out[[length(out)+1]] <- tibble(dot_id=F$dot_id[i], true_step_id=step)
      i <- i+1
    } else {
      if(is.finite(hx)) out[[length(out)+1]] <- tibble(dot_id=H$dot_id[j], true_step_id=step)
      j <- j+1
    }
    step <- step+1
  }
  bind_rows(out)
}

perpendicular_events <- function(df_segment){
  # df_segment contains a single mouse_id/image_id/segment with x_along_cm, y_perp_cm and side (L/R)
  d <- df_segment %>% 
    dplyr::filter(is.finite(x_along_cm), is.finite(y_perp_cm), side %in% c("L","R")) %>%
    dplyr::arrange(x_along_cm)

  if (nrow(d) < 3) return(tibble::tibble(segment=character(), ref_side=character(), perpendicular_dist_cm=numeric()))

  # split by side
  L <- d %>% dplyr::filter(side == "L") %>% dplyr::arrange(x_along_cm)
  R <- d %>% dplyr::filter(side == "R") %>% dplyr::arrange(x_along_cm)

  # helper: point-to-line distance for point P0 to line through P1->P2 in 2D
  p2l <- function(x0,y0,x1,y1,x2,y2){
    num <- abs((x2-x1)*(y1-y0) - (x1-x0)*(y2-y1))
    den <- sqrt((x2-x1)^2 + (y2-y1)^2)
    ifelse(den > 0, num/den, NA_real_)
  }

  out <- list()

  # For each consecutive pair on one side, find an opposite-side point whose x is between them.
  calc_ref <- function(A, B, ref_label){
    if (nrow(A) < 2 || nrow(B) < 1) return(NULL)
    for (i in 1:(nrow(A)-1)){
      x1 <- A$x_along_cm[i];   y1 <- A$y_perp_cm[i]
      x2 <- A$x_along_cm[i+1]; y2 <- A$y_perp_cm[i+1]
      # require forward order
      if (!is.finite(x1) || !is.finite(x2) || x2 <= x1) next
      mid <- (x1 + x2)/2
      cand <- B %>% dplyr::filter(x_along_cm > x1, x_along_cm < x2)
      if (nrow(cand) < 1) next
      # choose the one closest to the midpoint (mouse-like alternation usually gives exactly one)
      j <- which.min(abs(cand$x_along_cm - mid))
      x0 <- cand$x_along_cm[j]; y0 <- cand$y_perp_cm[j]
      out[[length(out)+1]] <<- tibble::tibble(
        segment = unique(d$segment)[1],
        ref_side = ref_label,
        perpendicular_dist_cm = p2l(x0,y0,x1,y1,x2,y2)
      )
    }
    NULL
  }

  calc_ref(L, R, "L")
  calc_ref(R, L, "R")

  dplyr::bind_rows(out) %>% dplyr::filter(is.finite(perpendicular_dist_cm))
}


# --------- UI ----------
ui <- fluidPage(
  titlePanel("GaitTrackR — Gait features from paw prints"),
  tabsetPanel(
    tabPanel("\U0001F5BC  Image \u2192 Data", br(), imageAnnotationUI()),
    tabPanel("\U0001F4CA  Analysis",
    br(),
    sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel (.xlsx)", accept = ".xlsx"),
      checkboxInput("align", "Straighten tracks (alignment)", FALSE),
      numericInput("max_gap", "Step pairing max gap (cm)", 2, min=0, step=0.1),
      checkboxInput("norm_length", "Normalize distance measures by mouse length", FALSE),
      tags$small("If mouse length (cm) is provided, distance-based readouts can be reported as a fraction of body length."),
      hr(),
      
      
      radioButtons("meta_mode", "Genotype / Treatment",
                   choices = c("Use columns in file"="cols", "Enter manually"="manual"),
                   selected = "cols"),
      uiOutput("meta_cols_ui"),
      conditionalPanel("input.meta_mode=='manual'",
                       tags$small("Edit genotype / treatment for each mouse below.")),
      
     
      selectInput(
        "color_by",
        "Color by",
        choices = c("genotype", "treatment", "genotype+treatment", "none"),
        selected = "genotype"
      ),
      hr(),
      
      hr(),
      tags$strong("Exclude mice (mouse_id)"),
      tags$small("Comma, space, or newline separated. These mice will be removed from all analyses/plots."),
      textAreaInput("exclude_ids", NULL, value = "", rows = 3),
      
      hr(),
      
      selectInput(
        "plot_format",
        "Download plot as",
        choices = c("png", "pdf", "svg", "jpeg"),
        selected = "png"
      ),
      downloadButton("download_plot", "Download current plot"),
      hr(),
      
      selectInput("plot_type", "Plot type", choices = c(
        "Mean ± SD (bar)" = "mean_sd",
        "Mean CV ± SD (bar)" = "cv_sd",
        "Per side (FB distance): bar + points" = "side_fb",
        "Per segment (Perpendicular): bar + points" = "seg_perp",
        "Base of support (L-R): bar + points" = "bos",
        "Paw overlap (hind vs front): bar + points" = "overlap",
        "Per paw step length: bar + points" = "paw_step",
        "Left-right asymmetry index: bar + points" = "asym",
        "Drift / stability (y_perp): bar + points" = "drift",
        "QC tracks (along vs perp)" = "qc_tracks"
      ), selected = "mean_sd"),
      
      conditionalPanel(
        "input.plot_type=='mean_sd' || input.plot_type=='cv_sd'",
        selectInput("measure", "Measurement", choices = c(
          "Step length" = "step",
          "Front–hind distance (2D)" = "fb2d",
          "Front–hind distance (x only)" = "fbx",
          "Perpendicular deviation" = "perp"
        ), selected = "step")
      ),
      
      conditionalPanel(
        "input.plot_type=='qc_tracks'",
        selectInput("qc_mouse", "QC mouse_id", choices = character(0)),
        checkboxInput("qc_show_labels", "Show dot_id labels", TRUE)
      ),
      


      conditionalPanel(
        "input.plot_type=='side_fb'",
        selectInput("fb_mode", "FB distance mode", choices = c(
          "2D distance (x and y)" = "fb2d",
          "x-only distance (ignore y)" = "fbx"
        ), selected = "fb2d")
      ),
      
      
      hr(),
      
      tags$strong("Genotype colors (name = hex)"),
      tags$small("Order matters: top-to-bottom becomes the genotype order in plots."),
      textAreaInput("geno_pal", NULL, palette_to_text(default_geno_palette), rows = 4),
      
      tags$br(),
      
      tags$strong("Treatment colors (name = hex)"),
      tags$small("Order matters: top-to-bottom becomes the treatment order in plots."),
      textAreaInput("treat_pal", NULL, palette_to_text(default_treat_palette), rows = 5),
      
      
      hr(),


      downloadButton("download_features", "Download mouse-level features (.xlsx)"),
      tags$br(),
      downloadButton("download_steps", "Download step-level table (.xlsx)")
    ),
    mainPanel(
      h4("Data preview"),
      DTOutput("preview"),
      conditionalPanel("input.meta_mode=='manual'",
                       hr(),
                       h4("Manual genotype/treatment table"),
                       DTOutput("meta_table")),
      hr(),
      h4("Mouse-level features"),
      DTOutput("features_table"),
      hr(),
      plotOutput("plot", height = 520),
      hr(),
      
    )   # closes mainPanel
  )     # closes sidebarLayout
  )     # closes tabPanel("Analysis")
  )     # closes tabsetPanel
)       # closes fluidPage

# --------- server ----------
server <- function(input, output, session){
  imageAnnotationServer(input, output, session)
  raw_df <- reactive({
    req(input$file)
    readxl::read_xlsx(input$file$datapath)
  })
  
  output$preview <- renderDT({
    req(raw_df())
    datatable(head(raw_df(), 50), options=list(scrollX=TRUE, pageLength=8))
  })
  
  output$meta_cols_ui <- renderUI({
    req(raw_df())
    cols <- names(raw_df())
    if (input$meta_mode == "cols") {
      tagList(
        selectInput("geno_col", "Genotype column", choices = c("None", cols),
                    selected = if ("genotype" %in% cols) "genotype" else "None"),
        selectInput("treat_col", "Treatment column", choices = c("None", cols),
                    selected = if ("treatment" %in% cols) "treatment" else "None"),
        selectInput("length_col", "Mouse length column (cm)", choices = c("None", cols),
                    selected = if ("mouse_length_cm" %in% cols) "mouse_length_cm" else "None")
      )
    } else NULL
  })
  excluded_ids <- reactive({
    parse_exclude_ids(input$exclude_ids)
  })
  meta_manual <- reactiveVal(NULL)
  
  observeEvent(raw_df(), {
    df <- raw_df()
    validate(need("mouse_id" %in% names(df), "Excel must contain a 'mouse_id' column."))
    
    keep_ids <- setdiff(unique(df$mouse_id), excluded_ids())
    
    meta_manual(
      tibble(mouse_id = keep_ids) %>%
        arrange(mouse_id) %>%
        mutate(genotype = NA_character_, treatment = NA_character_, mouse_length_cm = NA_real_)
    )
  })
  
  
  observeEvent(paws_rot(), {
    df <- paws_rot()
    ids <- sort(unique(df$mouse_id))
    updateSelectInput(session, "qc_mouse", choices = ids, selected = ids[1])
  }, ignoreInit = TRUE)
  
  
  output$meta_table <- renderDT({
    req(meta_manual())
    datatable(meta_manual(), editable=TRUE, options=list(pageLength=10, scrollX=TRUE))
  })
  
  observeEvent(input$meta_table_cell_edit, {
    info <- input$meta_table_cell_edit
    df <- meta_manual()
    df[info$row, info$col] <- info$value
    meta_manual(df)
  })
  
  paws <- reactive({
    df <- raw_df()
    
    needed <- c("mouse_id","paw","x","y","dot_id")
    validate(need(all(needed %in% names(df)),
                  paste("Missing required columns:", paste(setdiff(needed, names(df)), collapse=", "))))
    
    # ---- EXCLUDE mice early ----
    ex <- excluded_ids()
    if (length(ex) > 0) {
      df <- df %>% filter(!mouse_id %in% ex)
    }
    
    if (!"image_id" %in% names(df)) df <- df %>% mutate(image_id = 1L)
    if (!"pixels_per_cm" %in% names(df)) df <- df %>% mutate(pixels_per_cm = 1)
    
    # genotype/treatment source
    if (input$meta_mode == "cols") {
      geno <- if (!is.null(input$geno_col) && input$geno_col != "None" && input$geno_col %in% names(df)) df[[input$geno_col]] else NA
      trt  <- if (!is.null(input$treat_col) && input$treat_col != "None" && input$treat_col %in% names(df)) df[[input$treat_col]] else NA
      lenv <- if (!is.null(input$length_col) && input$length_col != "None" && input$length_col %in% names(df)) df[[input$length_col]] else NA
      df <- df %>% mutate(genotype = geno, treatment = trt, mouse_length_cm = lenv)
    } else {
      req(meta_manual())
      df <- df %>% left_join(meta_manual(), by="mouse_id")
    }
    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)
    
    df <- df %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels),
        mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm))
      )
    df %>%
      mutate(
        paw = factor(paw, levels=c("FL","FR","HL","HR")),
        side = if_else(grepl("L$", as.character(paw)), "L", "R"),
        segment = if_else(grepl("^F", as.character(paw)), "Front", "Hind"),
        y = 3024 - y
      ) %>%
      group_by(mouse_id, image_id, paw) %>%
      arrange(dot_id, .by_group = TRUE) %>%
      mutate(step_id = row_number()) %>%
      ungroup()
  })
  
  
  paws_rot <- reactive({
    df <- paws()
    
    out <- df %>%
      group_by(mouse_id, image_id) %>%
      group_modify(~{
        if (isTRUE(input$align)) {
          straighten_mouse(.x)
        } else {
          d <- .x
          dc <- d %>% filter(!is.na(x), !is.na(y))
          if(nrow(dc) < 1) return(d %>% mutate(x_along_cm=NA_real_, y_perp_cm=NA_real_))
          mx <- mean(dc$x); my <- mean(dc$y)
          s <- unique(d$pixels_per_cm); if(length(s)!=1 || is.na(s)) s <- 1
          d %>% mutate(x_along_cm = (x-mx)/s, y_perp_cm = (y-my)/s)
        }
      }) %>%
      ungroup()
    
    ts <- out %>%
      filter(!is.na(dot_id), !is.na(x_along_cm)) %>%
      group_by(mouse_id, image_id, side) %>%
      group_modify(~ assign_steps(.x, max_gap_cm = input$max_gap)) %>%
      ungroup()
    
    out %>% left_join(ts, by=c("mouse_id","image_id","side","dot_id"))
  })
  
  features_mouse <- reactive({
    df <- paws_rot()

    # step length
    step_df <- df %>%
      filter(!is.na(x_along_cm)) %>%
      arrange(mouse_id, image_id, paw, x_along_cm) %>%
      group_by(mouse_id, image_id, paw) %>%
      mutate(step_length_cm = x_along_cm - lag(x_along_cm)) %>%
      ungroup() %>%
      filter(!is.na(step_length_cm))

    step_mouse <- step_df %>%
      group_by(mouse_id) %>%
      summarise(
        mean_step_length = mean(step_length_cm, na.rm=TRUE),
        cv_step_length   = sd(step_length_cm, na.rm=TRUE) / abs(mean_step_length),
        .groups="drop"
      )

    # per-paw step summaries (FL/FR/HL/HR)
    step_paw_mouse <- step_df %>%
      group_by(mouse_id, paw) %>%
      summarise(
        mean_step_length_paw = mean(step_length_cm, na.rm=TRUE),
        sd_step_length_paw   = sd(step_length_cm, na.rm=TRUE),
        cv_step_length_paw   = sd_step_length_paw / abs(mean_step_length_paw),
        .groups="drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = paw,
        values_from = c(mean_step_length_paw, cv_step_length_paw),
        names_sep = "_"
      )

    # front–hind distance (2D + x-only)
    fb_df <- df %>%
      filter(!is.na(true_step_id), !is.na(x_along_cm), !is.na(y_perp_cm)) %>%
      group_by(mouse_id, image_id, side, true_step_id) %>%
      summarise(
        front_x = first(x_along_cm[segment=="Front"]),
        hind_x  = first(x_along_cm[segment=="Hind"]),
        front_y = first(y_perp_cm[segment=="Front"]),
        hind_y  = first(y_perp_cm[segment=="Hind"]),
        has_both = any(segment=="Front") & any(segment=="Hind"),
        fb_distance_2d_cm = if_else(has_both, sqrt((front_x-hind_x)^2 + (front_y-hind_y)^2), NA_real_),
        fb_distance_x_cm  = if_else(has_both, abs(front_x - hind_x), NA_real_),
        .groups="drop"
      )

    fb_mouse <- fb_df %>%
      group_by(mouse_id) %>%
      summarise(
        mean_fb_distance_2d = mean(fb_distance_2d_cm, na.rm=TRUE),
        cv_fb_distance_2d   = sd(fb_distance_2d_cm, na.rm=TRUE) / abs(mean_fb_distance_2d),
        mean_fb_distance_x  = mean(fb_distance_x_cm,  na.rm=TRUE),
        cv_fb_distance_x    = sd(fb_distance_x_cm,  na.rm=TRUE) / abs(mean_fb_distance_x),
        .groups="drop"
      )

    # paw overlap (hind relative to front) using true_step_id pairing within side
    overlap_mouse <- fb_df %>%
      filter(is.finite(front_x), is.finite(hind_x), has_both) %>%
      mutate(
        overlap_signed_cm = hind_x - front_x,
        overlap_abs_cm    = abs(hind_x - front_x)
      ) %>%
      group_by(mouse_id) %>%
      summarise(
        mean_overlap_signed = mean(overlap_signed_cm, na.rm=TRUE),
        mean_overlap_abs    = mean(overlap_abs_cm, na.rm=TRUE),
        cv_overlap_abs      = sd(overlap_abs_cm, na.rm=TRUE) / abs(mean_overlap_abs),
        .groups="drop"
      )

    # base of support (L-R distance) per segment
    bos_lr <- df %>%
      filter(is.finite(x_along_cm), is.finite(y_perp_cm)) %>%
      group_by(mouse_id, image_id, segment, side) %>%
      arrange(x_along_cm, .by_group = TRUE) %>%
      mutate(lr_index = row_number()) %>%
      ungroup() %>%
      select(mouse_id, image_id, segment, side, lr_index, y_perp_cm) %>%
      tidyr::pivot_wider(names_from = side, values_from = y_perp_cm, names_prefix = "y_") %>%
      mutate(bos_cm = abs(y_L - y_R)) %>%
      filter(is.finite(bos_cm))

    bos_mouse <- bos_lr %>%
      group_by(mouse_id, segment) %>%
      summarise(
        mean_bos = mean(bos_cm, na.rm=TRUE),
        sd_bos   = sd(bos_cm, na.rm=TRUE),
        cv_bos   = sd_bos / abs(mean_bos),
        .groups="drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = segment,
        values_from = c(mean_bos, cv_bos),
        names_sep = "_"
      )

    # perpendicular deviation
    perp_df <- df %>%
      filter(!is.na(x_along_cm), !is.na(y_perp_cm), segment %in% c("Front","Hind")) %>%
      group_by(mouse_id, image_id, segment) %>%
      group_modify(~ perpendicular_events(.x)) %>%
      ungroup()

    perp_mouse <- perp_df %>%
      group_by(mouse_id) %>%
      summarise(
        mean_perpendicular = mean(perpendicular_dist_cm, na.rm=TRUE),
        cv_perpendicular   = sd(perpendicular_dist_cm, na.rm=TRUE) / abs(mean_perpendicular),
        .groups="drop"
      )

    # drift / straightness-like summaries from y_perp
    drift_mouse <- df %>%
      filter(is.finite(y_perp_cm)) %>%
      group_by(mouse_id) %>%
      summarise(
        mean_drift_y     = mean(y_perp_cm, na.rm=TRUE),
        mean_abs_drift_y = mean(abs(y_perp_cm), na.rm=TRUE),
        sd_drift_y       = sd(y_perp_cm, na.rm=TRUE),
        range_drift_y    = diff(range(y_perp_cm, na.rm=TRUE)),
        .groups="drop"
      )

    # meta (includes mouse_length_cm if provided)
    meta <- df %>% distinct(mouse_id, genotype, treatment, mouse_length_cm)

    # asymmetry indices from per-paw mean step lengths
    asym_from_paws <- function(L, R) {
      ifelse(is.finite(L) & is.finite(R) & (abs(L) + abs(R)) > 0,
             abs(L - R) / ((abs(L) + abs(R)) / 2),
             NA_real_)
    }

    asym_mouse <- step_paw_mouse %>%
      mutate(
        asym_step_front = asym_from_paws(mean_step_length_paw_FL, mean_step_length_paw_FR),
        asym_step_hind  = asym_from_paws(mean_step_length_paw_HL, mean_step_length_paw_HR)
      ) %>%
      select(mouse_id, asym_step_front, asym_step_hind)

    out <- meta %>%
      left_join(step_mouse,     by="mouse_id") %>%
      left_join(step_paw_mouse, by="mouse_id") %>%
      left_join(asym_mouse,     by="mouse_id") %>%
      left_join(fb_mouse,       by="mouse_id") %>%
      left_join(bos_mouse,      by="mouse_id") %>%
      left_join(overlap_mouse,  by="mouse_id") %>%
      left_join(drift_mouse,    by="mouse_id") %>%
      left_join(perp_mouse,     by="mouse_id")

    # add length-normalized versions (divide by mouse_length_cm)
    out <- out %>%
      mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm))) %>%
      mutate(
        mean_step_length_norm      = mean_step_length      / mouse_length_cm,
        mean_fb_distance_2d_norm   = mean_fb_distance_2d   / mouse_length_cm,
        mean_fb_distance_x_norm    = mean_fb_distance_x    / mouse_length_cm,
        mean_perpendicular_norm    = mean_perpendicular    / mouse_length_cm,
        mean_overlap_signed_norm   = mean_overlap_signed   / mouse_length_cm,
        mean_overlap_abs_norm      = mean_overlap_abs      / mouse_length_cm,
        mean_bos_Front_norm        = mean_bos_Front        / mouse_length_cm,
        mean_bos_Hind_norm         = mean_bos_Hind         / mouse_length_cm,
        mean_drift_y_norm          = mean_drift_y          / mouse_length_cm,
        mean_abs_drift_y_norm      = mean_abs_drift_y      / mouse_length_cm,
        sd_drift_y_norm            = sd_drift_y            / mouse_length_cm,
        range_drift_y_norm         = range_drift_y         / mouse_length_cm,
        mean_step_length_paw_FL_norm = mean_step_length_paw_FL / mouse_length_cm,
        mean_step_length_paw_FR_norm = mean_step_length_paw_FR / mouse_length_cm,
        mean_step_length_paw_HL_norm = mean_step_length_paw_HL / mouse_length_cm,
        mean_step_length_paw_HR_norm = mean_step_length_paw_HR / mouse_length_cm
      )

    out %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))
  })



# ---- Step-level / event-level table for export (duplicates OK) ----
  steps_table <- reactive({
    req(paws_rot())

    df <- paws_rot()

    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)

    meta <- df %>%
      distinct(mouse_id, image_id, genotype, treatment, mouse_length_cm) %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)), levels = treat_pal$levels),
        geno_trt  = factor(
          paste0(as.character(genotype), "_", as.character(treatment)),
          levels = make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        )
      )

    # Step length per paw print (between consecutive prints of the same paw within image)
    step_len <- df %>%
      filter(!is.na(x_along_cm)) %>%
      arrange(mouse_id, image_id, paw, x_along_cm) %>%
      group_by(mouse_id, image_id, paw) %>%
      mutate(step_length_cm = x_along_cm - lag(x_along_cm)) %>%
      ungroup() %>%
      mutate(measure_type = "step_length")

    # FB distances per paired step (true_step_id) and side
    fb_df <- df %>%
      filter(!is.na(true_step_id), !is.na(x_along_cm), !is.na(y_perp_cm)) %>%
      group_by(mouse_id, image_id, side, true_step_id) %>%
      summarise(
        segment_front_present = any(segment=="Front"),
        segment_hind_present  = any(segment=="Hind"),
        front_x = dplyr::first(x_along_cm[segment=="Front"]),
        hind_x  = dplyr::first(x_along_cm[segment=="Hind"]),
        front_y = dplyr::first(y_perp_cm[segment=="Front"]),
        hind_y  = dplyr::first(y_perp_cm[segment=="Hind"]),
        fb_distance_2d_cm = if_else(segment_front_present & segment_hind_present,
                                   sqrt((front_x-hind_x)^2 + (front_y-hind_y)^2),
                                   NA_real_),
        fb_distance_x_cm  = if_else(segment_front_present & segment_hind_present,
                                   abs(front_x-hind_x),
                                   NA_real_),
        .groups="drop"
      ) %>%
      mutate(measure_type = "fb_distance")

    # Perpendicular deviation events (updated definition), per segment and reference side
    perp_events <- df %>%
      filter(!is.na(x_along_cm), !is.na(y_perp_cm), segment %in% c("Front","Hind")) %>%
      group_by(mouse_id, image_id, segment) %>%
      group_modify(~ perpendicular_events(.x)) %>%
      ungroup() %>%
      mutate(measure_type = "perpendicular_deviation")

    # Combine into one long table (duplicates OK)
    # Keep relevant identifying columns + computed values
    out_step_len <- step_len %>%
      select(mouse_id, image_id, paw, side, segment, dot_id, step_id,
             x, y, x_along_cm, y_perp_cm, step_length_cm, measure_type) %>%
      left_join(meta, by=c("mouse_id","image_id"))

    out_fb <- fb_df %>%
      select(mouse_id, image_id, side, true_step_id,
             fb_distance_2d_cm, fb_distance_x_cm, measure_type) %>%
      left_join(meta, by=c("mouse_id","image_id"))

    out_perp <- perp_events %>%
      select(mouse_id, image_id, segment, ref_side, perpendicular_dist_cm, measure_type) %>%
      left_join(meta, by=c("mouse_id","image_id"))

    # Return as a list of sheets, so it stays readable in Excel
    list(
      step_length = out_step_len,
      fb_distance = out_fb,
      perpendicular_deviation = out_perp
    )
  })
  output$features_table <- renderDT({
    req(features_mouse())
    datatable(features_mouse(), options=list(scrollX=TRUE, pageLength=10))
  })
  
  output$download_features <- downloadHandler(
    filename = function() "gait_features_mouse_level.xlsx",
    content = function(file) writexl::write_xlsx(features_mouse(), file)
  )
  
  

  output$download_steps <- downloadHandler(
    filename = function() "gait_step_level_tables.xlsx",
    content = function(file) {
      req(steps_table())
      # steps_table() returns a named list of data.frames -> multiple sheets
      writexl::write_xlsx(steps_table(), file)
    }
  )

output$download_plot <- downloadHandler(
    filename = function() {
      paste0("gait_plot_", input$plot_type, ".", input$plot_format)
    },
    content = function(file) {
      req(plot_obj())
      
      fmt <- tolower(input$plot_format)
      
      # svg needs svglite
      if (fmt == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
        stop("Package 'svglite' is required for SVG export. Install it with install.packages('svglite').")
      }
      
      dev <- switch(
        fmt,
        "png"  = "png",
        "pdf"  = "pdf",
        "jpeg" = "jpeg",
        "jpg"  = "jpeg",
        "svg"  = "svg",
        "png"
      )
      
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj(),
        device = dev,
        width = 8,
        height = 6,
        units = "in",
        dpi = 300
      )
    }
  )
  
  
  plot_obj <- reactive({
    req(features_mouse(), paws_rot())
    
    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)
    
    gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
    
    f <- features_mouse() %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)), levels = treat_pal$levels),
        geno_trt  = factor(
          paste0(as.character(genotype), "_", as.character(treatment)),
          levels = gt_levels
        )
      )
    
    
    grp <- input$color_by
    if (grp == "none") grp <- NULL
    if (identical(grp, "genotype+treatment")) grp <- "geno_trt"
    
    # --- QC tracks ---
    if (input$plot_type == "qc_tracks") {
      df <- paws_rot()
      req(input$qc_mouse)
      
      df <- df %>% filter(mouse_id == input$qc_mouse)
      
      p <- ggplot(df, aes(x = x_along_cm, y = y_perp_cm, colour = paw)) +
        geom_path(aes(group = interaction(image_id, paw)), linewidth = 0.6, alpha = 0.8) +
        geom_point(size = 2) +
        scale_colour_manual(values = paw_cols) +
        facet_wrap(~ image_id, ncol = 1) +
        coord_equal() +
        ggprism::theme_prism(base_size = 14, base_family = "Arial") +
        labs(
          title = paste("Tracks:", input$qc_mouse, "—", ifelse(input$align, "Aligned", "Raw centered")),
          x = "x_along (cm)", y = "y_perp (cm)"
        )
      
      if (isTRUE(input$qc_show_labels)) {
        p <- p + ggrepel::geom_label_repel(aes(label = dot_id), size = 3)
      }
      
      return(p)
    }
    
    # choose measure columns
    if (input$measure == "step") {
      mean_col_raw <- "mean_step_length"
      mean_col_norm <- "mean_step_length_norm"
      cv_col <- "cv_step_length"
      y_mean_raw <- "Mean step length (cm)"
      y_mean_norm <- "Step length / body length"
      y_cv <- "CV(step length)"
    } else if (input$measure == "fb2d") {
      mean_col_raw <- "mean_fb_distance_2d"
      mean_col_norm <- "mean_fb_distance_2d_norm"
      cv_col <- "cv_fb_distance_2d"
      y_mean_raw <- "Mean FB distance 2D (cm)"
      y_mean_norm <- "FB distance 2D / body length"
      y_cv <- "CV(FB distance 2D)"
    } else if (input$measure == "fbx") {
      mean_col_raw <- "mean_fb_distance_x"
      mean_col_norm <- "mean_fb_distance_x_norm"
      cv_col <- "cv_fb_distance_x"
      y_mean_raw <- "Mean FB distance x-only (cm)"
      y_mean_norm <- "FB distance x-only / body length"
      y_cv <- "CV(FB distance x-only)"
    } else {
      mean_col_raw <- "mean_perpendicular"
      mean_col_norm <- "mean_perpendicular_norm"
      cv_col <- "cv_perpendicular"
      y_mean_raw <- "Mean perpendicular (cm)"
      y_mean_norm <- "Perpendicular / body length"
      y_cv <- "CV(perpendicular)"
    }

    use_norm <- isTRUE(input$norm_length)
    mean_col <- if (use_norm) mean_col_norm else mean_col_raw
    y_mean   <- if (use_norm) y_mean_norm else y_mean_raw

    # --- mean/cv by group barplots ---
    if (input$plot_type %in% c("mean_sd","cv_sd")) {
      if (is.null(grp)) {
        if (input$plot_type == "mean_sd") {
          m <- mean(f[[mean_col]], na.rm=TRUE); s <- sd(f[[mean_col]], na.rm=TRUE)
          return(
            ggplot(data.frame(group="all", mean=m, sd=s), aes(x=group, y=mean)) +
              geom_col(colour="grey20") +
              geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
              ggprism::theme_prism(base_size = 14,base_family = "Arial") + labs(title="Mean ± SD", x=NULL, y=y_mean)
          )
        } else {
          m <- mean(f[[cv_col]], na.rm=TRUE); s <- sd(f[[cv_col]], na.rm=TRUE)
          return(
            ggplot(data.frame(group="all", mean=m, sd=s), aes(x=group, y=mean)) +
              geom_col(colour="grey20") +
              geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
              ggprism::theme_prism(base_size = 14,base_family = "Arial") + labs(title="Mean CV ± SD", x=NULL, y=y_cv)
          )
        }
      } else {
        val_col <- if (input$plot_type=="mean_sd") mean_col else cv_col
        ylab <- if (input$plot_type=="mean_sd") y_mean else y_cv
        ttl  <- if (input$plot_type=="mean_sd") "Mean ± SD" else "Mean CV ± SD"
        
        summ <- f %>%
          group_by(.data[[grp]]) %>%
          summarise(mean = mean(.data[[val_col]], na.rm=TRUE),
                    sd   = sd(.data[[val_col]], na.rm=TRUE),
                    .groups="drop") %>%
          rename(group = 1)
        
        p <- ggplot(summ, aes(x=group, y=mean, fill=group)) +
          geom_col(colour="grey20") +
          geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
          ggprism::theme_prism(base_size = 14,base_family = "Arial") +
          labs(title=paste(ttl, "—", grp), x=grp, y=ylab)
        
        if (grp=="genotype" && length(geno_pal$colors) > 0) p <- p + scale_fill_manual(values = geno_pal$colors, drop = FALSE)
        if (grp=="treatment" && length(treat_pal$colors) > 0) p <- p + scale_fill_manual(values = treat_pal$colors, drop = FALSE)
        
        if (grp == "geno_trt") {
          # Always apply the alpha-based geno_trt palette (stable + no extra geom requirements)
          gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)

          # Ensure x-axis order matches palette-box order
          summ <- summ %>% mutate(group = factor(as.character(group), levels = gt_levels))

          fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
          p <- p + scale_fill_manual(values = fills, drop = FALSE)
        }

        
        return(p)
      }
    }
    
    # --- per side (FB) ---
    if (input$plot_type == "side_fb") {
      df <- paws_rot() %>% distinct(mouse_id, genotype, treatment, mouse_length_cm) %>%
        left_join(
          paws_rot() %>%
            filter(!is.na(true_step_id), !is.na(x_along_cm), !is.na(y_perp_cm)) %>%
            group_by(mouse_id, image_id, side, true_step_id) %>%
            summarise(
              front_x = first(x_along_cm[segment=="Front"]),
              hind_x  = first(x_along_cm[segment=="Hind"]),
              front_y = first(y_perp_cm[segment=="Front"]),
              hind_y  = first(y_perp_cm[segment=="Hind"]),
              has_both = any(segment=="Front") & any(segment=="Hind"),
              fb_distance_2d_cm = if_else(has_both, sqrt((front_x-hind_x)^2 + (front_y-hind_y)^2), NA_real_),
        fb_distance_x_cm  = if_else(has_both, abs(front_x - hind_x), NA_real_),
              .groups="drop"
            ) %>%
            group_by(mouse_id, side) %>%
            summarise(mean_fb_distance = if (identical(input$fb_mode, "fbx")) mean(fb_distance_x_cm, na.rm=TRUE) else mean(fb_distance_2d_cm, na.rm=TRUE), .groups="drop"),
          by="mouse_id"
        )

      # optional length normalization (per mouse)
      if (isTRUE(input$norm_length)) {
        df <- df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                            mean_fb_distance = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                                       mean_fb_distance / mouse_length_cm, NA_real_))
      }

      gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
      
      df <- df %>%
        mutate(
          genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
          treatment = if_else(is.na(treatment) | treatment == "", "NA", as.character(treatment)),
          treatment = factor(treatment, levels = treat_pal$levels),
          geno_trt  = factor(paste0(as.character(genotype), "_", as.character(treatment)),
                             levels = gt_levels)
        )
      
      
      
      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"  # sensible fallback for these plot types
      
      summ <- df %>%
        group_by(.data[[grp2]], side) %>%
        summarise(
          mean = mean(mean_fb_distance, na.rm = TRUE),
          sd   = sd(mean_fb_distance, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(group = 1)
      
      # ✅ set factor order correctly for genotype / treatment / geno_trt
      if (grp2 == "genotype") {
        df   <- df   %>% mutate(genotype = factor(as.character(genotype), levels = geno_pal$levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        df   <- df   %>% mutate(treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)), levels = treat_pal$levels))
        summ <- summ %>% mutate(group     = factor(as.character(group),     levels = treat_pal$levels))
      } else if (grp2 == "geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }
      
      p <- ggplot(df, aes(x = .data[[grp2]], y = mean_fb_distance, fill = .data[[grp2]])) +
        geom_col(
          data = summ,
          aes(x = group, y = mean, fill = group),
          colour = "grey20",
          position = position_dodge(width = 0.85),
          inherit.aes = FALSE
        ) +
        geom_errorbar(
          data = summ,
          aes(x = group, ymin = mean - sd, ymax = mean + sd),
          width = 0.2,
          position = position_dodge(width = 0.85),
          inherit.aes = FALSE
        ) +
        geom_point(position = position_jitter(width = 0.12),
                   size = 2, alpha = 0.85, show.legend = FALSE) +
        facet_wrap(~side) +
        ggprism::theme_prism(base_size = 14, base_family = "Arial") +
        labs(title = "FB distance per side (mean per mouse)", x = grp2, y = "Mean FB distance (cm)")
      
      # ✅ correct fill scales
      if (grp2 == "genotype" && length(geno_pal$colors) > 0) {
        p <- p + scale_fill_manual(values = geno_pal$colors, drop = FALSE)
      }
      if (grp2 == "treatment" && length(treat_pal$colors) > 0) {
        p <- p + scale_fill_manual(values = treat_pal$colors, drop = FALSE)
      }
      if (grp2 == "geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values = fills, drop = FALSE)
      }
      
      return(p)
    }
    
    # --- per segment (perpendicular) ---
    if (input$plot_type == "seg_perp") {
      df <- paws_rot()
      meta <- df %>% distinct(mouse_id, genotype, treatment, mouse_length_cm)
      
      seg_df <- df %>%
        filter(!is.na(x_along_cm), !is.na(y_perp_cm), segment %in% c("Front","Hind")) %>%
        group_by(mouse_id, image_id, segment) %>%
        group_modify(~ perpendicular_events(.x)) %>%
        ungroup() %>%
        group_by(mouse_id, segment) %>%
        summarise(mean_perpendicular = mean(perpendicular_dist_cm, na.rm=TRUE), .groups="drop") %>%
        left_join(meta, by="mouse_id")

      # optional length normalization (per mouse)
      if (isTRUE(input$norm_length)) {
        seg_df <- seg_df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                                    mean_perpendicular = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                                                 mean_perpendicular / mouse_length_cm, NA_real_))
      }

      gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
      
      seg_df <- seg_df %>%
        mutate(
          genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
          treatment = if_else(is.na(treatment) | treatment == "", "NA", as.character(treatment)),
          treatment = factor(treatment, levels = treat_pal$levels),
          geno_trt  = factor(paste0(as.character(genotype), "_", as.character(treatment)),
                             levels = gt_levels)
        )
      

      
      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"  # sensible fallback for these plot types
      
      summ <- seg_df %>%
        group_by(.data[[grp2]], segment) %>%
        summarise(
          mean = mean(mean_perpendicular, na.rm = TRUE),
          sd   = sd(mean_perpendicular, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(group = 1)
      
      # ✅ set factor order correctly for genotype / treatment / geno_trt
      if (grp2 == "genotype") {
        seg_df <- seg_df %>% mutate(genotype = factor(as.character(genotype), levels = geno_pal$levels))
        summ   <- summ   %>% mutate(group    = factor(as.character(group),    levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        seg_df <- seg_df %>% mutate(treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)), levels = treat_pal$levels))
        summ   <- summ   %>% mutate(group     = factor(as.character(group),     levels = treat_pal$levels))
      } else if (grp2 == "geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        seg_df <- seg_df %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ   <- summ   %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }
      
      p <- ggplot(seg_df, aes(x = .data[[grp2]], y = mean_perpendicular, fill = .data[[grp2]])) +
        geom_col(
          data = summ,
          aes(x = group, y = mean, fill = group),
          colour = "grey20",
          position = position_dodge(width = 0.85),
          inherit.aes = FALSE
        ) +
        geom_errorbar(
          data = summ,
          aes(x = group, ymin = mean - sd, ymax = mean + sd),
          width = 0.2,
          position = position_dodge(width = 0.85),
          inherit.aes = FALSE
        ) +
        geom_point(position = position_jitter(width = 0.12),
                   size = 2, alpha = 0.85, show.legend = FALSE) +
        facet_wrap(~segment) +
        ggprism::theme_prism(base_size = 14, base_family = "Arial") +
        labs(title = "Perpendicular deviation per segment (mean per mouse)", x = grp2, y = "Mean perpendicular (cm)")
      
      # ✅ correct fill scales
      if (grp2 == "genotype" && length(geno_pal$colors) > 0) {
        p <- p + scale_fill_manual(values = geno_pal$colors, drop = FALSE)
      }
      if (grp2 == "treatment" && length(treat_pal$colors) > 0) {
        p <- p + scale_fill_manual(values = treat_pal$colors, drop = FALSE)
      }
      if (grp2 == "geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values = fills, drop = FALSE)
      }
      
      return(p)
      
    }

    # --- base of support (L-R) ---
    if (input$plot_type == "bos") {
      df <- features_mouse() %>%
        select(mouse_id, genotype, treatment, mouse_length_cm, mean_bos_Front, mean_bos_Hind) %>%
        pivot_longer(cols = c(mean_bos_Front, mean_bos_Hind),
                     names_to = "segment",
                     values_to = "value") %>%
        mutate(segment = recode(segment,
                                mean_bos_Front = "Front",
                                mean_bos_Hind  = "Hind"))

      if (isTRUE(input$norm_length)) {
        df <- df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                            value = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                            value / mouse_length_cm, NA_real_))
        ylab <- "Base of support / body length"
      } else {
        ylab <- "Base of support (cm)"
      }

      df <- df %>% mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels)
      )

      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"

      # summary per group x segment
      summ <- df %>%
        group_by(.data[[grp2]], segment) %>%
        summarise(mean = mean(value, na.rm=TRUE),
                  sd   = sd(value, na.rm=TRUE),
                  .groups="drop") %>%
        rename(group = 1)

      # set factor order for x axis
      if (grp2 == "genotype") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = treat_pal$levels))
      } else {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }

      p <- ggplot(df, aes(x=.data[[grp2]], y=value, fill=.data[[grp2]])) +
        geom_col(data=summ, aes(x=group, y=mean, fill=group),
                 colour="grey20", position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_errorbar(data=summ, aes(x=group, ymin=mean-sd, ymax=mean+sd),
                      width=0.2, position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_point(position=position_jitter(width=0.12), size=2, alpha=0.85, show.legend=FALSE) +
        facet_wrap(~segment) +
        ggprism::theme_prism(base_size=14, base_family="Arial") +
        labs(title="Base of support (L-R) per segment (mean per mouse)", x=grp2, y=ylab)

      if (grp2=="genotype" && length(geno_pal$colors)>0) p <- p + scale_fill_manual(values=geno_pal$colors, drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0) p <- p + scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values=fills, drop=FALSE)
      }
      return(p)
    }

    # --- paw overlap (hind vs front) ---
    if (input$plot_type == "overlap") {
      df <- features_mouse() %>%
        select(mouse_id, genotype, treatment, mouse_length_cm, mean_overlap_abs)

      if (isTRUE(input$norm_length)) {
        df <- df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                            mean_overlap_abs = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                                       mean_overlap_abs / mouse_length_cm, NA_real_))
        ylab <- "Hind–front overlap / body length"
      } else {
        ylab <- "Hind–front overlap (cm)"
      }

      df <- df %>% mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels)
      )

      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"

      summ <- df %>%
        group_by(.data[[grp2]]) %>%
        summarise(mean = mean(mean_overlap_abs, na.rm=TRUE),
                  sd   = sd(mean_overlap_abs, na.rm=TRUE),
                  .groups="drop") %>%
        rename(group = 1)

      if (grp2 == "genotype") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = treat_pal$levels))
      } else {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }

      p <- ggplot(df, aes(x=.data[[grp2]], y=mean_overlap_abs, fill=.data[[grp2]])) +
        geom_col(data=summ, aes(x=group, y=mean, fill=group),
                 colour="grey20", inherit.aes=FALSE) +
        geom_errorbar(data=summ, aes(x=group, ymin=mean-sd, ymax=mean+sd),
                      width=0.2, inherit.aes=FALSE) +
        geom_point(position=position_jitter(width=0.12), size=2, alpha=0.85, show.legend=FALSE) +
        ggprism::theme_prism(base_size=14, base_family="Arial") +
        labs(title="Paw overlap (hind vs front) (mean per mouse)", x=grp2, y=ylab)

      if (grp2=="genotype" && length(geno_pal$colors)>0) p <- p + scale_fill_manual(values=geno_pal$colors, drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0) p <- p + scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values=fills, drop=FALSE)
      }
      return(p)
    }

    # --- per paw step length ---
    if (input$plot_type == "paw_step") {
      df <- features_mouse() %>%
        select(mouse_id, genotype, treatment, mouse_length_cm,
               starts_with("mean_step_length_paw_")) %>%
        pivot_longer(cols = starts_with("mean_step_length_paw_"),
                     names_to = "paw",
                     values_to = "value") %>%
        mutate(paw = sub("mean_step_length_paw_", "", paw),
               paw = factor(paw, levels=c("FL","FR","HL","HR")))

      if (isTRUE(input$norm_length)) {
        df <- df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                            value = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                            value / mouse_length_cm, NA_real_))
        ylab <- "Step length / body length"
      } else {
        ylab <- "Step length (cm)"
      }

      df <- df %>% mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels)
      )

      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"

      summ <- df %>%
        group_by(.data[[grp2]], paw) %>%
        summarise(mean = mean(value, na.rm=TRUE),
                  sd   = sd(value, na.rm=TRUE),
                  .groups="drop") %>%
        rename(group = 1)

      if (grp2 == "genotype") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = treat_pal$levels))
      } else {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }

      p <- ggplot(df, aes(x=.data[[grp2]], y=value, fill=.data[[grp2]])) +
        geom_col(data=summ, aes(x=group, y=mean, fill=group),
                 colour="grey20", position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_errorbar(data=summ, aes(x=group, ymin=mean-sd, ymax=mean+sd),
                      width=0.2, position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_point(position=position_jitter(width=0.12), size=2, alpha=0.85, show.legend=FALSE) +
        facet_wrap(~paw, ncol=2) +
        ggprism::theme_prism(base_size=14, base_family="Arial") +
        labs(title="Per-paw step length (mean per mouse)", x=grp2, y=ylab)

      if (grp2=="genotype" && length(geno_pal$colors)>0) p <- p + scale_fill_manual(values=geno_pal$colors, drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0) p <- p + scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values=fills, drop=FALSE)
      }
      return(p)
    }

    # --- left-right asymmetry indices ---
    if (input$plot_type == "asym") {
      df <- features_mouse() %>%
        select(mouse_id, genotype, treatment, asym_step_front, asym_step_hind) %>%
        pivot_longer(cols = starts_with("asym_step_"),
                     names_to = "limb_set",
                     values_to = "asym") %>%
        mutate(limb_set = recode(limb_set,
                                 asym_step_front = "Front",
                                 asym_step_hind  = "Hind"))

      df <- df %>% mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels)
      )

      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"

      summ <- df %>%
        group_by(.data[[grp2]], limb_set) %>%
        summarise(mean = mean(asym, na.rm=TRUE),
                  sd   = sd(asym, na.rm=TRUE),
                  .groups="drop") %>%
        rename(group = 1)

      if (grp2 == "genotype") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = treat_pal$levels))
      } else {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }

      p <- ggplot(df, aes(x=.data[[grp2]], y=asym, fill=.data[[grp2]])) +
        geom_col(data=summ, aes(x=group, y=mean, fill=group),
                 colour="grey20", position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_errorbar(data=summ, aes(x=group, ymin=mean-sd, ymax=mean+sd),
                      width=0.2, position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_point(position=position_jitter(width=0.12), size=2, alpha=0.85, show.legend=FALSE) +
        facet_wrap(~limb_set) +
        ggprism::theme_prism(base_size=14, base_family="Arial") +
        labs(title="Left–right asymmetry index (mean per mouse)", x=grp2, y="Asymmetry index (0 = symmetric)")

      if (grp2=="genotype" && length(geno_pal$colors)>0) p <- p + scale_fill_manual(values=geno_pal$colors, drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0) p <- p + scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values=fills, drop=FALSE)
      }
      return(p)
    }

    # --- drift / stability from y_perp ---
    if (input$plot_type == "drift") {
      df <- features_mouse() %>%
        select(mouse_id, genotype, treatment, mouse_length_cm,
               mean_abs_drift_y, sd_drift_y, range_drift_y) %>%
        pivot_longer(cols = c(mean_abs_drift_y, sd_drift_y, range_drift_y),
                     names_to = "metric",
                     values_to = "value") %>%
        mutate(metric = recode(metric,
                               mean_abs_drift_y = "Mean |y_perp|",
                               sd_drift_y       = "SD(y_perp)",
                               range_drift_y    = "Range(y_perp)"))

      if (isTRUE(input$norm_length)) {
        df <- df %>% mutate(mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm)),
                            value = if_else(is.finite(mouse_length_cm) & mouse_length_cm > 0,
                                            value / mouse_length_cm, NA_real_))
        ylab <- "y_perp metric / body length"
      } else {
        ylab <- "y_perp metric (cm)"
      }

      df <- df %>% mutate(
        genotype  = factor(as.character(genotype),  levels = geno_pal$levels),
        treatment = factor(if_else(is.na(treatment) | treatment=="", "NA", as.character(treatment)),
                           levels = treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment, geno_pal$levels, treat_pal$levels)
      )

      grp2 <- input$color_by
      if (grp2 == "genotype+treatment") grp2 <- "geno_trt"
      if (grp2 == "none") grp2 <- "genotype"

      summ <- df %>%
        group_by(.data[[grp2]], metric) %>%
        summarise(mean = mean(value, na.rm=TRUE),
                  sd   = sd(value, na.rm=TRUE),
                  .groups="drop") %>%
        rename(group = 1)

      if (grp2 == "genotype") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = geno_pal$levels))
      } else if (grp2 == "treatment") {
        summ <- summ %>% mutate(group = factor(as.character(group), levels = treat_pal$levels))
      } else {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        df   <- df   %>% mutate(geno_trt = factor(as.character(geno_trt), levels = gt_levels))
        summ <- summ %>% mutate(group    = factor(as.character(group),    levels = gt_levels))
      }

      p <- ggplot(df, aes(x=.data[[grp2]], y=value, fill=.data[[grp2]])) +
        geom_col(data=summ, aes(x=group, y=mean, fill=group),
                 colour="grey20", position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_errorbar(data=summ, aes(x=group, ymin=mean-sd, ymax=mean+sd),
                      width=0.2, position=position_dodge(width=0.85), inherit.aes=FALSE) +
        geom_point(position=position_jitter(width=0.12), size=2, alpha=0.85, show.legend=FALSE) +
        facet_wrap(~metric, scales="free_y") +
        ggprism::theme_prism(base_size=14, base_family="Arial") +
        labs(title="Drift / stability metrics from y_perp (mean per mouse)", x=grp2, y=ylab)

      if (grp2=="genotype" && length(geno_pal$colors)>0) p <- p + scale_fill_manual(values=geno_pal$colors, drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0) p <- p + scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p + scale_fill_manual(values=fills, drop=FALSE)
      }
      return(p)
    }


    NULL
  })
  
  output$plot <- renderPlot({
    req(plot_obj())
    plot_obj()
  }, height = 520)
  
}

shinyApp(ui, server)

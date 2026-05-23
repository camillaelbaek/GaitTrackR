# app.R — GaitTrackR  (3-tab restructure)
# Tab 1: Image → Data   (image annotation module, unchanged)
# Tab 2: Feature Calculation (upload, metadata, processing, export)
# Tab 3: Plotting (all plot types, palettes, download)

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

source("./image_module_ui.R")
source("./image_module_server.R")


# ── default palettes ──────────────────────────────────────────────────────────
default_geno_palette  <- c(wt="#4D4D4D", het="#a08679", ko="#D95F02")
default_treat_palette <- c(`NA`="#999999", vehicle="#1B9E77", drug="#E41A1C")
paw_cols <- c(front_left="#e31a1c", front_right="#fb9a99",
              hind_left="#1f78b4",  hind_right="#a6cee3")

palette_to_text <- function(p) {
  paste(paste0(names(p), " = ", unname(p)), collapse = "\n")
}

parse_palette_text <- function(txt) {
  lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  kv  <- strsplit(lines, "=", fixed = TRUE)
  nm  <- trimws(vapply(kv, `[[`, character(1), 1))
  val <- trimws(vapply(kv, function(x) paste(x[-1], collapse="="), character(1)))
  ok  <- nzchar(nm) & nzchar(val)
  nm  <- nm[ok]; val <- val[ok]
  keep <- !duplicated(nm); nm <- nm[keep]; val <- val[keep]
  pal  <- val; names(pal) <- nm
  list(levels = nm, colors = pal)
}

parse_exclude_ids <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(character(0))
  ids <- trimws(unlist(strsplit(txt, "[,\\s]+")))
  unique(ids[nzchar(ids)])
}

make_geno_trt_levels <- function(geno_levels, treat_levels) {
  eg <- expand.grid(genotype  = geno_levels,
                    treatment = treat_levels,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  paste0(eg$genotype, "_", eg$treatment)
}

make_geno_trt_factor <- function(genotype, treatment, geno_levels, treat_levels) {
  gt <- paste0(as.character(genotype), "_", as.character(treatment))
  factor(gt, levels = make_geno_trt_levels(geno_levels, treat_levels))
}

geno_trt_fill_fallback <- function(geno_trt_levels, geno_pal, treat_levels) {
  a <- seq(1.0, 0.35, length.out = max(1, length(treat_levels)))
  names(a) <- treat_levels
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


# ── gait helpers ──────────────────────────────────────────────────────────────
straighten_mouse <- function(df) {
  dfc <- df %>% filter(!is.na(x), !is.na(y))
  if (nrow(dfc) < 2) return(df %>% mutate(x_along_cm=NA_real_, y_perp_cm=NA_real_))
  m  <- lm(y ~ x, data=dfc); b <- coef(m)[2]
  uF <- c(1,b)/sqrt(1+b^2); uP <- c(-b,1)/sqrt(1+b^2)
  mx <- mean(dfc$x); my <- mean(dfc$y)
  xc <- df$x - mx;   yc <- df$y - my
  s  <- unique(df$pixels_per_cm); if (length(s)!=1 || is.na(s)) s <- 1
  df %>% mutate(x_along_cm = (xc*uF[1]+yc*uF[2])/s,
                y_perp_cm  = (xc*uP[1]+yc*uP[2])/s)
}

assign_steps <- function(df_side, max_gap_cm = 2) {
  F <- df_side %>% filter(segment=="Front") %>% arrange(x_along_cm)
  H <- df_side %>% filter(segment=="Hind")  %>% arrange(x_along_cm)
  nF <- nrow(F); nH <- nrow(H)
  if (nF==0 && nH==0)
    return(tibble(dot_id=integer(), true_step_id=integer(), segment=character()))

  if (nF>0 && nH>0 && nF==nH) {
    return(bind_rows(
      F %>% mutate(true_step_id=row_number()) %>% select(dot_id, true_step_id, segment),
      H %>% mutate(true_step_id=row_number()) %>% select(dot_id, true_step_id, segment)
    ))
  }

  i <- 1; j <- 1; step <- 1; out <- list()
  while (i<=nF || j<=nH) {
    fx <- if (i<=nF) F$x_along_cm[i] else Inf
    hx <- if (j<=nH) H$x_along_cm[j] else Inf
    if (is.finite(fx) && is.finite(hx) && abs(fx-hx) <= max_gap_cm) {
      out[[length(out)+1]] <- tibble(dot_id=c(F$dot_id[i],H$dot_id[j]),
                                     true_step_id=step, segment=c("Front","Hind"))
      i <- i+1; j <- j+1
    } else if (fx < hx) {
      if (is.finite(fx)) out[[length(out)+1]] <- tibble(
        dot_id=F$dot_id[i], true_step_id=step, segment="Front")
      i <- i+1
    } else {
      if (is.finite(hx)) out[[length(out)+1]] <- tibble(
        dot_id=H$dot_id[j], true_step_id=step, segment="Hind")
      j <- j+1
    }
    step <- step+1
  }
  bind_rows(out)
}

perpendicular_events <- function(df_segment) {
  d <- df_segment %>%
    dplyr::filter(is.finite(x_along_cm), is.finite(y_perp_cm), side %in% c("L","R")) %>%
    dplyr::arrange(x_along_cm)
  if (nrow(d) < 3)
    return(tibble::tibble(segment=character(), ref_side=character(),
                          perpendicular_dist_cm=numeric()))
  L <- d %>% dplyr::filter(side=="L") %>% dplyr::arrange(x_along_cm)
  R <- d %>% dplyr::filter(side=="R") %>% dplyr::arrange(x_along_cm)
  p2l <- function(x0,y0,x1,y1,x2,y2) {
    num <- abs((x2-x1)*(y1-y0)-(x1-x0)*(y2-y1))
    den <- sqrt((x2-x1)^2+(y2-y1)^2)
    ifelse(den>0, num/den, NA_real_)
  }
  out <- list()
  calc_ref <- function(A, B, ref_label) {
    if (nrow(A)<2 || nrow(B)<1) return(NULL)
    for (i in 1:(nrow(A)-1)) {
      x1<-A$x_along_cm[i]; y1<-A$y_perp_cm[i]
      x2<-A$x_along_cm[i+1]; y2<-A$y_perp_cm[i+1]
      if (!is.finite(x1)||!is.finite(x2)||x2<=x1) next
      cand <- B %>% dplyr::filter(x_along_cm>x1, x_along_cm<x2)
      if (nrow(cand)<1) next
      j  <- which.min(abs(cand$x_along_cm-(x1+x2)/2))
      x0 <- cand$x_along_cm[j]; y0 <- cand$y_perp_cm[j]
      out[[length(out)+1]] <<- tibble::tibble(
        segment=unique(d$segment)[1], ref_side=ref_label,
        perpendicular_dist_cm=p2l(x0,y0,x1,y1,x2,y2))
    }
  }
  calc_ref(L,R,"L"); calc_ref(R,L,"R")
  dplyr::bind_rows(out) %>% dplyr::filter(is.finite(perpendicular_dist_cm))
}


# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .section-header {
      font-size: 13px; font-weight: 600; color: #37474f;
      text-transform: uppercase; letter-spacing: 0.04em;
      margin: 14px 0 6px 0; padding-bottom: 4px;
      border-bottom: 1px solid #e0e0e0;
    }
    .detect-ok   { color:#2e7d32; font-size:12px; }
    .detect-warn { color:#e65100; font-size:12px; }
    .detect-na   { color:#9e9e9e; font-size:12px; }
    .group-chip {
      display:inline-block; padding:2px 8px; border-radius:10px;
      font-size:11px; margin:2px; background:#eceff1; color:#37474f;
    }
  "))),
  titlePanel("GaitTrackR — Gait features from paw prints"),

  tabsetPanel(

    # ── TAB 1: Image annotation ──────────────────────────────────────────────
    tabPanel("\U0001F5BC\ufe0f  Image \u2192 Data",
      br(),
      imageAnnotationUI()
    ),

    # ── TAB 2: Feature Calculation ───────────────────────────────────────────
    tabPanel("\u2699\ufe0f  Feature Calculation",
      br(),
      sidebarLayout(
        sidebarPanel(

          # — Upload —
          div(class="section-header", "\U0001F4C2  Data"),
          fileInput("file", NULL, accept=".xlsx",
                    placeholder="Upload paw data Excel (.xlsx)"),

          # — Processing —
          div(class="section-header", "\u2699\ufe0f  Processing"),
          checkboxInput("align", "Straighten tracks (alignment)", FALSE),
          numericInput("max_gap", "Step pairing max gap (cm)", 2, min=0, step=0.1),
          checkboxInput("norm_length",
            "Normalise distances by body length", FALSE),
          tags$small("Requires a mouse length column to be mapped below.",
                     style="color:#9e9e9e;"),

          # — Metadata —
          div(class="section-header", "\U0001F3F7\ufe0f  Metadata"),
          radioButtons("meta_mode", NULL,
            choices  = c("Map columns from file"="cols",
                         "Enter manually"       ="manual"),
            selected = "cols"),
          uiOutput("meta_cols_ui"),
          uiOutput("group_summary_ui"),

          # — Exclusions —
          div(class="section-header", "\U0001F6AB  Exclude mice"),
          tags$small("Comma- or space-separated mouse_id values."),
          textAreaInput("exclude_ids", NULL, value="", rows=2),

          # — Export —
          div(class="section-header", "\u2B07\ufe0f  Export"),
          downloadButton("download_features",
            "Mouse-level features (.xlsx)",
            style="width:100%; margin-bottom:6px;"),
          br(),
          downloadButton("download_steps",
            "Step-level tables (.xlsx)",
            style="width:100%;")
        ),

        mainPanel(
          h4("Data preview"),
          DTOutput("preview"),
          conditionalPanel("input.meta_mode=='manual'",
            hr(),
            h4("Manual genotype / treatment table"),
            tags$small("Edit cells directly. Genotype and treatment values here
                        must match the palette names in the Plotting tab.",
                       style="color:#9e9e9e;"),
            br(), br(),
            DTOutput("meta_table")
          ),
          hr(),
          h4("Mouse-level features"),
          DTOutput("features_table")
        )
      )
    ), # end Tab 2

    # ── TAB 3: Plotting ──────────────────────────────────────────────────────
    tabPanel("\U0001F4CA  Plotting",
      br(),
      sidebarLayout(
        sidebarPanel(

          div(class="section-header", "\U0001F3A8  Appearance"),
          selectInput("color_by", "Color by",
            choices  = c("genotype","treatment","genotype+treatment","none"),
            selected = "genotype"),

          div(class="section-header", "\U0001F4CA  Plot"),
          selectInput("plot_type", "Plot type", choices = c(
            "— Overview —"                            = "overview_bubble",
            "Mean \u00b1 SD (bar)"                    = "mean_sd",
            "Mean CV \u00b1 SD (bar)"                 = "cv_sd",
            "Per side (FB distance)"                  = "side_fb",
            "Per segment (Perpendicular)"             = "seg_perp",
            "Base of support (L\u2013R)"              = "bos",
            "Paw overlap (hind vs front)"             = "overlap",
            "Per paw stride length"                   = "paw_step",
            "Left\u2013right asymmetry index"         = "asym",
            "Drift / stability (y_perp)"              = "drift",
            "QC tracks (along vs perp)"               = "qc_tracks"
          ), selected="overview_bubble"),

          # Overview controls
          conditionalPanel("input.plot_type=='overview_bubble'",
            uiOutput("overview_ref_ui"),
            checkboxInput("show_significance",
              "Show significance (Wilcoxon, uncorrected)", value=FALSE),
            tags$small("\u26A0 Exploratory only \u2014 not corrected for multiple comparisons.",
                       style="color:#e65100;")
          ),

          # Mean / CV controls
          conditionalPanel(
            "input.plot_type=='mean_sd' || input.plot_type=='cv_sd'",
            selectInput("measure", "Measurement", choices=c(
              "Stride length (same paw)"   = "step",
              "Front\u2013hind dist. (2D)" = "fb2d",
              "Front\u2013hind dist. (x)"  = "fbx",
              "Perpendicular deviation"    = "perp"
            ), selected="step")
          ),

          # FB mode
          conditionalPanel("input.plot_type=='side_fb'",
            selectInput("fb_mode", "FB distance mode", choices=c(
              "2D distance (x and y)"  = "fb2d",
              "x-only distance"        = "fbx"
            ), selected="fb2d")
          ),

          # QC controls
          conditionalPanel("input.plot_type=='qc_tracks'",
            selectInput("qc_mouse", "QC mouse_id", choices=character(0)),
            checkboxInput("qc_show_labels", "Show dot_id labels", TRUE)
          ),

          # — Palettes —
          div(class="section-header", "\U0001F3A8  Palettes"),
          tags$small("name = hex, one per line. Order = factor order in plots.",
                     style="color:#9e9e9e;"),
          br(), br(),
          tags$b("Genotype"),
          textAreaInput("geno_pal", NULL, palette_to_text(default_geno_palette), rows=4),
          tags$b("Treatment"),
          textAreaInput("treat_pal", NULL, palette_to_text(default_treat_palette), rows=5),

          # — Download —
          div(class="section-header", "\u2B07\ufe0f  Download plot"),
          selectInput("plot_format", NULL,
            choices=c("png","pdf","svg","jpeg"), selected="png"),
          downloadButton("download_plot", "Download current plot",
                         style="width:100%;")
        ),

        mainPanel(
          uiOutput("plot_status_ui"),
          plotOutput("plot", height=520)
        )
      )
    ) # end Tab 3

  ) # end tabsetPanel
) # end fluidPage


# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  imageAnnotationServer(input, output, session)

  # ── Raw data ----------------------------------------------------------------
  raw_df <- reactive({
    req(input$file)
    readxl::read_xlsx(input$file$datapath)
  })

  output$preview <- renderDT({
    req(raw_df())
    datatable(head(raw_df(), 50), options=list(scrollX=TRUE, pageLength=8))
  })

  # ── Metadata column UI (Tab 2) ----------------------------------------------
  output$meta_cols_ui <- renderUI({
    req(raw_df(), input$meta_mode == "cols")
    cols <- names(raw_df())

    detect_badge <- function(col_name) {
      if (col_name %in% cols)
        tags$span(paste0("\u2713 '", col_name, "' detected"), class="detect-ok")
      else
        tags$span(paste0("\u26a0 no '", col_name, "' column"), class="detect-warn")
    }

    tagList(
      fluidRow(
        column(8, selectInput("geno_col", "Genotype column",
          choices  = c("None", cols),
          selected = if ("genotype" %in% cols) "genotype" else "None")),
        column(4, br(), br(), detect_badge("genotype"))
      ),
      fluidRow(
        column(8, selectInput("treat_col", "Treatment column",
          choices  = c("None", cols),
          selected = if ("treatment" %in% cols) "treatment" else "None")),
        column(4, br(), br(), detect_badge("treatment"))
      ),
      fluidRow(
        column(8, selectInput("length_col", "Mouse length column (cm)",
          choices  = c("None", cols),
          selected = if ("mouse_length_cm" %in% cols) "mouse_length_cm" else "None")),
        column(4, br(), br(),
          if ("mouse_length_cm" %in% cols)
            tags$span("\u2713 detected", class="detect-ok")
          else
            tags$span("optional", class="detect-na"))
      )
    )
  })

  # ── Group summary chips (Tab 2) ---------------------------------------------
  output$group_summary_ui <- renderUI({
    req(raw_df())
    df <- raw_df()

    geno_col  <- if (!is.null(input$geno_col)  && input$geno_col  != "None") input$geno_col  else NULL
    treat_col <- if (!is.null(input$treat_col) && input$treat_col != "None") input$treat_col else NULL

    if (is.null(geno_col) && is.null(treat_col)) return(NULL)
    if (!"mouse_id" %in% names(df)) return(NULL)

    mice <- df %>% select(mouse_id,
                          any_of(c(geno_col, treat_col))) %>%
                   distinct()

    chips <- tagList()

    if (!is.null(geno_col) && geno_col %in% names(mice)) {
      counts <- table(mice[[geno_col]])
      chips  <- tagList(chips,
        tags$div(style="margin-top:6px;",
          tags$small("Genotype:", style="color:#9e9e9e;"),
          lapply(names(counts), function(g)
            tags$span(paste0(g, " (n=", counts[[g]], ")"), class="group-chip"))
        ))
    }
    if (!is.null(treat_col) && treat_col %in% names(mice)) {
      counts <- table(mice[[treat_col]])
      chips  <- tagList(chips,
        tags$div(style="margin-top:4px;",
          tags$small("Treatment:", style="color:#9e9e9e;"),
          lapply(names(counts), function(t)
            tags$span(paste0(t, " (n=", counts[[t]], ")"), class="group-chip"))
        ))
    }
    chips
  })

  # ── Manual metadata --------------------------------------------------------
  excluded_ids <- reactive({ parse_exclude_ids(input$exclude_ids) })

  meta_manual <- reactiveVal(NULL)
  observeEvent(raw_df(), {
    df    <- raw_df()
    validate(need("mouse_id" %in% names(df), "Excel must contain a 'mouse_id' column."))
    keep  <- setdiff(unique(df$mouse_id), excluded_ids())
    meta_manual(
      tibble(mouse_id=keep) %>% arrange(mouse_id) %>%
        mutate(genotype=NA_character_, treatment=NA_character_, mouse_length_cm=NA_real_)
    )
  })

  output$meta_table <- renderDT({
    req(meta_manual())
    datatable(meta_manual(), editable=TRUE,
              options=list(pageLength=10, scrollX=TRUE))
  })
  observeEvent(input$meta_table_cell_edit, {
    info <- input$meta_table_cell_edit
    df   <- meta_manual()
    df[info$row, info$col] <- info$value
    meta_manual(df)
  })

  # ── QC mouse selector (Tab 3) -----------------------------------------------
  observeEvent(paws_rot(), {
    ids <- sort(unique(paws_rot()$mouse_id))
    updateSelectInput(session, "qc_mouse", choices=ids, selected=ids[1])
  }, ignoreInit=TRUE)

  # ── Core data ---------------------------------------------------------------
  paws <- reactive({
    df      <- raw_df()
    needed  <- c("mouse_id","paw","x","y","dot_id")
    validate(need(all(needed %in% names(df)),
      paste("Missing columns:", paste(setdiff(needed, names(df)), collapse=", "))))

    ex <- excluded_ids()
    if (length(ex) > 0) df <- df %>% filter(!mouse_id %in% ex)

    if (!"image_id"      %in% names(df)) df <- df %>% mutate(image_id=1L)
    if (!"pixels_per_cm" %in% names(df)) df <- df %>% mutate(pixels_per_cm=1)

    if (input$meta_mode == "cols") {
      geno <- if (!is.null(input$geno_col)  && input$geno_col  != "None" && input$geno_col  %in% names(df)) df[[input$geno_col]]  else NA
      trt  <- if (!is.null(input$treat_col) && input$treat_col != "None" && input$treat_col %in% names(df)) df[[input$treat_col]] else NA
      lenv <- if (!is.null(input$length_col)&& input$length_col!= "None" && input$length_col%in% names(df)) df[[input$length_col]]else NA
      df   <- df %>% mutate(genotype=geno, treatment=trt, mouse_length_cm=lenv)
    } else {
      req(meta_manual())
      df <- df %>% left_join(meta_manual(), by="mouse_id")
    }

    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)

    df <- df %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels=geno_pal$levels),
        treatment = factor(if_else(is.na(treatment)|treatment=="", "NA", as.character(treatment)),
                           levels=treat_pal$levels),
        geno_trt  = make_geno_trt_factor(genotype, treatment,
                                         geno_pal$levels, treat_pal$levels),
        mouse_length_cm = suppressWarnings(as.numeric(mouse_length_cm))
      )

    df %>%
      mutate(
        paw     = factor(paw, levels=c("front_left","front_right","hind_left","hind_right")),
        side    = if_else(grepl("left$", as.character(paw)), "L", "R"),
        segment = if_else(grepl("^front", as.character(paw)), "Front", "Hind")
      ) %>%
      group_by(mouse_id, image_id, paw) %>%
      arrange(dot_id, .by_group=TRUE) %>%
      mutate(step_id=row_number()) %>%
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
          d  <- .x; dc <- d %>% filter(!is.na(x), !is.na(y))
          if (nrow(dc)<1) return(d %>% mutate(x_along_cm=NA_real_, y_perp_cm=NA_real_))
          mx <- mean(dc$x); my <- mean(dc$y)
          s  <- unique(d$pixels_per_cm); if (length(s)!=1||is.na(s)) s <- 1
          d %>% mutate(x_along_cm=(x-mx)/s, y_perp_cm=(y-my)/s)
        }
      }) %>% ungroup()

    ts <- out %>%
      filter(!is.na(dot_id), !is.na(x_along_cm)) %>%
      group_by(mouse_id, image_id, side) %>%
      group_modify(~assign_steps(.x, max_gap_cm=input$max_gap)) %>%
      ungroup()

    out %>% left_join(ts, by=c("mouse_id","image_id","side","segment","dot_id"))
  })

  # ── Feature calculation ─────────────────────────────────────────────────────
  features_mouse <- reactive({
    df <- paws_rot()

    # Stride length (same paw)
    stride_df <- df %>%
      filter(!is.na(x_along_cm)) %>%
      arrange(mouse_id, image_id, paw, x_along_cm) %>%
      group_by(mouse_id, image_id, paw) %>%
      mutate(stride_length_cm=x_along_cm-lag(x_along_cm)) %>%
      ungroup() %>% filter(!is.na(stride_length_cm))

    stride_mouse <- stride_df %>%
      group_by(mouse_id) %>%
      summarise(mean_stride_length=mean(stride_length_cm, na.rm=TRUE),
                cv_stride_length=sd(stride_length_cm,na.rm=TRUE)/abs(mean_stride_length),
                .groups="drop")

    stride_paw_mouse <- stride_df %>%
      group_by(mouse_id, paw) %>%
      summarise(mean_stride_length_paw=mean(stride_length_cm,na.rm=TRUE),
                sd_stride_length_paw=sd(stride_length_cm,na.rm=TRUE),
                cv_stride_length_paw=sd_stride_length_paw/abs(mean_stride_length_paw),
                .groups="drop") %>%
      tidyr::pivot_wider(names_from=paw,
                         values_from=c(mean_stride_length_paw,cv_stride_length_paw),
                         names_sep="_")

    # Alternating step length (L→R / R→L)
    step_alt_df <- df %>%
      filter(!is.na(x_along_cm)) %>%
      arrange(mouse_id, image_id, segment, x_along_cm) %>%
      group_by(mouse_id, image_id, segment) %>%
      mutate(prev_side=lag(side), prev_x=lag(x_along_cm),
             is_alt=!is.na(prev_side)&side!=prev_side) %>%
      filter(is_alt) %>%
      mutate(alt_step_cm=x_along_cm-prev_x) %>% ungroup()

    alt_step_mouse <- step_alt_df %>%
      group_by(mouse_id, segment) %>%
      summarise(mean_step_length=mean(alt_step_cm,na.rm=TRUE),
                cv_step_length=sd(alt_step_cm,na.rm=TRUE)/abs(mean(alt_step_cm,na.rm=TRUE)),
                .groups="drop") %>%
      tidyr::pivot_wider(names_from=segment,
                         values_from=c(mean_step_length,cv_step_length), names_sep="_")

    # L-R symmetry index
    symmetry_mouse <- stride_paw_mouse %>%
      mutate(
        symmetry_front=ifelse(
          is.finite(mean_stride_length_paw_front_left)&is.finite(mean_stride_length_paw_front_right),
          abs(mean_stride_length_paw_front_left-mean_stride_length_paw_front_right)/
            (0.5*(abs(mean_stride_length_paw_front_left)+abs(mean_stride_length_paw_front_right))),
          NA_real_),
        symmetry_hind=ifelse(
          is.finite(mean_stride_length_paw_hind_left)&is.finite(mean_stride_length_paw_hind_right),
          abs(mean_stride_length_paw_hind_left-mean_stride_length_paw_hind_right)/
            (0.5*(abs(mean_stride_length_paw_hind_left)+abs(mean_stride_length_paw_hind_right))),
          NA_real_)
      ) %>% select(mouse_id, symmetry_front, symmetry_hind)

    # Diagonal coupling phase
    coupling_events <- function(d) {
      compute_phase <- function(ref_paw, coupled_paw) {
        ref <- d %>% filter(paw==ref_paw) %>% arrange(x_along_cm)
        cop <- d %>% filter(paw==coupled_paw) %>% arrange(x_along_cm)
        if (nrow(ref)<2||nrow(cop)<1) return(NA_real_)
        phases <- vapply(seq_len(nrow(ref)-1), function(i) {
          x1<-ref$x_along_cm[i]; x2<-ref$x_along_cm[i+1]
          cand <- cop %>% filter(x_along_cm>x1, x_along_cm<x2)
          if (nrow(cand)==0) return(NA_real_)
          (cand$x_along_cm[1]-x1)/(x2-x1)
        }, numeric(1))
        mean(phases, na.rm=TRUE)
      }
      data.frame(diagonal_coupling_left=compute_phase("hind_left","front_right"),
                 diagonal_coupling_right=compute_phase("hind_right","front_left"))
    }
    coupling_mouse <- df %>% filter(!is.na(x_along_cm)) %>%
      group_by(mouse_id) %>% group_modify(~coupling_events(.x)) %>% ungroup()

    # FB distance
    fb_df <- df %>%
      filter(!is.na(true_step_id), !is.na(x_along_cm), !is.na(y_perp_cm)) %>%
      group_by(mouse_id, image_id, side, true_step_id) %>%
      summarise(
        front_x=first(x_along_cm[segment=="Front"]),
        hind_x=first(x_along_cm[segment=="Hind"]),
        front_y=first(y_perp_cm[segment=="Front"]),
        hind_y=first(y_perp_cm[segment=="Hind"]),
        has_both=any(segment=="Front")&any(segment=="Hind"),
        fb_distance_2d_cm=if_else(has_both,sqrt((front_x-hind_x)^2+(front_y-hind_y)^2),NA_real_),
        fb_distance_x_cm=if_else(has_both,abs(front_x-hind_x),NA_real_),
        .groups="drop")

    fb_mouse <- fb_df %>%
      group_by(mouse_id) %>%
      summarise(mean_fb_distance_2d=mean(fb_distance_2d_cm,na.rm=TRUE),
                cv_fb_distance_2d=sd(fb_distance_2d_cm,na.rm=TRUE)/abs(mean_fb_distance_2d),
                mean_fb_distance_x=mean(fb_distance_x_cm,na.rm=TRUE),
                cv_fb_distance_x=sd(fb_distance_x_cm,na.rm=TRUE)/abs(mean_fb_distance_x),
                .groups="drop")

    # Paw overlap
    overlap_mouse <- fb_df %>%
      filter(is.finite(front_x),is.finite(hind_x),has_both) %>%
      mutate(overlap_signed_cm=hind_x-front_x, overlap_abs_cm=abs(hind_x-front_x)) %>%
      group_by(mouse_id) %>%
      summarise(mean_overlap_signed=mean(overlap_signed_cm,na.rm=TRUE),
                mean_overlap_abs=mean(overlap_abs_cm,na.rm=TRUE),
                cv_overlap_abs=sd(overlap_abs_cm,na.rm=TRUE)/abs(mean_overlap_abs),
                .groups="drop")

    # Base of support
    bos_lr <- df %>%
      filter(is.finite(x_along_cm),is.finite(y_perp_cm)) %>%
      group_by(mouse_id,image_id,segment,side) %>%
      arrange(x_along_cm,.by_group=TRUE) %>%
      mutate(lr_index=row_number()) %>% ungroup() %>%
      select(mouse_id,image_id,segment,side,lr_index,y_perp_cm) %>%
      tidyr::pivot_wider(names_from=side, values_from=y_perp_cm, names_prefix="y_") %>%
      mutate(bos_cm=abs(y_L-y_R)) %>% filter(is.finite(bos_cm))

    bos_mouse <- bos_lr %>%
      group_by(mouse_id,segment) %>%
      summarise(mean_bos=mean(bos_cm,na.rm=TRUE),
                sd_bos=sd(bos_cm,na.rm=TRUE),
                cv_bos=sd_bos/abs(mean_bos), .groups="drop") %>%
      tidyr::pivot_wider(names_from=segment,
                         values_from=c(mean_bos,cv_bos), names_sep="_")

    # Perpendicular deviation
    perp_df <- df %>%
      filter(!is.na(x_along_cm),!is.na(y_perp_cm),segment%in%c("Front","Hind")) %>%
      group_by(mouse_id,image_id,segment) %>%
      group_modify(~perpendicular_events(.x)) %>% ungroup()

    perp_mouse <- perp_df %>%
      group_by(mouse_id) %>%
      summarise(mean_perpendicular=mean(perpendicular_dist_cm,na.rm=TRUE),
                cv_perpendicular=sd(perpendicular_dist_cm,na.rm=TRUE)/abs(mean_perpendicular),
                .groups="drop")

    # Drift
    drift_mouse <- df %>%
      filter(is.finite(y_perp_cm)) %>%
      group_by(mouse_id) %>%
      summarise(mean_drift_y=mean(y_perp_cm,na.rm=TRUE),
                mean_abs_drift_y=mean(abs(y_perp_cm),na.rm=TRUE),
                sd_drift_y=sd(y_perp_cm,na.rm=TRUE),
                range_drift_y=diff(range(y_perp_cm,na.rm=TRUE)),
                .groups="drop")

    # Asymmetry indices
    asym_from_paws <- function(L,R)
      ifelse(is.finite(L)&is.finite(R)&(abs(L)+abs(R))>0,
             abs(L-R)/((abs(L)+abs(R))/2), NA_real_)

    asym_mouse <- stride_paw_mouse %>%
      mutate(
        asym_step_front=asym_from_paws(mean_stride_length_paw_front_left,
                                       mean_stride_length_paw_front_right),
        asym_step_hind=asym_from_paws(mean_stride_length_paw_hind_left,
                                      mean_stride_length_paw_hind_right)
      ) %>% select(mouse_id, asym_step_front, asym_step_hind)

    # Meta — guaranteed 1 row per mouse
    meta <- df %>%
      group_by(mouse_id) %>%
      summarise(genotype=first(genotype), treatment=first(treatment),
                mouse_length_cm=suppressWarnings(as.numeric(first(mouse_length_cm))),
                .groups="drop")

    out <- meta %>%
      left_join(stride_mouse,      by="mouse_id") %>%
      left_join(stride_paw_mouse,  by="mouse_id") %>%
      left_join(alt_step_mouse,    by="mouse_id") %>%
      left_join(symmetry_mouse,    by="mouse_id") %>%
      left_join(coupling_mouse,    by="mouse_id") %>%
      left_join(asym_mouse,        by="mouse_id") %>%
      left_join(fb_mouse,          by="mouse_id") %>%
      left_join(bos_mouse,         by="mouse_id") %>%
      left_join(overlap_mouse,     by="mouse_id") %>%
      left_join(drift_mouse,       by="mouse_id") %>%
      left_join(perp_mouse,        by="mouse_id")

    # Body-length normalised columns
    out <- out %>%
      mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm))) %>%
      mutate(
        mean_stride_length_norm                 = mean_stride_length                / mouse_length_cm,
        mean_step_length_Front_norm             = mean_step_length_Front            / mouse_length_cm,
        mean_step_length_Hind_norm              = mean_step_length_Hind             / mouse_length_cm,
        mean_fb_distance_2d_norm                = mean_fb_distance_2d               / mouse_length_cm,
        mean_fb_distance_x_norm                 = mean_fb_distance_x                / mouse_length_cm,
        mean_perpendicular_norm                 = mean_perpendicular                / mouse_length_cm,
        mean_overlap_signed_norm                = mean_overlap_signed               / mouse_length_cm,
        mean_overlap_abs_norm                   = mean_overlap_abs                  / mouse_length_cm,
        mean_bos_Front_norm                     = mean_bos_Front                    / mouse_length_cm,
        mean_bos_Hind_norm                      = mean_bos_Hind                     / mouse_length_cm,
        mean_drift_y_norm                       = mean_drift_y                      / mouse_length_cm,
        mean_abs_drift_y_norm                   = mean_abs_drift_y                  / mouse_length_cm,
        sd_drift_y_norm                         = sd_drift_y                        / mouse_length_cm,
        range_drift_y_norm                      = range_drift_y                     / mouse_length_cm,
        mean_stride_length_paw_front_left_norm  = mean_stride_length_paw_front_left / mouse_length_cm,
        mean_stride_length_paw_front_right_norm = mean_stride_length_paw_front_right/ mouse_length_cm,
        mean_stride_length_paw_hind_left_norm   = mean_stride_length_paw_hind_left  / mouse_length_cm,
        mean_stride_length_paw_hind_right_norm  = mean_stride_length_paw_hind_right / mouse_length_cm
      )

    out %>% mutate(across(where(is.numeric), ~ifelse(is.nan(.)|is.infinite(.), NA_real_, .)))
  })

  # ── Step-level export table ─────────────────────────────────────────────────
  steps_table <- reactive({
    req(paws_rot())
    df        <- paws_rot()
    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)

    meta <- df %>%
      distinct(mouse_id, image_id, genotype, treatment, mouse_length_cm) %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels=geno_pal$levels),
        treatment = factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),
                           levels=treat_pal$levels),
        geno_trt  = factor(paste0(as.character(genotype),"_",as.character(treatment)),
                           levels=make_geno_trt_levels(geno_pal$levels,treat_pal$levels))
      )

    step_len <- df %>%
      filter(!is.na(x_along_cm)) %>%
      arrange(mouse_id,image_id,paw,x_along_cm) %>%
      group_by(mouse_id,image_id,paw) %>%
      mutate(step_length_cm=x_along_cm-lag(x_along_cm)) %>%
      ungroup() %>% mutate(measure_type="step_length")

    fb_df <- df %>%
      filter(!is.na(true_step_id),!is.na(x_along_cm),!is.na(y_perp_cm)) %>%
      group_by(mouse_id,image_id,side,true_step_id) %>%
      summarise(
        segment_front_present=any(segment=="Front"),
        segment_hind_present=any(segment=="Hind"),
        front_x=dplyr::first(x_along_cm[segment=="Front"]),
        hind_x=dplyr::first(x_along_cm[segment=="Hind"]),
        front_y=dplyr::first(y_perp_cm[segment=="Front"]),
        hind_y=dplyr::first(y_perp_cm[segment=="Hind"]),
        fb_distance_2d_cm=if_else(segment_front_present&segment_hind_present,
          sqrt((front_x-hind_x)^2+(front_y-hind_y)^2),NA_real_),
        fb_distance_x_cm=if_else(segment_front_present&segment_hind_present,
          abs(front_x-hind_x),NA_real_),
        .groups="drop") %>%
      mutate(measure_type="fb_distance")

    perp_events <- df %>%
      filter(!is.na(x_along_cm),!is.na(y_perp_cm),segment%in%c("Front","Hind")) %>%
      group_by(mouse_id,image_id,segment) %>%
      group_modify(~perpendicular_events(.x)) %>% ungroup() %>%
      mutate(measure_type="perpendicular_deviation")

    list(
      step_length = step_len %>%
        select(mouse_id,image_id,paw,side,segment,dot_id,step_id,
               x,y,x_along_cm,y_perp_cm,step_length_cm,measure_type) %>%
        left_join(meta,by=c("mouse_id","image_id")),
      fb_distance = fb_df %>%
        select(mouse_id,image_id,side,true_step_id,
               fb_distance_2d_cm,fb_distance_x_cm,measure_type) %>%
        left_join(meta,by=c("mouse_id","image_id")),
      perpendicular_deviation = perp_events %>%
        select(mouse_id,image_id,segment,ref_side,perpendicular_dist_cm,measure_type) %>%
        left_join(meta,by=c("mouse_id","image_id"))
    )
  })

  # ── Feature table output (Tab 2) ─────────────────────────────────────────────
  output$features_table <- renderDT({
    req(features_mouse())
    datatable(features_mouse(), options=list(scrollX=TRUE, pageLength=10))
  })

  # ── Downloads ─────────────────────────────────────────────────────────────
  output$download_features <- downloadHandler(
    filename = function() "gait_features_mouse_level.xlsx",
    content  = function(file) writexl::write_xlsx(features_mouse(), file)
  )
  output$download_steps <- downloadHandler(
    filename = function() "gait_step_level_tables.xlsx",
    content  = function(file) { req(steps_table()); writexl::write_xlsx(steps_table(), file) }
  )

  # ── Plot status (Tab 3) ──────────────────────────────────────────────────
  output$plot_status_ui <- renderUI({
    if (is.null(tryCatch(features_mouse(), error=function(e) NULL))) {
      div(style="padding:40px; text-align:center; color:#9e9e9e;",
          tags$p("\u26A0\ufe0f  No features calculated yet.",
                 style="font-size:16px;"),
          tags$p("Upload and process your data in the Feature Calculation tab first.",
                 style="font-size:13px;"))
    } else NULL
  })

  # ── Overview ref group selector ────────────────────────────────────────────
  output$overview_ref_ui <- renderUI({
    req(features_mouse())
    f   <- features_mouse()
    grp <- input$color_by
    if (is.null(grp)||grp=="none") grp <- "genotype"
    if (grp=="genotype+treatment") grp <- "geno_trt"
    choices <- as.character(unique(na.omit(f[[grp]])))
    selectInput("overview_ref_group","Reference group",
                choices=choices, selected=choices[1])
  })

  # ── Plot reactive ──────────────────────────────────────────────────────────
  plot_obj <- reactive({
    req(features_mouse(), paws_rot())

    geno_pal  <- parse_palette_text(input$geno_pal)
    treat_pal <- parse_palette_text(input$treat_pal)
    gt_levels <- make_geno_trt_levels(geno_pal$levels, treat_pal$levels)

    f <- features_mouse() %>%
      mutate(
        genotype  = factor(as.character(genotype),  levels=geno_pal$levels),
        treatment = factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),
                           levels=treat_pal$levels),
        geno_trt  = factor(paste0(as.character(genotype),"_",as.character(treatment)),
                           levels=gt_levels)
      )

    grp <- input$color_by
    if (grp=="none") grp <- NULL
    if (identical(grp,"genotype+treatment")) grp <- "geno_trt"

    # ---- QC tracks ----
    if (input$plot_type=="qc_tracks") {
      df <- paws_rot(); req(input$qc_mouse)
      df <- df %>% filter(mouse_id==input$qc_mouse)
      p <- ggplot(df,aes(x=x_along_cm,y=y_perp_cm,colour=paw))+
        geom_path(aes(group=interaction(image_id,paw)),linewidth=0.6,alpha=0.8)+
        geom_point(size=2)+
        scale_colour_manual(values=paw_cols)+
        facet_wrap(~image_id,ncol=1)+coord_equal()+
        ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title=paste("Tracks:",input$qc_mouse,"\u2014",
                         ifelse(input$align,"Aligned","Raw centered")),
             x="x_along (cm)",y="y_perp (cm)")
      if (isTRUE(input$qc_show_labels))
        p <- p+ggrepel::geom_label_repel(aes(label=dot_id),size=3)
      return(p)
    }

    # ---- Measure column mapping ----
    if (input$measure=="step") {
      mean_col_raw<-"mean_stride_length"; mean_col_norm<-"mean_stride_length_norm"
      cv_col<-"cv_stride_length"
      y_mean_raw<-"Mean stride length (cm)"; y_mean_norm<-"Stride length / body length"
      y_cv<-"CV(stride length)"
    } else if (input$measure=="fb2d") {
      mean_col_raw<-"mean_fb_distance_2d"; mean_col_norm<-"mean_fb_distance_2d_norm"
      cv_col<-"cv_fb_distance_2d"
      y_mean_raw<-"Mean FB distance 2D (cm)"; y_mean_norm<-"FB distance 2D / body length"
      y_cv<-"CV(FB distance 2D)"
    } else if (input$measure=="fbx") {
      mean_col_raw<-"mean_fb_distance_x"; mean_col_norm<-"mean_fb_distance_x_norm"
      cv_col<-"cv_fb_distance_x"
      y_mean_raw<-"Mean FB distance x-only (cm)"; y_mean_norm<-"FB distance x-only / body length"
      y_cv<-"CV(FB distance x-only)"
    } else {
      mean_col_raw<-"mean_perpendicular"; mean_col_norm<-"mean_perpendicular_norm"
      cv_col<-"cv_perpendicular"
      y_mean_raw<-"Mean perpendicular (cm)"; y_mean_norm<-"Perpendicular / body length"
      y_cv<-"CV(perpendicular)"
    }
    use_norm  <- isTRUE(input$norm_length)
    mean_col  <- if (use_norm) mean_col_norm else mean_col_raw
    y_mean    <- if (use_norm) y_mean_norm   else y_mean_raw

    # helper: apply fill scale
    add_fill_scale <- function(p, grp2) {
      if (grp2=="genotype"  && length(geno_pal$colors)>0)
        p <- p+scale_fill_manual(values=geno_pal$colors,  drop=FALSE)
      if (grp2=="treatment" && length(treat_pal$colors)>0)
        p <- p+scale_fill_manual(values=treat_pal$colors, drop=FALSE)
      if (grp2=="geno_trt") {
        fills <- geno_trt_fill_fallback(gt_levels, geno_pal, treat_pal$levels)
        p <- p+scale_fill_manual(values=fills, drop=FALSE)
      }
      p
    }

    # helper: fix factor order in summ/df
    fix_levels <- function(df, grp2) {
      if (grp2=="genotype")
        df <- df %>% mutate(across(any_of(c("genotype","group")),
          ~factor(as.character(.), levels=geno_pal$levels)))
      else if (grp2=="treatment")
        df <- df %>% mutate(across(any_of(c("treatment","group")),
          ~factor(if_else(is.na(as.character(.))|as.character(.)==""
                          ,"NA",as.character(.)), levels=treat_pal$levels)))
      else
        df <- df %>% mutate(across(any_of(c("geno_trt","group")),
          ~factor(as.character(.), levels=gt_levels)))
      df
    }

    # ---- Mean/CV bar ----
    if (input$plot_type %in% c("mean_sd","cv_sd")) {
      if (is.null(grp)) {
        val <- if (input$plot_type=="mean_sd") mean_col else cv_col
        ylab<- if (input$plot_type=="mean_sd") y_mean   else y_cv
        m   <- mean(f[[val]],na.rm=TRUE); s <- sd(f[[val]],na.rm=TRUE)
        return(
          ggplot(data.frame(group="all",mean=m,sd=s),aes(x=group,y=mean))+
            geom_col(colour="grey20")+
            geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2)+
            ggprism::theme_prism(base_size=14,base_family="Arial")+
            labs(x=NULL,y=ylab)
        )
      }
      val_col <- if (input$plot_type=="mean_sd") mean_col else cv_col
      ylab    <- if (input$plot_type=="mean_sd") y_mean   else y_cv
      ttl     <- if (input$plot_type=="mean_sd") "Mean \u00b1 SD" else "Mean CV \u00b1 SD"

      summ <- f %>%
        group_by(.data[[grp]]) %>%
        summarise(mean=mean(.data[[val_col]],na.rm=TRUE),
                  sd=sd(.data[[val_col]],na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      summ <- fix_levels(summ, grp)

      p <- ggplot(summ,aes(x=group,y=mean,fill=group))+
        geom_col(colour="grey20")+
        geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2)+
        ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title=paste(ttl,"\u2014",grp),x=grp,y=ylab)
      return(add_fill_scale(p, grp))
    }

    # ---- Per-side FB distance ----
    if (input$plot_type=="side_fb") {
      df2 <- paws_rot() %>% distinct(mouse_id,genotype,treatment,mouse_length_cm) %>%
        left_join(
          paws_rot() %>%
            filter(!is.na(true_step_id),!is.na(x_along_cm),!is.na(y_perp_cm)) %>%
            group_by(mouse_id,image_id,side,true_step_id) %>%
            summarise(
              front_x=first(x_along_cm[segment=="Front"]),hind_x=first(x_along_cm[segment=="Hind"]),
              front_y=first(y_perp_cm[segment=="Front"]),hind_y=first(y_perp_cm[segment=="Hind"]),
              has_both=any(segment=="Front")&any(segment=="Hind"),
              fb_distance_2d_cm=if_else(has_both,sqrt((front_x-hind_x)^2+(front_y-hind_y)^2),NA_real_),
              fb_distance_x_cm=if_else(has_both,abs(front_x-hind_x),NA_real_),.groups="drop") %>%
            group_by(mouse_id,side) %>%
            summarise(mean_fb_distance=if(identical(input$fb_mode,"fbx"))
              mean(fb_distance_x_cm,na.rm=TRUE) else mean(fb_distance_2d_cm,na.rm=TRUE),
              .groups="drop"),by="mouse_id")

      if (isTRUE(input$norm_length))
        df2 <- df2 %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          mean_fb_distance=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,
                                   mean_fb_distance/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"

      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=factor(paste0(as.character(genotype),"_",as.character(treatment)),levels=gt_levels))

      summ <- df2 %>% group_by(.data[[grp2]],side) %>%
        summarise(mean=mean(mean_fb_distance,na.rm=TRUE),
                  sd=sd(mean_fb_distance,na.rm=TRUE),.groups="drop") %>% rename(group=1)
      df2  <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=mean_fb_distance,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~side)+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="FB distance per side",x=grp2,y="Mean FB distance (cm)")
      return(add_fill_scale(p, grp2))
    }

    # ---- Per-segment perpendicular ----
    if (input$plot_type=="seg_perp") {
      df2  <- paws_rot()
      meta2<- df2 %>% distinct(mouse_id,genotype,treatment,mouse_length_cm)
      seg_df <- df2 %>%
        filter(!is.na(x_along_cm),!is.na(y_perp_cm),segment%in%c("Front","Hind")) %>%
        group_by(mouse_id,image_id,segment) %>%
        group_modify(~perpendicular_events(.x)) %>% ungroup() %>%
        group_by(mouse_id,segment) %>%
        summarise(mean_perpendicular=mean(perpendicular_dist_cm,na.rm=TRUE),.groups="drop") %>%
        left_join(meta2,by="mouse_id")

      if (isTRUE(input$norm_length))
        seg_df <- seg_df %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          mean_perpendicular=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,
                                     mean_perpendicular/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"

      seg_df <- seg_df %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=factor(paste0(as.character(genotype),"_",as.character(treatment)),levels=gt_levels))

      summ   <- seg_df %>% group_by(.data[[grp2]],segment) %>%
        summarise(mean=mean(mean_perpendicular,na.rm=TRUE),
                  sd=sd(mean_perpendicular,na.rm=TRUE),.groups="drop") %>% rename(group=1)
      seg_df <- fix_levels(seg_df,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(seg_df,aes(x=.data[[grp2]],y=mean_perpendicular,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~segment)+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Perpendicular deviation per segment",x=grp2,y="Mean perpendicular (cm)")
      return(add_fill_scale(p, grp2))
    }

    # ---- Base of support ----
    if (input$plot_type=="bos") {
      df2 <- features_mouse() %>%
        select(mouse_id,genotype,treatment,mouse_length_cm,mean_bos_Front,mean_bos_Hind) %>%
        pivot_longer(c(mean_bos_Front,mean_bos_Hind),names_to="segment",values_to="value") %>%
        mutate(segment=recode(segment,mean_bos_Front="Front",mean_bos_Hind="Hind"))

      ylab <- if (isTRUE(input$norm_length)) "Base of support / body length" else "Base of support (cm)"
      if (isTRUE(input$norm_length))
        df2 <- df2 %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          value=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,value/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"
      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=make_geno_trt_factor(genotype,treatment,geno_pal$levels,treat_pal$levels))

      summ <- df2 %>% group_by(.data[[grp2]],segment) %>%
        summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      df2 <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=value,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~segment)+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Base of support (L-R) per segment",x=grp2,y=ylab)
      return(add_fill_scale(p, grp2))
    }

    # ---- Paw overlap ----
    if (input$plot_type=="overlap") {
      df2 <- features_mouse() %>% select(mouse_id,genotype,treatment,mouse_length_cm,mean_overlap_abs)
      ylab <- if (isTRUE(input$norm_length)) "Hind\u2013front overlap / body length" else "Hind\u2013front overlap (cm)"
      if (isTRUE(input$norm_length))
        df2 <- df2 %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          mean_overlap_abs=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,
                                   mean_overlap_abs/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"
      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=make_geno_trt_factor(genotype,treatment,geno_pal$levels,treat_pal$levels))

      summ <- df2 %>% group_by(.data[[grp2]]) %>%
        summarise(mean=mean(mean_overlap_abs,na.rm=TRUE),sd=sd(mean_overlap_abs,na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      df2 <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=mean_overlap_abs,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),width=0.2,inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Paw overlap (hind vs front)",x=grp2,y=ylab)
      return(add_fill_scale(p, grp2))
    }

    # ---- Per-paw stride length ----
    if (input$plot_type=="paw_step") {
      df2 <- features_mouse() %>%
        select(mouse_id,genotype,treatment,mouse_length_cm,starts_with("mean_stride_length_paw_")) %>%
        pivot_longer(starts_with("mean_stride_length_paw_"),names_to="paw",values_to="value") %>%
        mutate(paw=sub("mean_stride_length_paw_","",paw),
               paw=factor(paw,levels=c("front_left","front_right","hind_left","hind_right")))

      ylab <- if (isTRUE(input$norm_length)) "Step length / body length" else "Step length (cm)"
      if (isTRUE(input$norm_length))
        df2 <- df2 %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          value=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,value/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"
      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=make_geno_trt_factor(genotype,treatment,geno_pal$levels,treat_pal$levels))

      summ <- df2 %>% group_by(.data[[grp2]],paw) %>%
        summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      df2 <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=value,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~paw,ncol=2)+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Per-paw stride length",x=grp2,y=ylab)
      return(add_fill_scale(p, grp2))
    }

    # ---- Asymmetry indices ----
    if (input$plot_type=="asym") {
      df2 <- features_mouse() %>%
        select(mouse_id,genotype,treatment,asym_step_front,asym_step_hind) %>%
        pivot_longer(starts_with("asym_step_"),names_to="limb_set",values_to="asym") %>%
        mutate(limb_set=recode(limb_set,asym_step_front="Front",asym_step_hind="Hind"))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"
      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=make_geno_trt_factor(genotype,treatment,geno_pal$levels,treat_pal$levels))

      summ <- df2 %>% group_by(.data[[grp2]],limb_set) %>%
        summarise(mean=mean(asym,na.rm=TRUE),sd=sd(asym,na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      df2 <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=asym,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~limb_set)+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Left\u2013right asymmetry index",x=grp2,y="Asymmetry index (0 = symmetric)")
      return(add_fill_scale(p, grp2))
    }

    # ---- Drift / stability ----
    if (input$plot_type=="drift") {
      df2 <- features_mouse() %>%
        select(mouse_id,genotype,treatment,mouse_length_cm,
               mean_abs_drift_y,sd_drift_y,range_drift_y) %>%
        pivot_longer(c(mean_abs_drift_y,sd_drift_y,range_drift_y),
                     names_to="metric",values_to="value") %>%
        mutate(metric=recode(metric,mean_abs_drift_y="Mean |y_perp|",
                             sd_drift_y="SD(y_perp)",range_drift_y="Range(y_perp)"))

      ylab <- if (isTRUE(input$norm_length)) "y_perp metric / body length" else "y_perp metric (cm)"
      if (isTRUE(input$norm_length))
        df2 <- df2 %>% mutate(mouse_length_cm=suppressWarnings(as.numeric(mouse_length_cm)),
          value=if_else(is.finite(mouse_length_cm)&mouse_length_cm>0,value/mouse_length_cm,NA_real_))

      grp2 <- input$color_by
      if (grp2=="genotype+treatment") grp2 <- "geno_trt"
      if (grp2=="none") grp2 <- "genotype"
      df2 <- df2 %>% mutate(
        genotype=factor(as.character(genotype),levels=geno_pal$levels),
        treatment=factor(if_else(is.na(treatment)|treatment=="","NA",as.character(treatment)),levels=treat_pal$levels),
        geno_trt=make_geno_trt_factor(genotype,treatment,geno_pal$levels,treat_pal$levels))

      summ <- df2 %>% group_by(.data[[grp2]],metric) %>%
        summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE),.groups="drop") %>%
        rename(group=1)
      df2 <- fix_levels(df2,grp2); summ <- fix_levels(summ,grp2)

      p <- ggplot(df2,aes(x=.data[[grp2]],y=value,fill=.data[[grp2]]))+
        geom_col(data=summ,aes(x=group,y=mean,fill=group),colour="grey20",
                 position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_errorbar(data=summ,aes(x=group,ymin=mean-sd,ymax=mean+sd),
                      width=0.2,position=position_dodge(0.85),inherit.aes=FALSE)+
        geom_point(position=position_jitter(width=0.12),size=2,alpha=0.85,show.legend=FALSE)+
        facet_wrap(~metric,scales="free_y")+ggprism::theme_prism(base_size=14,base_family="Arial")+
        labs(title="Drift / stability (y_perp)",x=grp2,y=ylab)
      return(add_fill_scale(p, grp2))
    }

    # ---- Overview bubble ----
    if (input$plot_type=="overview_bubble") {
      req(input$overview_ref_group)
      grp_col <- if (identical(grp,"geno_trt")) "geno_trt" else if (!is.null(grp)) grp else "genotype"

      measure_map <- c(
        mean_stride_length="Stride length",
        mean_step_length_Front="Step length \u2014 Front",
        mean_step_length_Hind="Step length \u2014 Hind",
        cv_stride_length="CV stride length",
        cv_step_length_Front="CV step length \u2014 Front",
        cv_step_length_Hind="CV step length \u2014 Hind",
        symmetry_front="L-R symmetry \u2014 Front",
        symmetry_hind="L-R symmetry \u2014 Hind",
        diagonal_coupling_left="Diagonal coupling \u2014 Left",
        diagonal_coupling_right="Diagonal coupling \u2014 Right",
        mean_fb_distance_2d="FB distance (2D)",
        mean_fb_distance_x="FB distance (x only)",
        mean_bos_Front="Stance width \u2014 Front",
        mean_bos_Hind="Stance width \u2014 Hind",
        mean_perpendicular="Perpendicular deviation",
        mean_overlap_abs="Paw overlap",
        mean_abs_drift_y="Lateral drift"
      )
      measure_map <- measure_map[names(measure_map) %in% names(f)]
      ref <- input$overview_ref_group

      long_df <- f %>%
        select(mouse_id,all_of(grp_col),all_of(names(measure_map))) %>%
        pivot_longer(all_of(names(measure_map)),names_to="measure_key",values_to="value") %>%
        mutate(measure_label=factor(measure_map[measure_key],
                                    levels=rev(unname(measure_map)))) %>%
        filter(!is.na(value))

      grp_summ <- long_df %>%
        group_by(.data[[grp_col]],measure_key,measure_label) %>%
        summarise(mean_val=mean(value,na.rm=TRUE),sd_val=sd(value,na.rm=TRUE),
                  n=sum(!is.na(value)),.groups="drop")

      ref_stats <- grp_summ %>%
        filter(as.character(.data[[grp_col]])==ref) %>%
        select(measure_key,mean_ref=mean_val,sd_ref=sd_val,n_ref=n)

      effect_df <- grp_summ %>%
        left_join(ref_stats,by="measure_key") %>%
        mutate(pooled_sd=sqrt(((n-1)*sd_val^2+(n_ref-1)*sd_ref^2)/pmax(n+n_ref-2,1)),
               cohens_d=ifelse(pooled_sd>0,(mean_val-mean_ref)/pooled_sd,0),
               abs_d=abs(cohens_d),
               direction=dplyr::case_when(
                 as.character(.data[[grp_col]])==ref ~ "reference",
                 cohens_d>0 ~ "higher", cohens_d<0 ~ "lower", TRUE ~ "reference"))

      if (isTRUE(input$show_significance)) {
        sig_rows <- long_df %>%
          filter(as.character(.data[[grp_col]])!=ref) %>%
          group_by(.data[[grp_col]],measure_key) %>%
          group_modify(function(gd,key) {
            rv <- long_df %>% filter(as.character(.data[[grp_col]])==ref,
                                     measure_key==key$measure_key) %>% pull(value)
            gv <- gd$value
            p  <- if (length(rv)>=2&&length(gv)>=2)
                    tryCatch(wilcox.test(gv,rv,exact=FALSE)$p.value,error=function(e) NA_real_)
                  else NA_real_
            data.frame(p_value=p)
          }) %>% ungroup() %>%
          mutate(sig_star=dplyr::case_when(
            is.na(p_value)~"",p_value<0.001~"***",p_value<0.01~"**",
            p_value<0.05~"*",p_value<0.10~"+",TRUE~""))
        effect_df <- effect_df %>%
          left_join(sig_rows %>% select(.data[[grp_col]],measure_key,sig_star),
                    by=c(grp_col,"measure_key")) %>%
          mutate(sig_star=tidyr::replace_na(sig_star,""))
      } else {
        effect_df <- effect_df %>% mutate(sig_star="")
      }

      effect_df <- fix_levels(effect_df, grp_col)

      p <- ggplot(effect_df,aes(x=.data[[grp_col]],y=measure_label))+
        geom_point(aes(size=pmax(abs_d,0.05),fill=direction),
                   shape=21,colour="grey30",stroke=0.5,alpha=0.88)+
        scale_size_area(max_size=14,name="|Cohen's d|",
                        breaks=c(0.2,0.5,0.8,1.2),
                        labels=c("0.2\nsmall","0.5\nmed","0.8\nlarge","1.2"))+
        scale_fill_manual(values=c(reference="#CCCCCC",higher="#D7191C",lower="#2C7BB6"),
                          name=paste0("vs ",ref),
                          guide=guide_legend(override.aes=list(size=5)))+
        ggprism::theme_prism(base_size=12,base_family="Arial")+
        theme(panel.grid.major.y=element_line(colour="grey88",linewidth=0.4),
              axis.text.y=element_text(size=10),legend.position="right",
              axis.text.x=element_text(angle=30,hjust=1))+
        labs(title=paste("Gait overview \u2014 effect size vs",ref),
             subtitle=if(isTRUE(input$show_significance))
               "Wilcoxon test, uncorrected for multiple comparisons" else NULL,
             x=NULL,y=NULL)

      if (isTRUE(input$show_significance)&&any(nzchar(effect_df$sig_star)))
        p <- p+geom_text(aes(label=sig_star),size=3.5,vjust=-1.1,colour="grey20")
      return(p)
    }

    NULL
  })

  output$plot <- renderPlot({
    req(plot_obj())
    plot_obj()
  }, height=520)

  output$download_plot <- downloadHandler(
    filename = function() paste0("gait_plot_",input$plot_type,".",input$plot_format),
    content  = function(file) {
      req(plot_obj())
      fmt <- tolower(input$plot_format)
      if (fmt=="svg" && !requireNamespace("svglite",quietly=TRUE))
        stop("Package 'svglite' required for SVG export.")
      ggplot2::ggsave(filename=file, plot=plot_obj(),
                      device=switch(fmt,png="png",pdf="pdf",jpeg="jpeg",jpg="jpeg",svg="svg","png"),
                      width=8, height=6, units="in", dpi=300)
    }
  )

} # end server

shinyApp(ui, server)

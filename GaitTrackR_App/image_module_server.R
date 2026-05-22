# image_module_server.R
# Server logic for the "Image -> Data" annotation tab in GaitTrackR

# ---- Paw color palette (must match app.R) ----
IMG_PAW_COLS <- c(FL = "#e31a1c", FR = "#fb9a99",
                  HL = "#1f78b4", HR = "#a6cee3")

# Max display width (px) — image is resized to this for display + detection
IMG_MAX_W <- 1200

# ============================================================
#   Blob detection
# ============================================================

detect_paw_blobs <- function(img_path, scale_factor,
                              front_color, hind_color,
                              min_size) {

  img <- imager::load.image(img_path)

  # Resize for processing (same scale as display)
  if (scale_factor < 1) {
    new_w <- round(imager::width(img)  * scale_factor)
    new_h <- round(imager::height(img) * scale_factor)
    img   <- imager::resize(img, new_w, new_h)
  }

  r <- imager::R(img)
  g <- imager::G(img)
  b <- imager::B(img)

  # Relative color masks (robust to lighting variation)
  make_mask <- function(color) {
    if (color == "red") {
      (r - g) > 0.15 & (r - b) > 0.15 & r > 0.30
    } else {   # blue
      (b - r) > 0.10 & (b - g) > 0.05 & b > 0.20
    }
  }

  get_centroids <- function(mask, init_paw, min_size) {
    if (sum(mask) < min_size) return(data.frame())

    labeled <- imager::label(mask)
    df <- as.data.frame(labeled)
    names(df)[names(df) == "value"] <- "label"
    df <- df[df$label > 0, ]
    if (nrow(df) == 0) return(data.frame())

    out <- df |>
      dplyr::group_by(label) |>
      dplyr::summarise(x    = mean(x),
                       y    = mean(y),
                       size = dplyr::n(),
                       .groups = "drop") |>
      dplyr::filter(size >= min_size) |>
      dplyr::arrange(x) |>
      dplyr::mutate(paw    = init_paw,
                    dot_id = dplyr::row_number()) |>
      dplyr::select(x, y, paw, dot_id)

    as.data.frame(out)
  }

  # Front paws start as "FL", hind as "HL"
  # User toggles L <-> R interactively
  front_pts <- get_centroids(make_mask(front_color), "FL", min_size)
  hind_pts  <- get_centroids(make_mask(hind_color),  "HL", min_size)

  all_pts <- dplyr::bind_rows(front_pts, hind_pts)
  if (nrow(all_pts) == 0) return(data.frame())
  as.data.frame(all_pts)
}

# ============================================================
#   Recompute dot_id per paw, ordered by x position
# ============================================================

recompute_dot_ids <- function(pts) {
  if (is.null(pts) || nrow(pts) == 0) return(pts)
  pts   <- pts[order(pts$paw, pts$x), ]
  split_pts <- split(pts, pts$paw)
  split_pts <- lapply(split_pts, function(p) {
    p$dot_id <- seq_len(nrow(p))
    p
  })
  out <- do.call(rbind, split_pts)
  rownames(out) <- NULL
  out
}

# ============================================================
#   Server function
# ============================================================

imageAnnotationServer <- function(input, output, session) {

  # ---- Reactive state ----
  img_data         <- reactiveVal(NULL)   # list(raster, width, height, scale_factor, path)
  annotation_pts   <- reactiveVal(NULL)   # data.frame(x, y, paw, dot_id)
  scale_mode_on    <- reactiveVal(FALSE)
  scale_clicks_xy  <- reactiveVal(list(x = numeric(0), y = numeric(0)))
  pixels_per_cm    <- reactiveVal(NULL)
  last_ppcm        <- reactiveVal(NULL)   # persists across images

  # ---- Load image ----
  observeEvent(input$img_upload, {
    req(input$img_upload)
    path <- input$img_upload$datapath

    mg   <- magick::image_read(path)
    info <- magick::image_info(mg)

    sf <- min(1, IMG_MAX_W / info$width)

    if (sf < 1) {
      nw <- round(info$width  * sf)
      nh <- round(info$height * sf)
      mg <- magick::image_resize(mg, paste0(nw, "x", nh, "!"))
    }

    ni <- magick::image_info(mg)

    img_data(list(
      path         = path,
      raster       = as.raster(mg),
      width        = ni$width,
      height       = ni$height,
      scale_factor = sf
    ))

    # Auto-fill image_id from filename
    updateTextInput(session, "img_image_id",
                    value = tools::file_path_sans_ext(input$img_upload$name))

    # Reset per-image state
    annotation_pts(NULL)
    scale_clicks_xy(list(x = numeric(0), y = numeric(0)))
    scale_mode_on(FALSE)

    # Apply reused scale if checkbox is ticked
    if (isTRUE(input$img_reuse_scale) && !is.null(last_ppcm())) {
      pixels_per_cm(last_ppcm())
    } else {
      pixels_per_cm(NULL)
    }
  })

  # ---- loaded flag for conditional panel ----
  output$img_loaded <- reactive({ !is.null(img_data()) })
  outputOptions(output, "img_loaded", suspendWhenHidden = FALSE)

  # ---- Dynamic plot height ----
  output$img_plot_ui <- renderUI({
    d <- img_data()
    if (is.null(d)) return(NULL)
    h <- min(600, round(800 * d$height / d$width))
    plotOutput("img_annotation_plot",
               click  = "img_plot_click",
               height = paste0(h, "px"))
  })

  # ---- Scale mode button ----
  observeEvent(input$img_set_scale_btn, {
    req(img_data())
    scale_mode_on(TRUE)
    scale_clicks_xy(list(x = numeric(0), y = numeric(0)))
    pixels_per_cm(NULL)
    showNotification("Scale mode: click first point on ruler",
                     type = "message", duration = 4)
  })

  # ---- Reuse scale checkbox ----
  observeEvent(input$img_reuse_scale, {
    if (isTRUE(input$img_reuse_scale) && !is.null(last_ppcm())) {
      pixels_per_cm(last_ppcm())
      showNotification(
        sprintf("Reusing scale: %.1f px/cm", last_ppcm()),
        type = "message", duration = 3
      )
    } else if (!isTRUE(input$img_reuse_scale)) {
      pixels_per_cm(NULL)
    }
  })

  # ---- Scale status UI ----
  output$img_scale_status <- renderUI({
    ppcm <- pixels_per_cm()
    sc   <- scale_clicks_xy()

    if (!is.null(ppcm)) {
      tags$p(sprintf("\u2713 Scale set: %.1f px/cm", ppcm),
             class = "scale-set")
    } else if (scale_mode_on()) {
      if (length(sc$x) == 0) {
        tags$p("Click first point on ruler...", class = "scale-wait")
      } else {
        tags$p("Click second point on ruler...", class = "scale-wait")
      }
    } else {
      tags$p("Scale not set", class = "scale-unset")
    }
  })

  # ---- Mode status bar ----
  output$img_mode_status <- renderUI({
    if (scale_mode_on()) {
      tags$div(
        style = paste("background:#fff3e0; border:1px solid #ffb300;",
                      "border-radius:4px; padding:6px 12px; font-size:13px;"),
        "\U0001F4CF  Scale mode active — click two points on the ruler"
      )
    } else {
      mode_label <- switch(input$img_edit_mode,
        add_FL    = "Adding FL (front left)",
        add_FR    = "Adding FR (front right)",
        add_HL    = "Adding HL (hind left)",
        add_HR    = "Adding HR (hind right)",
        toggle_lr = "Toggle L \u2194 R — click a dot to flip side",
        delete    = "Delete — click a dot to remove it",
        ""
      )
      tags$div(
        style = paste("background:#f5f5f5; border:1px solid #ddd;",
                      "border-radius:4px; padding:6px 12px; font-size:13px;"),
        paste("Edit mode:", mode_label)
      )
    }
  })

  # ---- Detect button ----
  observeEvent(input$img_detect_btn, {
    req(img_data())
    withProgress(message = "Detecting paw prints...", value = 0.5, {
      tryCatch({
        pts <- detect_paw_blobs(
          img_path     = img_data()$path,
          scale_factor = img_data()$scale_factor,
          front_color  = input$img_front_color,
          hind_color   = input$img_hind_color,
          min_size     = input$img_min_blob
        )

        if (is.null(pts) || nrow(pts) == 0) {
          showNotification(
            "No paw prints detected. Try reducing 'Min blob size'.",
            type = "warning", duration = 6
          )
        } else {
          showNotification(
            sprintf("Detected %d paw prints. Use edit tools to correct L/R.",
                    nrow(pts)),
            type = "message", duration = 5
          )
          annotation_pts(pts)
        }
      }, error = function(e) {
        showNotification(paste("Detection error:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })
  })

  # ---- Clear button ----
  observeEvent(input$img_clear_btn, {
    annotation_pts(NULL)
  })

  # ---- Click handling ----
  observeEvent(input$img_plot_click, {
    req(img_data())
    cx <- input$img_plot_click$x
    cy <- input$img_plot_click$y

    # --- Scale mode ---
    if (scale_mode_on()) {
      sc <- scale_clicks_xy()
      sc$x <- c(sc$x, cx)
      sc$y <- c(sc$y, cy)
      scale_clicks_xy(sc)

      if (length(sc$x) >= 2) {
        px_dist <- sqrt((sc$x[2] - sc$x[1])^2 + (sc$y[2] - sc$y[1])^2)
        ppcm    <- px_dist / input$img_ruler_cm
        pixels_per_cm(ppcm)
        last_ppcm(ppcm)
        scale_mode_on(FALSE)
        showNotification(
          sprintf("\u2713 Scale set: %.1f px/cm", ppcm),
          type = "message", duration = 4
        )
      } else {
        showNotification("Now click the second ruler point",
                         type = "message", duration = 4)
      }
      return()
    }

    # --- Annotation modes ---
    mode <- input$img_edit_mode
    pts  <- annotation_pts()

    if (grepl("^add_", mode)) {
      paw_label <- sub("^add_", "", mode)
      new_row   <- data.frame(x = cx, y = cy, paw = paw_label,
                               dot_id = NA_integer_,
                               stringsAsFactors = FALSE)
      pts  <- if (is.null(pts)) new_row else rbind(pts, new_row)
      pts  <- recompute_dot_ids(pts)
      annotation_pts(pts)

    } else if (mode == "delete" && !is.null(pts) && nrow(pts) > 0) {
      dists   <- sqrt((pts$x - cx)^2 + (pts$y - cy)^2)
      nearest <- which.min(dists)
      if (dists[nearest] < 30) {
        pts <- pts[-nearest, , drop = FALSE]
        pts <- recompute_dot_ids(pts)
        annotation_pts(pts)
      }

    } else if (mode == "toggle_lr" && !is.null(pts) && nrow(pts) > 0) {
      dists   <- sqrt((pts$x - cx)^2 + (pts$y - cy)^2)
      nearest <- which.min(dists)
      if (dists[nearest] < 30) {
        current <- pts$paw[nearest]
        toggled <- switch(current,
          FL = "FR", FR = "FL",
          HL = "HR", HR = "HL",
          current
        )
        pts$paw[nearest] <- toggled
        pts <- recompute_dot_ids(pts)
        annotation_pts(pts)
      }
    }
  })

  # ---- Render plot ----
  output$img_annotation_plot <- renderPlot({
    req(img_data())
    d  <- img_data()
    w  <- d$width
    h  <- d$height

    par(mar = c(0, 0, 0, 0))
    plot(NULL,
         xlim = c(0, w), ylim = c(h, 0),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    rasterImage(d$raster, 0, 0, w, h)

    # Draw scale line
    sc <- scale_clicks_xy()
    if (length(sc$x) >= 1) {
      points(sc$x[1], sc$y[1], pch = 3, cex = 2.5,
             col = "#00c853", lwd = 3)
    }
    if (length(sc$x) >= 2) {
      points(sc$x[2], sc$y[2], pch = 3, cex = 2.5,
             col = "#00c853", lwd = 3)
      lines(sc$x[1:2], sc$y[1:2], col = "#00c853", lwd = 2)
    }

    # Draw annotated points
    pts <- annotation_pts()
    if (!is.null(pts) && nrow(pts) > 0) {
      for (paw in intersect(names(IMG_PAW_COLS), unique(pts$paw))) {
        sub <- pts[pts$paw == paw, ]
        if (nrow(sub) == 0) next
        points(sub$x, sub$y,
               col = IMG_PAW_COLS[paw], bg = IMG_PAW_COLS[paw],
               pch = 21, cex = 2.2, lwd = 1.5)
        text(sub$x, sub$y - 18,
             labels = paste0(paw, sub$dot_id),
             col = IMG_PAW_COLS[paw], cex = 0.75, font = 2)
      }
    }
  })

  # ---- Points table ----
  output$img_points_table <- DT::renderDT({
    pts <- annotation_pts()
    if (is.null(pts) || nrow(pts) == 0) {
      return(DT::datatable(
        data.frame(message = "No points yet — upload an image and click Detect paws"),
        options  = list(dom = "t"),
        rownames = FALSE
      ))
    }

    display <- pts[order(pts$paw, pts$dot_id),
                   c("dot_id", "paw", "x", "y")]
    display$x <- round(display$x, 1)
    display$y <- round(display$y, 1)

    DT::datatable(display,
                  options  = list(pageLength = 30, dom = "tip"),
                  rownames = FALSE)
  })

  # ---- Export status ----
  output$img_export_status <- renderUI({
    pts  <- annotation_pts()
    ppcm <- pixels_per_cm()
    mid  <- trimws(input$img_mouse_id)

    issues <- character(0)
    if (is.null(pts)  || nrow(pts) == 0) issues <- c(issues, "no points")
    if (is.null(ppcm))                   issues <- c(issues, "scale not set")
    if (!nzchar(mid))                    issues <- c(issues, "mouse_id empty")

    if (length(issues) == 0) {
      tags$p(sprintf("\u2713 %d points ready (%d FL, %d FR, %d HL, %d HR)",
                     nrow(pts),
                     sum(pts$paw == "FL"), sum(pts$paw == "FR"),
                     sum(pts$paw == "HL"), sum(pts$paw == "HR")),
             class = "scale-set")
    } else {
      tags$p(paste("\u26A0", paste(issues, collapse = ", ")),
             style = "color: #e65100; font-size: 12px;")
    }
  })

  # ---- Export ----
  output$img_export_btn <- downloadHandler(
    filename = function() {
      iid <- trimws(input$img_image_id)
      if (!nzchar(iid)) iid <- "paw_data"
      paste0(iid, "_coordinates.xlsx")
    },
    content = function(file) {
      req(annotation_pts(), pixels_per_cm())
      pts <- annotation_pts()

      export_df <- data.frame(
        mouse_id      = trimws(input$img_mouse_id),
        dot_id        = pts$dot_id,
        x             = round(pts$x, 2),
        y             = round(pts$y, 2),
        image_id      = trimws(input$img_image_id),
        pixels_per_cm = round(pixels_per_cm(), 2),
        paw           = pts$paw,
        stringsAsFactors = FALSE
      )
      export_df <- export_df[order(export_df$paw, export_df$dot_id), ]
      writexl::write_xlsx(export_df, file)
    }
  )
}

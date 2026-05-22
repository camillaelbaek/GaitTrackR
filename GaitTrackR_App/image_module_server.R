# image_module_server.R

IMG_PAW_COLS <- c(
  front_left  = "#e31a1c",
  front_right = "#fb9a99",
  hind_left   = "#1f78b4",
  hind_right  = "#a6cee3"
)
IMG_MAX_W <- 1200

# Short labels shown on image dots
paw_short <- c(front_left="FL", front_right="FR", hind_left="HL", hind_right="HR")

recompute_dot_ids <- function(pts) {
  if (is.null(pts) || nrow(pts) == 0) return(pts)
  pts <- pts[order(pts$paw, pts$x), ]
  split_pts <- split(pts, pts$paw)
  split_pts <- lapply(split_pts, function(p) { p$dot_id <- seq_len(nrow(p)); p })
  out <- do.call(rbind, split_pts)
  rownames(out) <- NULL
  out
}

imageAnnotationServer <- function(input, output, session) {

  # ---- Reactive state ----
  all_images_data  <- reactiveVal(list())   # named list of image info
  all_annotations  <- reactiveVal(list())   # named list of data.frames
  current_img_name <- reactiveVal(NULL)
  zoom_state       <- reactiveVal(NULL)     # NULL = full, list(xlim, ylim) = zoomed
  scale_mode_on    <- reactiveVal(FALSE)
  scale_clicks_xy  <- reactiveVal(list(x = numeric(0), y = numeric(0)))
  ppcm_store       <- reactiveVal(list())   # pixels_per_cm per image name
  last_ppcm        <- reactiveVal(NULL)

  # ---- Derived ----
  current_img_data <- reactive({
    req(current_img_name())
    all_images_data()[[current_img_name()]]
  })
  current_pts <- reactive({
    req(current_img_name())
    all_annotations()[[current_img_name()]]
  })
  current_ppcm <- reactive({
    nm <- current_img_name()
    if (is.null(nm)) return(NULL)
    ppcm_store()[[nm]]
  })

  # ---- Load images ----
  observeEvent(input$img_upload, {
    req(input$img_upload)
    withProgress(message = "Loading images...", {
      imgs <- list()
      for (i in seq_len(nrow(input$img_upload))) {
        nm   <- input$img_upload$name[i]
        path <- input$img_upload$datapath[i]
        setProgress(i / nrow(input$img_upload), detail = nm)

        mg   <- magick::image_read(path)
        info <- magick::image_info(mg)
        sf   <- min(1, IMG_MAX_W / info$width)
        if (sf < 1) {
          nw <- round(info$width  * sf)
          nh <- round(info$height * sf)
          mg <- magick::image_resize(mg, paste0(nw, "x", nh, "!"))
        }
        ni <- magick::image_info(mg)
        imgs[[nm]] <- list(
          path         = path,
          name         = nm,
          raster       = as.raster(mg),
          width        = ni$width,
          height       = ni$height,
          scale_factor = sf
        )
      }
      all_images_data(imgs)

      # Preserve existing annotations, init NULL for new images
      ann <- all_annotations()
      for (nm in names(imgs)) {
        if (is.null(ann[[nm]])) ann[[nm]] <- NULL
      }
      all_annotations(ann)

      first_nm <- names(imgs)[1]
      current_img_name(first_nm)
      zoom_state(NULL)
      scale_mode_on(FALSE)
      scale_clicks_xy(list(x = numeric(0), y = numeric(0)))

      updateTextInput(session, "img_image_id",
                      value = tools::file_path_sans_ext(first_nm))
    })
  })

  # ---- Navigator UI ----
  output$img_navigator_ui <- renderUI({
    imgs <- all_images_data()
    if (length(imgs) == 0)
      return(p("No images loaded", style = "color:gray; font-size:12px;"))

    nms <- names(imgs)
    cur <- current_img_name()
    idx <- which(nms == cur)

    tagList(
      selectInput("img_select", NULL, choices = nms, selected = cur),
      fluidRow(
        column(6, actionButton("img_prev_btn", "\u2190 Prev",
                               class = "btn-sm btn-default", width = "100%")),
        column(6, actionButton("img_next_btn", "Next \u2192",
                               class = "btn-sm btn-default", width = "100%"))
      ),
      tags$small(sprintf("Image %d of %d", idx, length(nms)),
                 style = "color:gray;")
    )
  })

  observeEvent(input$img_select, {
    req(input$img_select)
    if (identical(input$img_select, current_img_name())) return()
    switch_to_image(input$img_select)
  }, ignoreInit = TRUE)

  observeEvent(input$img_prev_btn, {
    nms <- names(all_images_data())
    idx <- which(nms == current_img_name())
    if (length(idx) && idx > 1) switch_to_image(nms[idx - 1])
  })

  observeEvent(input$img_next_btn, {
    nms <- names(all_images_data())
    idx <- which(nms == current_img_name())
    if (length(idx) && idx < length(nms)) switch_to_image(nms[idx + 1])
  })

  switch_to_image <- function(nm) {
    current_img_name(nm)
    zoom_state(NULL)
    scale_mode_on(FALSE)
    scale_clicks_xy(list(x = numeric(0), y = numeric(0)))
    updateSelectInput(session, "img_select", selected = nm)
    updateTextInput(session, "img_image_id",
                    value = tools::file_path_sans_ext(nm))
    if (isTRUE(input$img_reuse_scale) && !is.null(last_ppcm())) {
      ps <- ppcm_store(); ps[[nm]] <- last_ppcm(); ppcm_store(ps)
    }
  }

  # ---- Image title ----
  output$img_title_ui <- renderUI({
    nm <- current_img_name()
    if (is.null(nm)) return(NULL)
    h4(nm, style = "text-align:center; margin-bottom:4px; font-family:monospace;")
  })

  # ---- Dynamic plot container ----
  output$img_plot_ui <- renderUI({
    d <- current_img_data()
    if (is.null(d)) {
      return(div(
        style = paste("border:2px dashed #ccc; border-radius:8px;",
                      "padding:60px; text-align:center; color:#aaa;"),
        p("Upload images to start annotating", style = "font-size:16px;")
      ))
    }
    h <- min(600, round(800 * d$height / d$width))
    plotOutput("img_annotation_plot",
               click  = "img_plot_click",
               brush  = brushOpts("img_brush", resetOnNew = TRUE),
               height = paste0(h, "px"))
  })

  # ---- Scale mode ----
  observeEvent(input$img_set_scale_btn, {
    req(current_img_data())
    scale_mode_on(TRUE)
    scale_clicks_xy(list(x = numeric(0), y = numeric(0)))
    ps <- ppcm_store(); ps[[current_img_name()]] <- NULL; ppcm_store(ps)
    showNotification("Scale mode: click first point on ruler",
                     type = "message", duration = 4)
  })

  observeEvent(input$img_reuse_scale, {
    if (isTRUE(input$img_reuse_scale) && !is.null(last_ppcm())) {
      ps <- ppcm_store(); ps[[current_img_name()]] <- last_ppcm(); ppcm_store(ps)
    } else if (!isTRUE(input$img_reuse_scale)) {
      ps <- ppcm_store(); ps[[current_img_name()]] <- NULL; ppcm_store(ps)
    }
  })

  output$img_scale_status <- renderUI({
    ppcm <- current_ppcm()
    sc   <- scale_clicks_xy()
    if (!is.null(ppcm)) {
      tags$p(sprintf("\u2713 Scale: %.1f px/cm", ppcm), class = "scale-set")
    } else if (scale_mode_on()) {
      if (length(sc$x) == 0) tags$p("Click first ruler point...",  class = "scale-wait")
      else                   tags$p("Click second ruler point...", class = "scale-wait")
    } else {
      tags$p("Scale not set", class = "scale-unset")
    }
  })

  # ---- Zoom ----
  observeEvent(input$img_zoom_btn, {
    br <- input$img_brush
    if (!is.null(br)) {
      zoom_state(list(
        xlim = c(br$xmin, br$xmax),
        ylim = c(br$ymax, br$ymin)   # keep inverted (image convention)
      ))
    }
  })

  observeEvent(input$img_reset_zoom_btn, {
    zoom_state(NULL)
  })

  # ---- Click handling ----
  observeEvent(input$img_plot_click, {
    req(current_img_data())
    cx <- input$img_plot_click$x
    cy <- input$img_plot_click$y
    nm <- current_img_name()

    # Scale mode
    if (scale_mode_on()) {
      sc <- scale_clicks_xy()
      sc$x <- c(sc$x, cx); sc$y <- c(sc$y, cy)
      scale_clicks_xy(sc)
      if (length(sc$x) >= 2) {
        ppcm <- sqrt((sc$x[2]-sc$x[1])^2 + (sc$y[2]-sc$y[1])^2) / input$img_ruler_cm
        ps <- ppcm_store(); ps[[nm]] <- ppcm; ppcm_store(ps)
        last_ppcm(ppcm)
        scale_mode_on(FALSE)
        showNotification(sprintf("\u2713 Scale: %.1f px/cm", ppcm),
                         type = "message", duration = 4)
      } else {
        showNotification("Click second ruler point", type = "message", duration = 3)
      }
      return()
    }

    # Annotation modes
    mode <- input$img_edit_mode
    ann  <- all_annotations()
    pts  <- ann[[nm]]

    # Dynamic threshold: larger when zoomed out, smaller when zoomed in
    zs        <- zoom_state()
    d         <- current_img_data()
    threshold <- if (!is.null(zs) && !is.null(d)) {
      max(15, round(30 * diff(zs$xlim) / d$width))
    } else 30

    if (grepl("^add_", mode)) {
      paw_label <- sub("^add_", "", mode)
      new_row   <- data.frame(x=cx, y=cy, paw=paw_label,
                               dot_id=NA_integer_, stringsAsFactors=FALSE)
      pts  <- if (is.null(pts)) new_row else rbind(pts, new_row)
      pts  <- recompute_dot_ids(pts)
      ann[[nm]] <- pts; all_annotations(ann)

    } else if (mode == "delete" && !is.null(pts) && nrow(pts) > 0) {
      dists   <- sqrt((pts$x - cx)^2 + (pts$y - cy)^2)
      nearest <- which.min(dists)
      if (dists[nearest] < threshold) {
        pts <- pts[-nearest, , drop=FALSE]
        pts <- recompute_dot_ids(pts)
        ann[[nm]] <- pts; all_annotations(ann)
      }

    } else if (mode == "toggle_lr" && !is.null(pts) && nrow(pts) > 0) {
      dists   <- sqrt((pts$x - cx)^2 + (pts$y - cy)^2)
      nearest <- which.min(dists)
      if (dists[nearest] < threshold) {
        pts$paw[nearest] <- switch(pts$paw[nearest],
          front_left  = "front_right",
          front_right = "front_left",
          hind_left   = "hind_right",
          hind_right  = "hind_left",
          pts$paw[nearest]
        )
        pts <- recompute_dot_ids(pts)
        ann[[nm]] <- pts; all_annotations(ann)
      }
    }
  })

  # ---- Clear current image ----
  observeEvent(input$img_clear_btn, {
    nm <- current_img_name()
    ann <- all_annotations(); ann[[nm]] <- NULL; all_annotations(ann)
  })

  # ---- Render plot ----
  output$img_annotation_plot <- renderPlot({
    req(current_img_data())
    d  <- current_img_data()
    zs <- zoom_state()
    xlim <- if (!is.null(zs)) zs$xlim else c(0, d$width)
    ylim <- if (!is.null(zs)) zs$ylim else c(d$height, 0)

    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE, asp=1)
    rasterImage(d$raster, 0, 0, d$width, d$height)

    # Scale line
    sc <- scale_clicks_xy()
    if (length(sc$x) >= 1)
      points(sc$x[1], sc$y[1], pch=3, cex=2.5, col="#00c853", lwd=3)
    if (length(sc$x) >= 2) {
      points(sc$x[2], sc$y[2], pch=3, cex=2.5, col="#00c853", lwd=3)
      lines(sc$x[1:2], sc$y[1:2], col="#00c853", lwd=2)
    }

    # Annotated paw points
    pts <- current_pts()
    if (!is.null(pts) && nrow(pts) > 0) {
      for (paw in intersect(names(IMG_PAW_COLS), unique(pts$paw))) {
        sub <- pts[pts$paw == paw, ]
        if (nrow(sub) == 0) next
        points(sub$x, sub$y,
               col=IMG_PAW_COLS[paw], bg=IMG_PAW_COLS[paw],
               pch=21, cex=2.2, lwd=1.5)
        text(sub$x, sub$y - 18,
             labels = paste0(paw_short[paw], sub$dot_id),
             col=IMG_PAW_COLS[paw], cex=0.75, font=2)
      }
    }
  })

  # ---- Mode status bar ----
  output$img_mode_status <- renderUI({
    if (scale_mode_on()) {
      div(style=paste("background:#fff3e0; border:1px solid #ffb300;",
                      "border-radius:4px; padding:6px 12px; font-size:13px;"),
          "\U0001F4CF  Scale mode — click two points on the ruler")
    } else {
      mode_label <- switch(input$img_edit_mode,
        add_front_left  = "Adding front_left",
        add_front_right = "Adding front_right",
        add_hind_left   = "Adding hind_left",
        add_hind_right  = "Adding hind_right",
        toggle_lr       = "Toggle left \u2194 right — click a dot",
        delete          = "Delete — click a dot to remove",
        ""
      )
      n        <- if (!is.null(current_pts())) nrow(current_pts()) else 0
      zoom_txt <- if (!is.null(zoom_state())) " | \U0001F50D Zoomed" else ""
      n_imgs   <- length(all_images_data())
      n_ann    <- sum(sapply(all_annotations(), function(x) !is.null(x) && nrow(x) > 0))
      div(style=paste("background:#f5f5f5; border:1px solid #ddd;",
                      "border-radius:4px; padding:6px 12px; font-size:13px;"),
          paste0(mode_label, " | ", n, " points this image",
                 zoom_txt,
                 " | ", n_ann, "/", n_imgs, " images annotated"))
    }
  })

  # ---- Points table ----
  output$img_points_table <- DT::renderDT({
    pts <- current_pts()
    if (is.null(pts) || nrow(pts) == 0) {
      return(DT::datatable(
        data.frame(message="No points yet — click on the image to add paw prints"),
        options=list(dom="t"), rownames=FALSE
      ))
    }
    display <- pts[order(pts$paw, pts$dot_id), c("dot_id","paw","x","y")]
    display$x <- round(display$x, 1); display$y <- round(display$y, 1)
    DT::datatable(display, options=list(pageLength=30, dom="tip"), rownames=FALSE)
  })

  # ---- Export status ----
  output$img_export_status <- renderUI({
    ann   <- all_annotations()
    n_pts <- sum(sapply(ann, function(x) if (!is.null(x)) nrow(x) else 0))
    n_ann <- sum(sapply(ann, function(x) !is.null(x) && nrow(x) > 0))
    n_set <- sum(sapply(names(all_images_data()), function(nm) !is.null(ppcm_store()[[nm]])))

    if (n_pts == 0) {
      tags$p("\u26A0 No points annotated", style="color:#e65100; font-size:12px;")
    } else {
      tags$p(sprintf("\u2713 %d image(s) annotated, %d points total, %d scale(s) set",
                     n_ann, n_pts, n_set),
             style="color:#2e7d32; font-size:12px;")
    }
  })

  # ---- Build export df (one image) ----
  build_export_df <- function(nm) {
    pts  <- all_annotations()[[nm]]
    ppcm <- ppcm_store()[[nm]]
    if (is.null(pts) || nrow(pts) == 0) return(NULL)
    data.frame(
      mouse_id      = trimws(input$img_mouse_id),
      dot_id        = pts$dot_id,
      x             = round(pts$x, 2),
      y             = round(pts$y, 2),
      image_id      = tools::file_path_sans_ext(nm),
      pixels_per_cm = if (!is.null(ppcm)) round(ppcm, 2) else NA_real_,
      paw           = pts$paw,
      stringsAsFactors = FALSE
    )
  }

  # ---- Export current image ----
  output$img_export_current <- downloadHandler(
    filename = function() paste0(tools::file_path_sans_ext(current_img_name()),
                                 "_coordinates.xlsx"),
    content = function(file) {
      df <- build_export_df(current_img_name())
      req(!is.null(df))
      writexl::write_xlsx(df[order(df$paw, df$dot_id), ], file)
    }
  )

  # ---- Export all images ----
  output$img_export_all <- downloadHandler(
    filename = function() "all_images_coordinates.xlsx",
    content = function(file) {
      dfs <- Filter(Negate(is.null),
                    lapply(names(all_images_data()), build_export_df))
      req(length(dfs) > 0)
      combined <- do.call(rbind, dfs)
      combined <- combined[order(combined$image_id, combined$paw, combined$dot_id), ]
      writexl::write_xlsx(combined, file)
    }
  )
}

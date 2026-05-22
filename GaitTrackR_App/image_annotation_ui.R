# image_annotation_ui.R
# UI for the "Image → Data" annotation tab in GaitTrackR
# Place this file in: GaitTrackR_App/source/

imageAnnotationUI <- function() {
  tagList(
    fluidRow(

      # ── Sidebar ──────────────────────────────────────────────────────────────
      column(3,

        # Image upload
        wellPanel(
          h5("📁 Image", style = "font-weight:bold; margin-top:0;"),
          fileInput("ia_file", NULL,
                    accept      = c("image/jpeg", "image/png", ".jpg", ".jpeg", ".png"),
                    buttonLabel = "Browse…",
                    placeholder = "No file selected"),
          fluidRow(
            column(6, textInput("ia_mouse_id", "mouse_id", value = "")),
            column(6, textInput("ia_image_id", "image_id", value = ""))
          )
        ),

        # Color mapping
        wellPanel(
          h5("🎨 Paw colors", style = "font-weight:bold; margin-top:0;"),
          tags$small("Which paw type is printed in each color?"),
          br(), br(),
          selectInput("ia_color_red",  "Red  =",
                      choices  = c("Front (FL/FR)" = "front", "Hind (HL/HR)" = "hind"),
                      selected = "front"),
          selectInput("ia_color_blue", "Blue =",
                      choices  = c("Hind (HL/HR)" = "hind", "Front (FL/FR)" = "front"),
                      selected = "hind")
        ),

        # Scale calibration
        wellPanel(
          h5("📏 Scale", style = "font-weight:bold; margin-top:0;"),
          checkboxInput("ia_reuse_scale", "Reuse scale from previous image", value = FALSE),
          uiOutput("ia_scale_ui"),
          tags$code(textOutput("ia_scale_status"),
                    style = "background:none; font-size:13px;")
        ),

        # Auto-detection
        wellPanel(
          h5("🔍 Auto-detect", style = "font-weight:bold; margin-top:0;"),
          fluidRow(
            column(6, numericInput("ia_thresh_red",  "Red thresh",
                                   value = 0.20, min = 0.05, max = 0.8, step = 0.05)),
            column(6, numericInput("ia_thresh_blue", "Blue thresh",
                                   value = 0.15, min = 0.05, max = 0.8, step = 0.05))
          ),
          numericInput("ia_min_blob", "Min blob size (px)", value = 30, min = 5, step = 5),
          tags$small("Increase min blob size to remove ink speckles."),
          br(), br(),
          actionButton("ia_detect", "▶  Detect paws",
                       class = "btn-success", width = "100%")
        ),

        # Edit mode
        wellPanel(
          h5("✏️ Edit mode", style = "font-weight:bold; margin-top:0;"),
          tags$small("Click on the image to act in the selected mode."),
          br(), br(),
          radioButtons("ia_mode", NULL,
            choices = c(
              "Add FL"             = "add_FL",
              "Add FR"             = "add_FR",
              "Add HL"             = "add_HL",
              "Add HR"             = "add_HR",
              "Delete point"       = "delete",
              "Toggle L ↔ R"       = "toggle",
              "Set scale (2 pts)"  = "scale"
            ),
            selected = "add_FL"
          )
        ),

        # Batch & export
        wellPanel(
          h5("📦 Batch", style = "font-weight:bold; margin-top:0;"),
          tags$small("Annotate one image at a time. Add each to the batch, then export all at once."),
          br(), br(),
          actionButton("ia_add_batch", "✅  Add image to batch",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          downloadButton("ia_export", "⬇  Export batch (.xlsx)",
                         style = "width:100%;")
        )
      ),

      # ── Main panel ──────────────────────────────────────────────────────────
      column(9,

        # Image canvas
        div(
          style = "background:#1e1e1e; border-radius:6px; overflow:hidden; margin-bottom:12px;",
          plotOutput("ia_plot",
                     click  = "ia_click",
                     height = "500px")
        ),

        # Tables
        fluidRow(
          column(6,
            h5("Current image — annotated points"),
            DTOutput("ia_points_dt")
          ),
          column(6,
            h5("Batch — accumulated images"),
            DTOutput("ia_batch_dt")
          )
        )
      )
    )
  )
}

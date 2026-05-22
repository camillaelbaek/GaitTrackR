# image_module_ui.R
# UI for the "Image -> Data" annotation tab in GaitTrackR

imageAnnotationUI <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .scale-set   { color: #2e7d32; font-weight: bold; font-size: 13px; }
        .scale-unset { color: #999;    font-size: 13px; }
        .scale-wait  { color: #e65100; font-size: 13px; }
        .paw-legend  { display: inline-block; width: 12px; height: 12px;
                       border-radius: 50%; margin-right: 4px; }
      "))
    ),

    fluidRow(

      # ---- Sidebar ----
      column(3,
        wellPanel(

          # 1. Upload
          h4("1. Upload image"),
          fileInput("img_upload", NULL,
                    accept = c("image/jpeg", "image/png"),
                    placeholder = "JPG or PNG"),
          fluidRow(
            column(6, textInput("img_mouse_id", "mouse_id", value = "")),
            column(6, textInput("img_image_id", "image_id", value = ""))
          ),

          hr(),

          # 2. Color mapping
          h4("2. Paw colors"),
          fluidRow(
            column(6,
              selectInput("img_front_color", "Front paws",
                          choices = c("red", "blue"), selected = "red")
            ),
            column(6,
              selectInput("img_hind_color", "Hind paws",
                          choices = c("blue", "red"), selected = "blue")
            )
          ),
          tags$small("Front = FL/FR,  Hind = HL/HR",
                     style = "color: gray;"),

          hr(),

          # 3. Scale
          h4("3. Set scale"),
          checkboxInput("img_reuse_scale", "Reuse scale from previous image",
                        value = FALSE),
          conditionalPanel("!input.img_reuse_scale",
            p("Switch to scale mode, then click two points on the ruler.",
              style = "font-size: 12px; color: gray; margin-bottom: 6px;"),
            actionButton("img_set_scale_btn", "\U0001F4CF  Start scale mode",
                         class = "btn-info btn-sm", width = "100%"),
            br(), br(),
            numericInput("img_ruler_cm", "Distance between points (cm)",
                         value = 20, min = 0.1, step = 0.1)
          ),
          uiOutput("img_scale_status"),

          hr(),

          # 4. Detect
          h4("4. Detect paws"),
          numericInput("img_min_blob", "Min blob size (pixels)",
                       value = 30, min = 5, max = 1000, step = 5),
          actionButton("img_detect_btn", "\u25B6  Detect paws",
                       class = "btn-success", width = "100%"),

          hr(),

          # 5. Edit
          h4("5. Edit (click on image)"),
          tags$div(
            tags$span(class = "paw-legend",
                      style = "background:#e31a1c;"), "FL",
            tags$span(class = "paw-legend",
                      style = "background:#fb9a99; margin-left:8px;"), "FR",
            tags$span(class = "paw-legend",
                      style = "background:#1f78b4; margin-left:8px;"), "HL",
            tags$span(class = "paw-legend",
                      style = "background:#a6cee3; margin-left:8px;"), "HR",
            style = "font-size: 12px; margin-bottom: 8px;"
          ),
          radioButtons("img_edit_mode", NULL,
            choices = c(
              "Add FL"       = "add_FL",
              "Add FR"       = "add_FR",
              "Add HL"       = "add_HL",
              "Add HR"       = "add_HR",
              "Toggle L \u2194 R" = "toggle_lr",
              "Delete point" = "delete"
            ),
            selected = "toggle_lr"
          ),
          actionButton("img_clear_btn", "\U0001F5D1  Clear all points",
                       class = "btn-warning btn-sm", width = "100%"),

          hr(),

          # 6. Export
          h4("6. Export"),
          uiOutput("img_export_status"),
          br(),
          downloadButton("img_export_btn", "\u2B07  Download Excel",
                         class = "btn-primary", style = "width: 100%;")
        )
      ),

      # ---- Main panel ----
      column(9,
        conditionalPanel("output.img_loaded == false",
          div(
            style = paste("border: 2px dashed #ccc; border-radius: 8px;",
                          "padding: 60px; text-align: center; color: #aaa;",
                          "margin-bottom: 12px;"),
            tags$i(class = "ti ti-photo", style = "font-size: 48px;"),
            br(), br(),
            p("Upload an image to start annotating",
              style = "font-size: 16px;")
          )
        ),

        # Image with click overlay
        uiOutput("img_plot_ui"),

        # Status bar
        div(style = "margin: 6px 0 10px 0;",
          uiOutput("img_mode_status")
        ),

        hr(),

        # Points table
        h5("Detected / annotated points"),
        DT::DTOutput("img_points_table")
      )
    )
  )
}

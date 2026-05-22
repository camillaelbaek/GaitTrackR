# image_module_ui.R

imageAnnotationUI <- function() {
  tagList(
    tags$head(tags$style(HTML("
      .scale-set   { color:#2e7d32; font-weight:bold; font-size:13px; }
      .scale-unset { color:#999; font-size:13px; }
      .scale-wait  { color:#e65100; font-size:13px; }
      .paw-legend  { display:inline-block; width:12px; height:12px;
                     border-radius:50%; margin-right:4px; }
    "))),

    fluidRow(

      # ---- Sidebar ----
      column(3,
        wellPanel(

          # 1. Upload
          h4("1. Upload images"),
          fileInput("img_upload", NULL,
                    multiple = TRUE,
                    accept   = c("image/jpeg","image/png"),
                    placeholder = "Select JPG / PNG files"),

          hr(),

          # 2. Navigate
          h4("2. Navigate"),
          uiOutput("img_navigator_ui"),

          hr(),

          # 3. Metadata
          h4("3. Metadata"),
          textInput("img_mouse_id", "mouse_id", value = ""),
          textInput("img_image_id", "image_id", value = ""),

          hr(),

          # 4. Paw colors
          h4("4. Paw colors"),
          fluidRow(
            column(6, selectInput("img_front_color", "Front paws",
                                  choices = c("red","blue"), selected = "red")),
            column(6, selectInput("img_hind_color",  "Hind paws",
                                  choices = c("blue","red"), selected = "blue"))
          ),
          tags$small("front = front_left / front_right,  hind = hind_left / hind_right",
                     style = "color:gray;"),

          hr(),

          # 5. Scale
          h4("5. Set scale"),
          checkboxInput("img_reuse_scale", "Reuse scale from previous image",
                        value = FALSE),
          conditionalPanel("!input.img_reuse_scale",
            p("Click 'Start scale mode', then click two points on the ruler.",
              style = "font-size:12px; color:gray; margin-bottom:6px;"),
            actionButton("img_set_scale_btn", "\U0001F4CF  Start scale mode",
                         class = "btn-info btn-sm", width = "100%"),
            br(), br(),
            numericInput("img_ruler_cm", "Distance between points (cm)",
                         value = 20, min = 0.1, step = 0.1)
          ),
          uiOutput("img_scale_status"),

          hr(),

          # 6. Add paw prints
          h4("6. Add paw prints"),
          tags$div(
            tags$span(class="paw-legend", style="background:#e31a1c;"), "front_left",
            tags$span(class="paw-legend", style="background:#fb9a99; margin-left:8px;"), "front_right",
            br(),
            tags$span(class="paw-legend", style="background:#1f78b4;"), "hind_left",
            tags$span(class="paw-legend", style="background:#a6cee3; margin-left:8px;"), "hind_right",
            style = "font-size:12px; margin-bottom:8px;"
          ),
          radioButtons("img_edit_mode", NULL,
            choices = c(
              "Add front_left"        = "add_front_left",
              "Add front_right"       = "add_front_right",
              "Add hind_left"         = "add_hind_left",
              "Add hind_right"        = "add_hind_right",
              "Toggle left \u2194 right" = "toggle_lr",
              "Delete point"          = "delete"
            ),
            selected = "add_front_left"
          ),
          actionButton("img_clear_btn", "\U0001F5D1  Clear current image",
                       class = "btn-warning btn-sm", width = "100%"),

          hr(),

          # 7. Zoom
          h4("7. Zoom"),
          p("Drag on image to select region, then click Zoom in.",
            style = "font-size:12px; color:gray; margin-bottom:6px;"),
          fluidRow(
            column(6, actionButton("img_zoom_btn", "\U0001F50D Zoom in",
                                   class="btn-sm btn-default", width="100%")),
            column(6, actionButton("img_reset_zoom_btn", "\u21BA Reset",
                                   class="btn-sm btn-default", width="100%"))
          ),

          hr(),

          # 8. Save & Export
          h4("8. Save & Export"),
          actionButton("img_save_temp_btn", "\U0001F4BE  Save current image to temp",
                       class = "btn-success", width = "100%"),
          br(), br(),
          uiOutput("img_temp_status"),
          hr(),
          uiOutput("img_export_status"),
          br(),
          downloadButton("img_export_current", "\u2B07 Current image (.xlsx)",
                         class = "btn-default btn-sm",
                         style = "width:100%; margin-bottom:6px;"),
          downloadButton("img_export_temp", "\u2B07 All saved / temp (.xlsx)",
                         class = "btn-primary",
                         style = "width:100%;")
        )
      ),

      # ---- Main panel ----
      column(9,
        uiOutput("img_title_ui"),
        uiOutput("img_plot_ui"),
        div(style = "margin: 6px 0 10px 0;", uiOutput("img_mode_status")),
        hr(),
        h5("Annotated points — current image"),
        DT::DTOutput("img_points_table")
      )
    )
  )
}

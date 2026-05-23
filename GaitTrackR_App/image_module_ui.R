# image_module_ui.R

imageAnnotationUI <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .scale-set   { color:#2e7d32; font-weight:bold; font-size:13px; }
        .scale-unset { color:#999; font-size:13px; }
        .scale-wait  { color:#e65100; font-size:13px; }
        .paw-legend  { display:inline-block; width:12px; height:12px;
                       border-radius:50%; margin-right:4px; }
        .kbd { display:inline-block; padding:1px 6px; border:1px solid #bbb;
               border-radius:3px; background:#f7f7f7; font-family:monospace;
               font-size:12px; color:#333; }
      ")),
      tags$script(HTML("
        $(document).on('keydown', function(e) {
          // Skip if focus is inside a text field
          if (/INPUT|TEXTAREA|SELECT/.test(e.target.tagName)) return;

          var dir = null;
          if (e.key === '+' || e.key === '=') dir = 'in';
          else if (e.key === '-' || e.key === '_') dir = 'out';
          else if (e.key === '0') dir = 'reset';

          if (dir !== null) {
            e.preventDefault();
            Shiny.setInputValue(
              'img_zoom_key',
              { direction: dir, nonce: Math.random() },
              { priority: 'event' }
            );
          }
        });
      "))
    ),

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
                         value = 20, min = 0.1, step = 0.1),
            actionButton("img_update_scale_btn", "\u21BB  Update scale",
                         class = "btn-warning btn-sm", width = "100%")
          ),
          uiOutput("img_scale_status"),

          hr(),

          # 6. Add paw prints
          h4("6. Add paw prints"),
          tags$div(
          uiOutput("img_paw_legend_ui")
           # tags$span(class="paw-legend", style="background:#e31a1c;"), "front_left",
           # tags$span(class="paw-legend", style="background:#fb9a99; margin-left:8px;"), "front_right",
           # br(),
           # tags$span(class="paw-legend", style="background:#1f78b4;"), "hind_left",
           # tags$span(class="paw-legend", style="background:#a6cee3; margin-left:8px;"), "hind_right",
           # style = "font-size:12px; margin-bottom:8px;"
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

          # 7. Save & Export
          h5("7. Save current image to combined temp table"),
          actionButton("img_save_temp_btn", "\U0001F4BE  Save current image to temp",
                       class = "btn-success", width = "100%"),
          br(), br(),
          uiOutput("img_temp_status"),
          hr(),
          uiOutput("img_export_status"),
          br(),
          h5("8. Download current image or combined"),
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

        # Zoom bar
        div(
          style = paste("display:flex; align-items:center; gap:12px;",
                        "background:#f5f5f5; border:1px solid #ddd;",
                        "border-radius:4px; padding:5px 12px;",
                        "margin-bottom:6px; font-size:12px; color:#555;"),
          tags$span("\U0001F50D Zoom:"),
          tags$span(class="kbd", "+"), tags$span("/ =  in"),
          tags$span(style="margin:0 4px;", "|"),
          tags$span(class="kbd", "\u2212"), tags$span("out"),
          tags$span(style="margin:0 4px;", "|"),
          tags$span(class="kbd", "0"), tags$span("reset"),
          tags$span(style="margin:0 4px;", "|"),
          actionButton("img_reset_zoom_btn", "\u21BA Reset zoom",
                       class = "btn-xs btn-default",
                       style = "padding:1px 8px; font-size:11px;")
        ),

        uiOutput("img_plot_ui"),
        div(style = "margin: 6px 0 10px 0;", uiOutput("img_mode_status")),
        hr(),
        h5("Annotated points — current image"),
        DT::DTOutput("img_points_table")
      )
    )
  )
}

# ============================================================ #
# ggPlot Builder — Advanced Visual Plot Constructor
# ============================================================ #
# A Shiny app that lets you build any ggplot2 visualization
# through a GUI, with on-the-fly data reshaping, full aesthetic
# mapping, and complete code generation.
# ============================================================ #
pacman::p_load(shiny,shinyjs,bslib,ggplot2,dplyr,tidyr,readr,readxl,haven,scales,colourpicker,rlang)

# ---- Helper Functions ----

## Generate ggplot2 code string from current app state ----
generate_code <- function(state) {
  
  lines <- c()
  
  # -- Data transformation code --
  data_var <- "plot_data"
  
  # Start with base data reference
  if (!is.null(state$pivot_applied) && state$pivot_applied) {
    lines <- c(lines, "# ---- Data Reshaping ----")
    if (state$pivot_type == "longer") {
      cols_str <- paste0("c(", paste0('"', state$pivot_cols, '"', collapse = ", "), ")")
      lines <- c(lines, paste0(
        data_var, " <- ", state$data_name, " %>%"))
      lines <- c(lines, paste0(
        '  tidyr::pivot_longer(cols = ', cols_str, ','))
      lines <- c(lines, paste0(
        '                      names_to = "', state$pivot_names_to, '",'))
      lines <- c(lines, paste0(
        '                      values_to = "', state$pivot_values_to, '")'))
    } else {
      lines <- c(lines, paste0(
        data_var, " <- ", state$data_name, " %>%"))
      lines <- c(lines, paste0(
        '  tidyr::pivot_wider(names_from = "', state$pivot_names_from, '",'))
      lines <- c(lines, paste0(
        '                     values_from = "', state$pivot_values_from, '")'))
    }
  } else {
    lines <- c(lines, paste0(data_var, " <- ", state$data_name))
  }
  
  # Filtering
  if (!is.null(state$filters) && length(state$filters) > 0) {
    lines <- c(lines, "")
    lines <- c(lines, "# ---- Filtering ----")
    for (f in state$filters) {
      lines <- c(lines, paste0(
        data_var, " <- ", data_var, " %>%"))
      lines <- c(lines, paste0("  dplyr::filter(", f, ")"))
    }
  }
  
  # Computed columns
  if (!is.null(state$computed) && length(state$computed) > 0) {
    lines <- c(lines, "")
    lines <- c(lines, "# ---- Computed Columns ----")
    for (comp in state$computed) {
      lines <- c(lines, paste0(
        data_var, " <- ", data_var, " %>%"))
      lines <- c(lines, paste0(
        '  dplyr::mutate(', comp$name, ' = ', comp$expr, ')'))
    }
  }
  
  # Group summaries
  if (!is.null(state$summary_applied) && state$summary_applied) {
    lines <- c(lines, "")
    lines <- c(lines, "# ---- Group Summary ----")
    group_str <- paste0('"', state$summary_groups, '"', collapse = ", ")
    lines <- c(lines, paste0(
      data_var, " <- ", data_var, " %>%"))
    lines <- c(lines, paste0(
      "  dplyr::group_by(dplyr::across(c(", group_str, "))) %>%"))
    
    summ_parts <- paste0(
      state$summary_stats$name, " = ",
      state$summary_stats$func, "(",
      state$summary_stats$col, ", na.rm = TRUE)"
    )
    summ_str <- paste0(summ_parts, collapse = ",\n                     ")
    lines <- c(lines, paste0(
      '  dplyr::summarise(', summ_str, ','))
    lines <- c(lines, '                    .groups = "drop")')
  }
  
  # -- ggplot code --
  lines <- c(lines, "")
  lines <- c(lines, "# ---- Plot ----")
  
  # Build aes() string
  aes_parts <- c()
  if (!is.null(state$aes$x) && state$aes$x != "") {
    x_val <- state$aes$x
    if (!is.null(state$aes$x_expr) && state$aes$x_expr != "") {
      x_val <- state$aes$x_expr
    }
    aes_parts <- c(aes_parts, paste0("x = ", x_val))
  }
  if (!is.null(state$aes$y) && state$aes$y != "") {
    y_val <- state$aes$y
    if (!is.null(state$aes$y_expr) && state$aes$y_expr != "") {
      y_val <- state$aes$y_expr
    }
    aes_parts <- c(aes_parts, paste0("y = ", y_val))
  }
  for (a in c("color", "fill", "size", "shape", "alpha", "group", "linetype", "label", "weight")) {
    val <- state$aes[[a]]
    if (!is.null(val) && val != "") {
      aes_parts <- c(aes_parts, paste0(a, " = ", val))
    }
  }
  
  aes_str <- paste0("aes(", paste(aes_parts, collapse = ", "), ")")
  lines <- c(lines, paste0("p <- ggplot(", data_var, ", ", aes_str, ") +"))
  
  # Geom layer(s)
  for (geom in state$geoms) {
    geom_args <- c()
    for (nm in names(geom$params)) {
      val <- geom$params[[nm]]
      if (!is.null(val) && val != "" && val != "default") {
        # Numeric params
        if (nm %in% c("alpha", "size", "width", "binwidth", "bins",
                      "linewidth", "position_width", "stroke")) {
          geom_args <- c(geom_args, paste0(nm, " = ", val))
        } else if (nm == "stat") {
          geom_args <- c(geom_args, paste0('stat = "', val, '"'))
        } else if (nm == "position") {
          if (val == "dodge") {
            pw <- geom$params$position_width
            if (is.null(pw) || pw == "") pw <- "0.9"
            geom_args <- c(geom_args, paste0("position = position_dodge(width = ", pw, ")"))
          } else if (val == "fill") {
            geom_args <- c(geom_args, 'position = "fill"')
          } else if (val == "jitter") {
            geom_args <- c(geom_args, "position = position_jitter(width = 0.2)")
          } else if (val == "stack") {
            geom_args <- c(geom_args, 'position = "stack"')
          }
        } else if (nm == "method") {
          geom_args <- c(geom_args, paste0('method = "', val, '"'))
        } else if (nm == "se") {
          geom_args <- c(geom_args, paste0("se = ", val))
        } else if (nm == "color_fixed") {
          geom_args <- c(geom_args, paste0('color = "', val, '"'))
        } else if (nm == "fill_fixed") {
          geom_args <- c(geom_args, paste0('fill = "', val, '"'))
        } else if (nm %in% c("notch", "outlier.shape", "varwidth")) {
          geom_args <- c(geom_args, paste0(nm, " = ", val))
        }
      }
    }
    
    # Remove position_width from direct args (used inside position_dodge)
    geom_args <- geom_args[!grepl("^position_width", geom_args)]
    
    arg_str <- ""
    if (length(geom_args) > 0) {
      arg_str <- paste(geom_args, collapse = ", ")
    }
    
    lines <- c(lines, paste0("  ", geom$func, "(", arg_str, ") +"))
  }
  
  # Faceting
  if (!is.null(state$facet) && state$facet$type != "none") {
    if (state$facet$type == "wrap") {
      facet_args <- paste0('~ ', state$facet$var)
      extras <- c()
      if (!is.null(state$facet$ncol) && state$facet$ncol != "") {
        extras <- c(extras, paste0("ncol = ", state$facet$ncol))
      }
      if (!is.null(state$facet$scales) && state$facet$scales != "fixed") {
        extras <- c(extras, paste0('scales = "', state$facet$scales, '"'))
      }
      extra_str <- ""
      if (length(extras) > 0) extra_str <- paste0(", ", paste(extras, collapse = ", "))
      lines <- c(lines, paste0("  facet_wrap(", facet_args, extra_str, ") +"))
    } else if (state$facet$type == "grid") {
      row_var <- if (!is.null(state$facet$row) && state$facet$row != "") state$facet$row else "."
      col_var <- if (!is.null(state$facet$col) && state$facet$col != "") state$facet$col else "."
      extras <- c()
      if (!is.null(state$facet$scales) && state$facet$scales != "fixed") {
        extras <- c(extras, paste0('scales = "', state$facet$scales, '"'))
      }
      extra_str <- ""
      if (length(extras) > 0) extra_str <- paste0(", ", paste(extras, collapse = ", "))
      lines <- c(lines, paste0("  facet_grid(", row_var, " ~ ", col_var, extra_str, ") +"))
    }
  }
  
  # Coordinate flip
  if (!is.null(state$coord_flip) && state$coord_flip) {
    lines <- c(lines, "  coord_flip() +")
  }
  
  # Scale transformations
  if (!is.null(state$scale_x_trans) && state$scale_x_trans != "identity") {
    lines <- c(lines, paste0('  scale_x_continuous(trans = "', state$scale_x_trans, '") +'))
  }
  if (!is.null(state$scale_y_trans) && state$scale_y_trans != "identity") {
    lines <- c(lines, paste0('  scale_y_continuous(trans = "', state$scale_y_trans, '") +'))
  }
  
  # Manual color/fill scales
  if (!is.null(state$color_palette) && state$color_palette != "default") {
    if (state$color_palette == "viridis") {
      lines <- c(lines, "  scale_color_viridis_d() +")
      lines <- c(lines, "  scale_fill_viridis_d() +")
    } else if (state$color_palette == "brewer") {
      pal <- if (!is.null(state$brewer_palette)) state$brewer_palette else "Set1"
      lines <- c(lines, paste0('  scale_color_brewer(palette = "', pal, '") +'))
      lines <- c(lines, paste0('  scale_fill_brewer(palette = "', pal, '") +'))
    } else if (state$color_palette == "grey") {
      lines <- c(lines, "  scale_color_grey() +")
      lines <- c(lines, "  scale_fill_grey() +")
    }
  }
  
  # Labels
  lab_parts <- c()
  if (!is.null(state$labs$title) && state$labs$title != "") {
    lab_parts <- c(lab_parts, paste0('title = "', state$labs$title, '"'))
  }
  if (!is.null(state$labs$subtitle) && state$labs$subtitle != "") {
    lab_parts <- c(lab_parts, paste0('subtitle = "', state$labs$subtitle, '"'))
  }
  if (!is.null(state$labs$x) && state$labs$x != "") {
    lab_parts <- c(lab_parts, paste0('x = "', state$labs$x, '"'))
  }
  if (!is.null(state$labs$y) && state$labs$y != "") {
    lab_parts <- c(lab_parts, paste0('y = "', state$labs$y, '"'))
  }
  if (!is.null(state$labs$caption) && state$labs$caption != "") {
    lab_parts <- c(lab_parts, paste0('caption = "', state$labs$caption, '"'))
  }
  if (!is.null(state$labs$color) && state$labs$color != "") {
    lab_parts <- c(lab_parts, paste0('color = "', state$labs$color, '"'))
  }
  if (!is.null(state$labs$fill) && state$labs$fill != "") {
    lab_parts <- c(lab_parts, paste0('fill = "', state$labs$fill, '"'))
  }
  
  if (length(lab_parts) > 0) {
    lines <- c(lines, paste0("  labs(", paste(lab_parts, collapse = ",\n       "), ") +"))
  }
  
  # Theme
  theme_func <- if (!is.null(state$theme)) state$theme else "theme_minimal"
  base_size <- if (!is.null(state$base_size) && state$base_size != "") state$base_size else "12"
  lines <- c(lines, paste0("  ", theme_func, "(base_size = ", base_size, ") +"))
  
  # Theme modifications
  theme_mods <- c()
  if (!is.null(state$legend_position) && state$legend_position != "right") {
    if (state$legend_position == "none") {
      theme_mods <- c(theme_mods, 'legend.position = "none"')
    } else {
      theme_mods <- c(theme_mods, paste0('legend.position = "', state$legend_position, '"'))
    }
  }
  if (!is.null(state$axis_text_angle) && state$axis_text_angle != "0") {
    theme_mods <- c(theme_mods, paste0(
      "axis.text.x = element_text(angle = ", state$axis_text_angle, ", hjust = 1)"))
  }
  if (!is.null(state$plot_title_face) && state$plot_title_face != "plain") {
    theme_mods <- c(theme_mods, paste0(
      'plot.title = element_text(face = "', state$plot_title_face, '")'))
  }
  
  if (length(theme_mods) > 0) {
    lines <- c(lines, paste0("  theme(", paste(theme_mods, collapse = ",\n        "), ")"))
  }
  
  # Clean trailing +
  code <- paste(lines, collapse = "\n")
  code <- gsub("\\+\\s*$", "", code)
  # Also handle + followed by nothing meaningful
  code <- gsub("\\) \\+\n\n", ")\n\n", code)
  
  # Add print
  code <- paste0(code, "\n\nprint(p)")
  
  return(code)
}

## Parse column types for smart defaults ----
col_types_info <- function(df) {
  tibble::tibble(
    col = names(df),
    class = sapply(df, function(x) class(x)[1]),
    is_numeric = sapply(df, is.numeric),
    is_factor = sapply(df, function(x) is.factor(x) || is.character(x)),
    n_unique = sapply(df, function(x) length(unique(na.omit(x)))),
    pct_na = sapply(df, function(x) round(mean(is.na(x)) * 100, 1))
  )
}


# ================================================================
# UI
# ================================================================

ui <- page_navbar(
  title = "ggPlot Builder",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    code_font = font_google("Fira Code"),
    "navbar-bg" = "#2c3e50"
  ),
  
  useShinyjs(),
  
  # Custom CSS for compact layout
  tags$head(tags$style(HTML("
    .shiny-input-container { margin-bottom: 8px; }
    .well { padding: 10px; }
    #code_output { font-size: 13px; background: #1e1e2e; color: #cdd6f4;
                   padding: 15px; border-radius: 8px; white-space: pre-wrap;
                   font-family: 'Fira Code', monospace; max-height: 500px;
                   overflow-y: auto; }
    .nav-tabs .nav-link { padding: 6px 12px; font-size: 13px; }
    .card { margin-bottom: 10px; }
    .sidebar { background: #f8f9fa; }
    .plot-container { min-height: 500px; }
    .badge-type { font-size: 10px; padding: 2px 6px; border-radius: 3px; }
    .badge-num { background: #3498db; color: white; }
    .badge-cat { background: #e74c3c; color: white; }
    .col-info-table td { padding: 2px 6px; font-size: 12px; }
    .geom-card { border: 1px solid #dee2e6; border-radius: 6px; padding: 8px;
                 margin-bottom: 6px; background: white; }
    .btn-xs { padding: 2px 8px; font-size: 11px; }
  "))),
  
  # ---- Tab: Data ----
  nav_panel(
    "1. Data",
    icon = icon("database"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Data Source",
        
        radioButtons("data_source", "Source:",
                     choices = c("Upload File" = "upload",
                                 "Built-in Dataset" = "builtin",
                                 "R Environment" = "env"),
                     selected = "builtin", inline = TRUE),
        
        conditionalPanel(
          "input.data_source == 'upload'",
          fileInput("file_upload", "Upload (.csv, .xlsx, .dta, .sav, .rds)",
                    accept = c(".csv", ".xlsx", ".xls", ".dta", ".sav", ".rds",
                               ".tsv", ".txt")),
          conditionalPanel(
            "input.file_upload != null",
            selectInput("sheet_select", "Sheet (for Excel):", choices = NULL)
          )
        ),
        
        conditionalPanel(
          "input.data_source == 'builtin'",
          selectInput("builtin_dataset", "Dataset:",
                      choices = c("mtcars", "iris", "diamonds", "mpg",
                                  "economics", "midwest", "msleep",
                                  "starwars", "storms", "txhousing"),
                      selected = "mtcars")
        ),
        
        hr(),
        h6("Data Info"),
        verbatimTextOutput("data_dims", placeholder = TRUE),
        
        hr(),
        h6("Column Types"),
        tableOutput("col_types_table")
      ),
      
      # Main panel: data operations
      navset_card_tab(
        title = "Data Operations",
        
        nav_panel(
          "Preview",
          DT::DTOutput("data_preview")
        ),
        
        nav_panel(
          "Pivot",
          icon = icon("arrows-rotate"),
          fluidRow(
            column(6,
                   radioButtons("pivot_type", "Pivot Type:",
                                c("Longer" = "longer", "Wider" = "wider"),
                                inline = TRUE),
                   conditionalPanel(
                     "input.pivot_type == 'longer'",
                     selectInput("pivot_cols", "Columns to pivot:",
                                 choices = NULL, multiple = TRUE),
                     textInput("pivot_names_to", "Names column:", value = "name"),
                     textInput("pivot_values_to", "Values column:", value = "value")
                   ),
                   conditionalPanel(
                     "input.pivot_type == 'wider'",
                     selectInput("pivot_names_from", "Names from:", choices = NULL),
                     selectInput("pivot_values_from", "Values from:", choices = NULL)
                   ),
                   actionButton("apply_pivot", "Apply Pivot",
                                class = "btn-primary btn-sm")
            ),
            column(6,
                   h6("Pivoted Preview"),
                   DT::DTOutput("pivot_preview")
            )
          )
        ),
        
        nav_panel(
          "Filter",
          icon = icon("filter"),
          fluidRow(
            column(4,
                   selectInput("filter_col", "Column:", choices = NULL),
                   selectInput("filter_op", "Operator:",
                               choices = c("==" , "!=" , ">" , ">=" , "<" , "<=" ,
                                           "%in%", "is.na", "!is.na",
                                           "str_detect")),
                   uiOutput("filter_value_ui"),
                   actionButton("add_filter", "Add Filter", class = "btn-primary btn-sm")
            ),
            column(8,
                   h6("Active Filters"),
                   uiOutput("active_filters"),
                   actionButton("clear_filters", "Clear All", class = "btn-outline-danger btn-sm")
            )
          )
        ),
        
        nav_panel(
          "Summarize",
          icon = icon("calculator"),
          fluidRow(
            column(5,
                   selectInput("summary_group_vars", "Group By:",
                               choices = NULL, multiple = TRUE),
                   hr(),
                   h6("Summary Statistics"),
                   fluidRow(
                     column(4, selectInput("summ_col", "Column:", choices = NULL)),
                     column(4, selectInput("summ_func", "Function:",
                                           choices = c("mean", "median", "sum", "sd",
                                                       "min", "max", "n()" = "n",
                                                       "n_distinct"))),
                     column(4, textInput("summ_name", "Name:", value = "stat"))
                   ),
                   actionButton("add_summ_stat", "Add Stat", class = "btn-primary btn-sm"),
                   actionButton("apply_summary", "Apply Summary",
                                class = "btn-success btn-sm mt-2")
            ),
            column(7,
                   h6("Defined Stats"),
                   uiOutput("defined_stats"),
                   hr(),
                   h6("Summary Preview"),
                   DT::DTOutput("summary_preview")
            )
          )
        ),
        
        nav_panel(
          "Compute",
          icon = icon("plus"),
          fluidRow(
            column(5,
                   textInput("comp_name", "New Column Name:", placeholder = "ratio"),
                   textInput("comp_expr", "Expression:",
                             placeholder = "column_a / column_b * 100"),
                   helpText("Use column names directly. Supports R expressions."),
                   actionButton("add_computed", "Add Column", class = "btn-primary btn-sm")
            ),
            column(7,
                   h6("Computed Columns"),
                   uiOutput("computed_cols_list"),
                   hr(),
                   h6("Preview with Computed"),
                   DT::DTOutput("compute_preview")
            )
          )
        )
      )
    )
  ),
  
  # ---- Tab: Map & Geom ----
  nav_panel(
    "2. Map + Geom",
    icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Aesthetic Mapping",
        
        # Column selector with type badges
        uiOutput("aes_col_info"),
        
        hr(),
        
        selectInput("aes_x", "X axis:", choices = NULL),
        textInput("aes_x_expr", "X expression (optional):",
                  placeholder = "reorder(col, -val)"),
        selectInput("aes_y", "Y axis:", choices = NULL),
        textInput("aes_y_expr", "Y expression (optional):",
                  placeholder = "after_stat(density)"),
        selectInput("aes_color", "Color:", choices = NULL),
        selectInput("aes_fill", "Fill:", choices = NULL),
        selectInput("aes_size", "Size:", choices = NULL),
        selectInput("aes_shape", "Shape:", choices = NULL),
        selectInput("aes_alpha", "Alpha:", choices = NULL),
        selectInput("aes_group", "Group:", choices = NULL),
        selectInput("aes_linetype", "Linetype:", choices = NULL),
        selectInput("aes_label", "Label:", choices = NULL),
        selectInput("aes_weight", "Weight:", choices = NULL)
      ),
      
      # Main: Geom configuration
      card(
        card_header("Geometry Layers"),
        fluidRow(
          column(4,
                 selectInput("geom_type", "Add Geom:",
                             choices = c(
                               "Bar / Col" = "bar",
                               "Histogram" = "histogram",
                               "Line" = "line",
                               "Point (Scatter)" = "point",
                               "Smooth" = "smooth",
                               "Boxplot" = "boxplot",
                               "Violin" = "violin",
                               "Density" = "density",
                               "Jitter" = "jitter",
                               "Area" = "area",
                               "Tile (Heatmap)" = "tile",
                               "Text" = "text",
                               "Errorbar" = "errorbar",
                               "Ribbon" = "ribbon",
                               "Segment" = "segment",
                               "Step" = "step",
                               "Rug" = "rug",
                               "Density 2D" = "density_2d",
                               "Hex" = "hex",
                               "Contour" = "contour",
                               "Col (identity)" = "col",
                               "Crossbar" = "crossbar",
                               "Pointrange" = "pointrange",
                               "Linerange" = "linerange"
                             ))
          ),
          column(4,
                 actionButton("add_geom", "Add Layer",
                              class = "btn-primary", icon = icon("plus"))
          ),
          column(4,
                 actionButton("clear_geoms", "Clear All Layers",
                              class = "btn-outline-danger btn-sm")
          )
        ),
        hr(),
        uiOutput("geom_layers_ui")
      )
    )
  ),
  
  # ---- Tab: Polish ----
  nav_panel(
    "3. Polish",
    icon = icon("palette"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Labels",
        textInput("lab_title", "Title:", placeholder = "Plot Title"),
        textInput("lab_subtitle", "Subtitle:", placeholder = ""),
        textInput("lab_x", "X Label:", placeholder = ""),
        textInput("lab_y", "Y Label:", placeholder = ""),
        textInput("lab_caption", "Caption:", placeholder = ""),
        textInput("lab_color", "Color Legend:", placeholder = ""),
        textInput("lab_fill", "Fill Legend:", placeholder = "")
      ),
      
      fluidRow(
        column(4,
               card(
                 card_header("Theme"),
                 selectInput("theme_choice", "Base Theme:",
                             choices = c("theme_minimal", "theme_bw", "theme_classic",
                                         "theme_light", "theme_dark", "theme_void",
                                         "theme_linedraw", "theme_gray",
                                         "theme_test"),
                             selected = "theme_minimal"),
                 numericInput("base_size", "Base Font Size:", value = 12, min = 6, max = 30),
                 selectInput("title_face", "Title Face:",
                             choices = c("plain", "bold", "italic", "bold.italic"),
                             selected = "bold")
               )
        ),
        column(4,
               card(
                 card_header("Scales & Coords"),
                 selectInput("color_palette", "Color Palette:",
                             choices = c("Default" = "default",
                                         "Viridis" = "viridis",
                                         "Color Brewer" = "brewer",
                                         "Greyscale" = "grey")),
                 conditionalPanel(
                   "input.color_palette == 'brewer'",
                   selectInput("brewer_pal", "Brewer Palette:",
                               choices = c("Set1", "Set2", "Set3", "Dark2",
                                           "Paired", "Pastel1", "Pastel2",
                                           "Accent", "Spectral", "RdYlBu",
                                           "RdBu", "PiYG", "BrBG"))
                 ),
                 selectInput("scale_x_trans", "X Scale Transform:",
                             choices = c("identity", "log10", "log2", "sqrt", "reverse")),
                 selectInput("scale_y_trans", "Y Scale Transform:",
                             choices = c("identity", "log10", "log2", "sqrt", "reverse")),
                 checkboxInput("coord_flip", "Flip Coordinates", value = FALSE)
               )
        ),
        column(4,
               card(
                 card_header("Legend & Axes"),
                 selectInput("legend_position", "Legend Position:",
                             choices = c("right", "left", "top", "bottom", "none"),
                             selected = "right"),
                 selectInput("axis_text_angle", "X Axis Text Angle:",
                             choices = c("0", "30", "45", "60", "90"),
                             selected = "0")
               ),
               card(
                 card_header("Faceting"),
                 selectInput("facet_type", "Facet Type:",
                             choices = c("None" = "none",
                                         "Wrap" = "wrap",
                                         "Grid" = "grid")),
                 conditionalPanel(
                   "input.facet_type == 'wrap'",
                   selectInput("facet_var", "Facet Variable:", choices = NULL),
                   numericInput("facet_ncol", "Columns:", value = NULL, min = 1, max = 10),
                   selectInput("facet_scales", "Scales:",
                               choices = c("fixed", "free", "free_x", "free_y"))
                 ),
                 conditionalPanel(
                   "input.facet_type == 'grid'",
                   selectInput("facet_row", "Row Variable:", choices = NULL),
                   selectInput("facet_col_var", "Column Variable:", choices = NULL),
                   selectInput("facet_grid_scales", "Scales:",
                               choices = c("fixed", "free", "free_x", "free_y"))
                 )
               )
        )
      )
    )
  ),
  
  # ---- Tab: Preview & Code ----
  nav_panel(
    "4. Output",
    icon = icon("code"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Export",
        numericInput("export_width", "Width (inches):", value = 10, min = 2, max = 30),
        numericInput("export_height", "Height (inches):", value = 7, min = 2, max = 30),
        numericInput("export_dpi", "DPI:", value = 300, min = 72, max = 600),
        downloadButton("download_png", "Download PNG", class = "btn-primary btn-sm mb-2"),
        downloadButton("download_pdf", "Download PDF", class = "btn-outline-primary btn-sm mb-2"),
        downloadButton("download_svg", "Download SVG", class = "btn-outline-primary btn-sm mb-2"),
        hr(),
        downloadButton("download_code", "Download R Script", class = "btn-success btn-sm"),
        hr(),
        actionButton("copy_code", "Copy Code to Clipboard",
                     class = "btn-outline-secondary btn-sm", icon = icon("clipboard"))
      ),
      
      navset_card_tab(
        title = "Output",
        nav_panel(
          "Plot Preview",
          plotOutput("plot_preview", height = "600px")
        ),
        nav_panel(
          "Generated Code",
          htmlOutput("code_output"),
          hr(),
          verbatimTextOutput("code_raw")
        ),
        nav_panel(
          "Working Data Preview",
          DT::DTOutput("working_data_preview")
        )
      )
    )
  )
)


# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {
  
  # ---- Reactive Values ----
  rv <- reactiveValues(
    raw_data = NULL,        # Original uploaded/selected data
    working_data = NULL,    # After all transformations
    data_name = "df",       # Name for code generation
    pivot_applied = FALSE,
    filters = list(),       # List of filter expression strings
    filter_labels = list(), # Human-readable filter descriptions
    computed = list(),      # List of list(name, expr)
    summary_applied = FALSE,
    summary_groups = NULL,
    summary_stats = NULL,   # data.frame(name, func, col)
    geoms = list()          # List of geom specifications
  )
  
  # ---- Data Loading ----
  
  ## Built-in datasets ----
  observeEvent(input$builtin_dataset, {
    req(input$data_source == "builtin")
    ds <- input$builtin_dataset
    df <- switch(ds,
                 "mtcars" = { d <- mtcars; d$car <- rownames(d); d },
                 "iris" = iris,
                 "diamonds" = ggplot2::diamonds,
                 "mpg" = ggplot2::mpg,
                 "economics" = ggplot2::economics,
                 "midwest" = ggplot2::midwest,
                 "msleep" = ggplot2::msleep,
                 "starwars" = dplyr::select(ggplot2::starwars, -where(is.list)),
                 "storms" = if (exists("storms", where = "package:dplyr")) dplyr::storms else ggplot2::storms,
                 "txhousing" = ggplot2::txhousing
    )
    rv$raw_data <- as.data.frame(df)
    rv$data_name <- ds
    reset_transformations()
  }, ignoreNULL = TRUE)
  
  ## File upload ----
  observeEvent(input$file_upload, {
    req(input$file_upload)
    path <- input$file_upload$datapath
    ext <- tools::file_ext(input$file_upload$name)
    
    df <- tryCatch({
      switch(tolower(ext),
             "csv" = readr::read_csv(path, show_col_types = FALSE),
             "tsv" = readr::read_tsv(path, show_col_types = FALSE),
             "txt" = readr::read_delim(path, show_col_types = FALSE),
             "xlsx" = readxl::read_excel(path),
             "xls" = readxl::read_excel(path),
             "dta" = haven::read_dta(path),
             "sav" = haven::read_sav(path),
             "rds" = readRDS(path),
             { showNotification("Unsupported file type", type = "error"); NULL }
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
    
    if (!is.null(df)) {
      rv$raw_data <- as.data.frame(df)
      rv$data_name <- tools::file_path_sans_ext(input$file_upload$name)
      reset_transformations()
    }
  })
  
  ## Reset transformations helper ----
  reset_transformations <- function() {
    rv$pivot_applied <- FALSE
    rv$filters <- list()
    rv$filter_labels <- list()
    rv$computed <- list()
    rv$summary_applied <- FALSE
    rv$summary_groups <- NULL
    rv$summary_stats <- NULL
    rv$working_data <- rv$raw_data
    update_col_choices()
  }
  
  ## Update column choices across all inputs ----
  update_col_choices <- function() {
    req(rv$working_data)
    cols <- names(rv$working_data)
    none_choice <- c("(none)" = "")
    col_choices <- c(none_choice, setNames(cols, cols))
    
    updateSelectInput(session, "aes_x", choices = col_choices)
    updateSelectInput(session, "aes_y", choices = col_choices)
    updateSelectInput(session, "aes_color", choices = col_choices)
    updateSelectInput(session, "aes_fill", choices = col_choices)
    updateSelectInput(session, "aes_size", choices = col_choices)
    updateSelectInput(session, "aes_shape", choices = col_choices)
    updateSelectInput(session, "aes_alpha", choices = col_choices)
    updateSelectInput(session, "aes_group", choices = col_choices)
    updateSelectInput(session, "aes_linetype", choices = col_choices)
    updateSelectInput(session, "aes_label", choices = col_choices)
    updateSelectInput(session, "aes_weight", choices = col_choices)
    
    # Data tab selectors
    updateSelectInput(session, "pivot_cols", choices = cols)
    updateSelectInput(session, "pivot_names_from", choices = cols)
    updateSelectInput(session, "pivot_values_from", choices = cols)
    updateSelectInput(session, "filter_col", choices = cols)
    updateSelectInput(session, "summary_group_vars", choices = cols)
    updateSelectInput(session, "summ_col", choices = cols)
    
    # Facet selectors
    updateSelectInput(session, "facet_var", choices = col_choices)
    updateSelectInput(session, "facet_row", choices = col_choices)
    updateSelectInput(session, "facet_col_var", choices = col_choices)
  }
  
  ## Rebuild working data from raw + all transformations ----
  rebuild_working_data <- function() {
    req(rv$raw_data)
    df <- rv$raw_data
    
    # Apply pivot
    if (rv$pivot_applied) {
      tryCatch({
        if (input$pivot_type == "longer") {
          df <- tidyr::pivot_longer(df,
                                    cols = all_of(input$pivot_cols),
                                    names_to = input$pivot_names_to,
                                    values_to = input$pivot_values_to
          )
        } else {
          df <- tidyr::pivot_wider(df,
                                   names_from = all_of(input$pivot_names_from),
                                   values_from = all_of(input$pivot_values_from)
          )
        }
      }, error = function(e) {
        showNotification(paste("Pivot error:", e$message), type = "error")
      })
    }
    
    # Apply computed columns
    if (length(rv$computed) > 0) {
      for (comp in rv$computed) {
        tryCatch({
          df <- dplyr::mutate(df, !!comp$name := rlang::eval_tidy(
            rlang::parse_expr(comp$expr), data = df
          ))
        }, error = function(e) {
          showNotification(paste("Compute error:", e$message), type = "error")
        })
      }
    }
    
    # Apply filters
    if (length(rv$filters) > 0) {
      for (f in rv$filters) {
        tryCatch({
          df <- dplyr::filter(df, rlang::eval_tidy(rlang::parse_expr(f), data = df))
        }, error = function(e) {
          showNotification(paste("Filter error:", e$message), type = "error")
        })
      }
    }
    
    # Apply summary
    if (rv$summary_applied && !is.null(rv$summary_groups) && !is.null(rv$summary_stats)) {
      tryCatch({
        df <- dplyr::group_by(df, dplyr::across(all_of(rv$summary_groups)))
        
        summ_exprs <- list()
        for (i in seq_len(nrow(rv$summary_stats))) {
          func <- rv$summary_stats$func[i]
          col <- rv$summary_stats$col[i]
          nm <- rv$summary_stats$name[i]
          
          if (func == "n") {
            summ_exprs[[nm]] <- rlang::expr(dplyr::n())
          } else if (func == "n_distinct") {
            summ_exprs[[nm]] <- rlang::parse_expr(
              paste0("dplyr::n_distinct(", col, ", na.rm = TRUE)"))
          } else {
            summ_exprs[[nm]] <- rlang::parse_expr(
              paste0(func, "(", col, ", na.rm = TRUE)"))
          }
        }
        
        df <- dplyr::summarise(df, !!!summ_exprs, .groups = "drop")
      }, error = function(e) {
        showNotification(paste("Summary error:", e$message), type = "error")
      })
    }
    
    rv$working_data <- as.data.frame(df)
    update_col_choices()
  }
  
  # ---- Data Tab Outputs ----
  
  output$data_dims <- renderText({
    req(rv$working_data)
    paste0(nrow(rv$working_data), " rows × ", ncol(rv$working_data), " cols")
  })
  
  output$col_types_table <- renderTable({
    req(rv$working_data)
    info <- col_types_info(rv$working_data)
    data.frame(
      Column = info$col,
      Type = info$class,
      Unique = info$n_unique,
      `% NA` = info$pct_na,
      check.names = FALSE
    )
  }, spacing = "xs", width = "100%")
  
  output$data_preview <- DT::renderDT({
    req(rv$working_data)
    DT::datatable(head(rv$working_data, 500),
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- Pivot ----
  observeEvent(input$apply_pivot, {
    rv$pivot_applied <- TRUE
    rebuild_working_data()
  })
  
  output$pivot_preview <- DT::renderDT({
    req(rv$working_data)
    DT::datatable(head(rv$working_data, 100),
                  options = list(pageLength = 8, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- Filter ----
  
  # Dynamic filter value input
  output$filter_value_ui <- renderUI({
    req(input$filter_col, rv$working_data)
    col <- rv$working_data[[input$filter_col]]
    op <- input$filter_op
    
    if (op %in% c("is.na", "!is.na")) {
      return(NULL)
    }
    
    if (op == "%in%") {
      unique_vals <- sort(unique(na.omit(col)))
      if (length(unique_vals) > 200) unique_vals <- unique_vals[1:200]
      selectInput("filter_value", "Values:",
                  choices = unique_vals, multiple = TRUE)
    } else if (is.numeric(col)) {
      numericInput("filter_value", "Value:", value = median(col, na.rm = TRUE))
    } else {
      unique_vals <- sort(unique(na.omit(col)))
      if (length(unique_vals) <= 50) {
        selectInput("filter_value", "Value:", choices = unique_vals)
      } else {
        textInput("filter_value", "Value:", placeholder = "pattern or value")
      }
    }
  })
  
  observeEvent(input$add_filter, {
    req(input$filter_col, input$filter_op)
    col <- input$filter_col
    op <- input$filter_op
    
    expr <- if (op == "is.na") {
      paste0("is.na(", col, ")")
    } else if (op == "!is.na") {
      paste0("!is.na(", col, ")")
    } else if (op == "%in%") {
      vals <- input$filter_value
      val_str <- paste0('"', vals, '"', collapse = ", ")
      paste0(col, " %in% c(", val_str, ")")
    } else if (op == "str_detect") {
      paste0('stringr::str_detect(', col, ', "', input$filter_value, '")')
    } else {
      val <- input$filter_value
      if (is.numeric(rv$working_data[[col]])) {
        paste0(col, " ", op, " ", val)
      } else {
        paste0(col, ' ', op, ' "', val, '"')
      }
    }
    
    rv$filters <- c(rv$filters, expr)
    rv$filter_labels <- c(rv$filter_labels, expr)
    rebuild_working_data()
  })
  
  observeEvent(input$clear_filters, {
    rv$filters <- list()
    rv$filter_labels <- list()
    rebuild_working_data()
  })
  
  output$active_filters <- renderUI({
    if (length(rv$filter_labels) == 0) return(p("No filters applied."))
    tags$ul(lapply(rv$filter_labels, function(f) tags$li(code(f))))
  })
  
  # ---- Computed Columns ----
  observeEvent(input$add_computed, {
    req(input$comp_name, input$comp_expr)
    rv$computed <- c(rv$computed, list(list(
      name = input$comp_name,
      expr = input$comp_expr
    )))
    rebuild_working_data()
    updateTextInput(session, "comp_name", value = "")
    updateTextInput(session, "comp_expr", value = "")
  })
  
  output$computed_cols_list <- renderUI({
    if (length(rv$computed) == 0) return(p("No computed columns."))
    tags$ul(lapply(rv$computed, function(c) {
      tags$li(code(paste0(c$name, " = ", c$expr)))
    }))
  })
  
  output$compute_preview <- DT::renderDT({
    req(rv$working_data)
    DT::datatable(head(rv$working_data, 100),
                  options = list(pageLength = 8, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- Group Summaries ----
  observeEvent(input$add_summ_stat, {
    new_row <- data.frame(
      name = input$summ_name,
      func = input$summ_func,
      col = input$summ_col,
      stringsAsFactors = FALSE
    )
    if (is.null(rv$summary_stats)) {
      rv$summary_stats <- new_row
    } else {
      rv$summary_stats <- rbind(rv$summary_stats, new_row)
    }
    updateTextInput(session, "summ_name", value = "")
  })
  
  output$defined_stats <- renderUI({
    if (is.null(rv$summary_stats) || nrow(rv$summary_stats) == 0) {
      return(p("No stats defined yet."))
    }
    tags$ul(lapply(seq_len(nrow(rv$summary_stats)), function(i) {
      s <- rv$summary_stats[i, ]
      tags$li(code(paste0(s$name, " = ", s$func, "(", s$col, ")")))
    }))
  })
  
  observeEvent(input$apply_summary, {
    req(input$summary_group_vars, rv$summary_stats)
    rv$summary_applied <- TRUE
    rv$summary_groups <- input$summary_group_vars
    rebuild_working_data()
  })
  
  output$summary_preview <- DT::renderDT({
    req(rv$working_data)
    DT::datatable(head(rv$working_data, 100),
                  options = list(pageLength = 8, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- Column Info for Mapping Tab ----
  output$aes_col_info <- renderUI({
    req(rv$working_data)
    info <- col_types_info(rv$working_data)
    tags$div(
      style = "font-size: 12px; max-height: 150px; overflow-y: auto;",
      tags$table(
        class = "col-info-table",
        style = "width: 100%;",
        tags$tr(tags$th("Column"), tags$th("Type"), tags$th("Unique")),
        lapply(seq_len(nrow(info)), function(i) {
          badge_class <- if (info$is_numeric[i]) "badge-num" else "badge-cat"
          type_label <- if (info$is_numeric[i]) "num" else "cat"
          tags$tr(
            tags$td(info$col[i]),
            tags$td(tags$span(class = paste("badge-type", badge_class), type_label)),
            tags$td(info$n_unique[i])
          )
        })
      )
    )
  })
  
  # ---- Geom Management ----
  
  observeEvent(input$add_geom, {
    geom_type <- input$geom_type
    
    geom_spec <- switch(geom_type,
                        "bar" = list(
                          func = "geom_bar",
                          label = "Bar",
                          params = list(stat = "count", position = "stack",
                                        position_width = "0.9", alpha = "", width = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "col" = list(
                          func = "geom_col",
                          label = "Col (identity)",
                          params = list(position = "stack", position_width = "0.9",
                                        alpha = "", width = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "histogram" = list(
                          func = "geom_histogram",
                          label = "Histogram",
                          params = list(bins = "30", binwidth = "", alpha = "0.7",
                                        color_fixed = "", fill_fixed = "", position = "stack")
                        ),
                        "line" = list(
                          func = "geom_line",
                          label = "Line",
                          params = list(linewidth = "", alpha = "", linetype = "",
                                        color_fixed = "")
                        ),
                        "step" = list(
                          func = "geom_step",
                          label = "Step",
                          params = list(linewidth = "", alpha = "", color_fixed = "")
                        ),
                        "point" = list(
                          func = "geom_point",
                          label = "Point",
                          params = list(size = "", alpha = "", shape = "", stroke = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "jitter" = list(
                          func = "geom_jitter",
                          label = "Jitter",
                          params = list(size = "", alpha = "", width = "0.2",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "smooth" = list(
                          func = "geom_smooth",
                          label = "Smooth",
                          params = list(method = "loess", se = "TRUE", alpha = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "boxplot" = list(
                          func = "geom_boxplot",
                          label = "Boxplot",
                          params = list(alpha = "", width = "", notch = "",
                                        outlier.shape = "", varwidth = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "violin" = list(
                          func = "geom_violin",
                          label = "Violin",
                          params = list(alpha = "", width = "", scale = "",
                                        color_fixed = "", fill_fixed = "")
                        ),
                        "density" = list(
                          func = "geom_density",
                          label = "Density",
                          params = list(alpha = "0.5", color_fixed = "", fill_fixed = "")
                        ),
                        "area" = list(
                          func = "geom_area",
                          label = "Area",
                          params = list(alpha = "0.5", color_fixed = "", fill_fixed = "",
                                        position = "stack")
                        ),
                        "tile" = list(
                          func = "geom_tile",
                          label = "Tile",
                          params = list(alpha = "", color_fixed = "")
                        ),
                        "text" = list(
                          func = "geom_text",
                          label = "Text",
                          params = list(size = "3", alpha = "", color_fixed = "")
                        ),
                        "errorbar" = list(
                          func = "geom_errorbar",
                          label = "Errorbar",
                          params = list(width = "0.2", alpha = "", color_fixed = "")
                        ),
                        "ribbon" = list(
                          func = "geom_ribbon",
                          label = "Ribbon",
                          params = list(alpha = "0.3", fill_fixed = "")
                        ),
                        "segment" = list(
                          func = "geom_segment",
                          label = "Segment",
                          params = list(linewidth = "", alpha = "", color_fixed = "")
                        ),
                        "rug" = list(
                          func = "geom_rug",
                          label = "Rug",
                          params = list(alpha = "0.5", color_fixed = "")
                        ),
                        "density_2d" = list(
                          func = "geom_density_2d",
                          label = "Density 2D",
                          params = list(alpha = "", color_fixed = "")
                        ),
                        "hex" = list(
                          func = "geom_hex",
                          label = "Hex",
                          params = list(bins = "30", alpha = "")
                        ),
                        "contour" = list(
                          func = "geom_contour",
                          label = "Contour",
                          params = list(bins = "", alpha = "", color_fixed = "")
                        ),
                        "crossbar" = list(
                          func = "geom_crossbar",
                          label = "Crossbar",
                          params = list(width = "0.5", alpha = "", color_fixed = "", fill_fixed = "")
                        ),
                        "pointrange" = list(
                          func = "geom_pointrange",
                          label = "Pointrange",
                          params = list(alpha = "", size = "", color_fixed = "")
                        ),
                        "linerange" = list(
                          func = "geom_linerange",
                          label = "Linerange",
                          params = list(alpha = "", color_fixed = "")
                        ),
                        # Default fallback
                        list(func = paste0("geom_", geom_type), label = geom_type,
                             params = list(alpha = ""))
    )
    
    geom_spec$id <- paste0("geom_", length(rv$geoms) + 1, "_", sample(1000:9999, 1))
    rv$geoms <- c(rv$geoms, list(geom_spec))
  })
  
  observeEvent(input$clear_geoms, {
    rv$geoms <- list()
  })
  
  # Dynamic geom parameter UI
  output$geom_layers_ui <- renderUI({
    if (length(rv$geoms) == 0) {
      return(p("No geometry layers added. Select a geom type and click 'Add Layer'.",
               class = "text-muted"))
    }
    
    lapply(seq_along(rv$geoms), function(i) {
      geom <- rv$geoms[[i]]
      id_prefix <- paste0("geom_", i, "_")
      
      # Build parameter inputs
      param_inputs <- lapply(names(geom$params), function(pname) {
        input_id <- paste0(id_prefix, pname)
        current_val <- geom$params[[pname]]
        
        # Choose input type based on parameter name
        if (pname == "stat") {
          selectInput(input_id, "Stat:",
                      choices = c("count", "identity", "summary", "bin"),
                      selected = current_val, width = "120px")
        } else if (pname == "position") {
          selectInput(input_id, "Position:",
                      choices = c("stack", "dodge", "fill", "jitter", "identity"),
                      selected = current_val, width = "120px")
        } else if (pname == "method") {
          selectInput(input_id, "Method:",
                      choices = c("loess", "lm", "glm", "gam"),
                      selected = current_val, width = "120px")
        } else if (pname == "se") {
          selectInput(input_id, "CI band:",
                      choices = c("TRUE", "FALSE"),
                      selected = current_val, width = "100px")
        } else if (pname %in% c("color_fixed", "fill_fixed")) {
          label <- if (pname == "color_fixed") "Fixed Color" else "Fixed Fill"
          textInput(input_id, label, value = current_val,
                    placeholder = "#hex or name", width = "120px")
        } else {
          numericInput(input_id, gsub("_", " ", tools::toTitleCase(pname)),
                       value = if (current_val != "") as.numeric(current_val) else NA,
                       width = "100px")
        }
      })
      
      # Remove button
      remove_id <- paste0("remove_geom_", i)
      
      div(
        class = "geom-card",
        fluidRow(
          column(3, tags$strong(paste0(i, ". ", geom$label)),
                 tags$br(), tags$code(geom$func, style = "font-size: 11px;")),
          column(8,
                 fluidRow(
                   lapply(param_inputs, function(inp) column(3, inp))
                 )
          ),
          column(1, actionButton(remove_id, "", icon = icon("trash"),
                                 class = "btn-outline-danger btn-xs mt-3"))
        )
      )
    })
  })
  
  # Observe geom parameter changes and removal
  observe({
    lapply(seq_along(rv$geoms), function(i) {
      # Track parameter changes
      geom <- rv$geoms[[i]]
      id_prefix <- paste0("geom_", i, "_")
      
      lapply(names(geom$params), function(pname) {
        input_id <- paste0(id_prefix, pname)
        val <- input[[input_id]]
        if (!is.null(val)) {
          val_str <- as.character(val)
          if (is.na(val)) val_str <- ""
          rv$geoms[[i]]$params[[pname]] <- val_str
        }
      })
      
      # Track remove button
      remove_id <- paste0("remove_geom_", i)
      observeEvent(input[[remove_id]], {
        rv$geoms <- rv$geoms[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # ---- Build Plot State ----
  plot_state <- reactive({
    req(rv$working_data)
    
    list(
      data_name = rv$data_name,
      pivot_applied = rv$pivot_applied,
      pivot_type = input$pivot_type,
      pivot_cols = input$pivot_cols,
      pivot_names_to = input$pivot_names_to,
      pivot_values_to = input$pivot_values_to,
      pivot_names_from = input$pivot_names_from,
      pivot_values_from = input$pivot_values_from,
      filters = rv$filters,
      computed = rv$computed,
      summary_applied = rv$summary_applied,
      summary_groups = rv$summary_groups,
      summary_stats = rv$summary_stats,
      aes = list(
        x = input$aes_x, y = input$aes_y,
        x_expr = input$aes_x_expr, y_expr = input$aes_y_expr,
        color = input$aes_color, fill = input$aes_fill,
        size = input$aes_size, shape = input$aes_shape,
        alpha = input$aes_alpha, group = input$aes_group,
        linetype = input$aes_linetype, label = input$aes_label,
        weight = input$aes_weight
      ),
      geoms = rv$geoms,
      facet = list(
        type = input$facet_type,
        var = input$facet_var,
        ncol = input$facet_ncol,
        scales = if (input$facet_type == "wrap") input$facet_scales else input$facet_grid_scales,
        row = input$facet_row,
        col = input$facet_col_var
      ),
      coord_flip = input$coord_flip,
      scale_x_trans = input$scale_x_trans,
      scale_y_trans = input$scale_y_trans,
      color_palette = input$color_palette,
      brewer_palette = input$brewer_pal,
      labs = list(
        title = input$lab_title, subtitle = input$lab_subtitle,
        x = input$lab_x, y = input$lab_y, caption = input$lab_caption,
        color = input$lab_color, fill = input$lab_fill
      ),
      theme = input$theme_choice,
      base_size = input$base_size,
      legend_position = input$legend_position,
      axis_text_angle = input$axis_text_angle,
      plot_title_face = input$title_face
    )
  })
  
  # ---- Generated Code ----
  generated_code <- reactive({
    state <- plot_state()
    if (length(state$geoms) == 0) return("# Add at least one geom layer")
    generate_code(state)
  })
  
  output$code_output <- renderUI({
    code <- generated_code()
    tags$pre(id = "code_output", code)
  })
  
  output$code_raw <- renderText({
    generated_code()
  })
  
  # ---- Plot Preview ----
  output$plot_preview <- renderPlot({
    req(rv$working_data, length(rv$geoms) > 0)
    
    code <- generated_code()
    
    # Replace data_name with actual data reference
    env <- new.env(parent = globalenv())
    env[[rv$data_name]] <- rv$raw_data
    env$plot_data <- rv$working_data
    
    # Override the data variable assignment so it uses working_data directly
    modified_code <- sub(
      paste0("^", rv$data_name, ".*$|^plot_data <- .*$"),
      "",
      code
    )
    # Just use working_data directly
    eval_code <- paste0(
      "plot_data <- plot_data\n",
      # Remove any line that assigns to plot_data from the original code
      gsub("plot_data <- [^\n]+\n", "", code)
    )
    
    tryCatch({
      # Simpler approach: just eval the generated code in an env
      # that already has plot_data set to working_data
      env$plot_data <- rv$working_data
      
      # Parse only the ggplot part (from "p <- ggplot..." onward)
      gg_start <- regexpr("p <- ggplot", code)
      if (gg_start > 0) {
        gg_code <- substring(code, gg_start)
        eval(parse(text = gg_code), envir = env)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Plot error:\n", e$message),
           cex = 1.2, col = "red")
    })
  }, res = 96)
  
  # ---- Working Data Preview ----
  output$working_data_preview <- DT::renderDT({
    req(rv$working_data)
    DT::datatable(head(rv$working_data, 500),
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ---- Export: PNG ----
  output$download_png <- downloadHandler(
    filename = function() paste0("ggplot_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$working_data, length(rv$geoms) > 0)
      env <- new.env(parent = globalenv())
      env$plot_data <- rv$working_data
      code <- generated_code()
      gg_start <- regexpr("p <- ggplot", code)
      if (gg_start > 0) {
        gg_code <- substring(code, gg_start)
        eval(parse(text = gg_code), envir = env)
        ggsave(file, plot = env$p,
               width = input$export_width, height = input$export_height,
               dpi = input$export_dpi, bg = "white")
      }
    }
  )
  
  # ---- Export: PDF ----
  output$download_pdf <- downloadHandler(
    filename = function() paste0("ggplot_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(rv$working_data, length(rv$geoms) > 0)
      env <- new.env(parent = globalenv())
      env$plot_data <- rv$working_data
      code <- generated_code()
      gg_start <- regexpr("p <- ggplot", code)
      if (gg_start > 0) {
        gg_code <- substring(code, gg_start)
        eval(parse(text = gg_code), envir = env)
        ggsave(file, plot = env$p,
               width = input$export_width, height = input$export_height,
               device = "pdf", bg = "white")
      }
    }
  )
  
  # ---- Export: SVG ----
  output$download_svg <- downloadHandler(
    filename = function() paste0("ggplot_", Sys.Date(), ".svg"),
    content = function(file) {
      req(rv$working_data, length(rv$geoms) > 0)
      env <- new.env(parent = globalenv())
      env$plot_data <- rv$working_data
      code <- generated_code()
      gg_start <- regexpr("p <- ggplot", code)
      if (gg_start > 0) {
        gg_code <- substring(code, gg_start)
        eval(parse(text = gg_code), envir = env)
        ggsave(file, plot = env$p,
               width = input$export_width, height = input$export_height,
               device = "svg", bg = "white")
      }
    }
  )
  
  # ---- Export: R Script ----
  output$download_code <- downloadHandler(
    filename = function() paste0("ggplot_code_", Sys.Date(), ".R"),
    content = function(file) {
      header <- paste0(
        "# ============================================================ #\n",
        "# ggPlot Builder — Generated Code\n",
        "# Generated: ", Sys.time(), "\n",
        "# ============================================================ #\n\n",
        ",ggplot2)\n,dplyr)\n,tidyr)\n,scales)\n\n"
      )
      writeLines(paste0(header, generated_code()), file)
    }
  )
  
  # ---- Copy to Clipboard ----
  observeEvent(input$copy_code, {
    code <- generated_code()
    shinyjs::runjs(sprintf(
      'navigator.clipboard.writeText(%s).then(function() {
         Shiny.notifications.show({html: "Code copied to clipboard!", type: "message"});
       });',
      jsonlite::toJSON(code, auto_unbox = TRUE)
    ))
    showNotification("Code copied to clipboard!", type = "message", duration = 2)
  })
  
  # ---- Initialize with default dataset ----
  observe({
    if (is.null(rv$raw_data) && input$data_source == "builtin") {
      rv$raw_data <- {d <- mtcars; d$car <- rownames(d); d}
      rv$data_name <- "mtcars"
      rv$working_data <- rv$raw_data
      update_col_choices()
    }
  })
}


# ---- Run App ----
shinyApp(ui = ui, server = server)
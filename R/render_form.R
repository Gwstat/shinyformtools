sft_layout_index_label <- function(labels, index, default) {
  if (is.null(labels)) {
    return(default)
  }

  index_name <- as.character(index)

  if (!is.null(names(labels)) && index_name %in% names(labels)) {
    value <- labels[[index_name]]

    if (!is.null(value) && nzchar(value)) {
      return(value)
    }
  }

  position <- as.integer(index) + 1L

  if (length(labels) >= position) {
    value <- labels[[position]]

    if (!is.null(value) && nzchar(value)) {
      return(value)
    }
  }

  default
}

sft_first_field_label <- function(fields, label_name) {
  labels <- vapply(
    fields,
    function(field) field[[label_name]] %||% "",
    character(1)
  )

  labels <- labels[nzchar(labels)]

  if (length(labels) == 0L) {
    return(NULL)
  }

  labels[[1L]]
}

sft_slide_label <- function(form, fields, slide_value) {
  sft_first_field_label(fields, "slide_label") %||%
    sft_layout_index_label(
      labels = form$slide_labels,
      index = slide_value,
      default = paste("Slide", as.integer(slide_value) + 1L)
    )
}

sft_tab_label <- function(form, fields, tab_value) {
  sft_first_field_label(fields, "tab_label") %||%
    sft_layout_index_label(
      labels = form$tab_labels,
      index = tab_value,
      default = paste("Tab", as.integer(tab_value) + 1L)
    )
}

sft_call_ui_region <- function(region, form, ns = identity, prefix = "", values = NULL) {
  if (!is.function(region)) {
    return(region)
  }

  args <- list(
    ns = ns,
    prefix = prefix,
    values = values,
    form = form
  )

  formals_names <- names(formals(region))

  if ("..." %in% formals_names) {
    return(do.call(region, args))
  }

  do.call(region, args[intersect(names(args), formals_names)])
}

sft_render_form_region <- function(region,
                                   form,
                                   ns = identity,
                                   prefix = "",
                                   values = NULL) {
  if (is.null(region)) {
    return(NULL)
  }

  region_ui <- sft_call_ui_region(
    region = region,
    form = form,
    ns = ns,
    prefix = prefix,
    values = values
  )

  if (is.null(region_ui)) {
    return(NULL)
  }

  if (is.character(region_ui)) {
    if (length(region_ui) != 1L || is.na(region_ui)) {
      stop("form header/footer character values must be scalar strings.", call. = FALSE)
    }

    return(shiny::HTML(region_ui))
  }

  region_ui
}


sft_label_mandatory <- function(label) {
  shiny::tagList(
    label,
    shiny::span(
      "*",
      style = "color: red; margin-left: 3px;"
    )
  )
}

sft_field_value_from_record <- function(record, field) {
  if (is.null(record)) {
    return(NULL)
  }

  if (is.data.frame(record)) {
    record <- sft_row_to_list(record)
  }

  if (field$db_column %in% names(record)) {
    return(record[[field$db_column]])
  }

  if (field$id %in% names(record)) {
    return(record[[field$id]])
  }

  NULL
}

sft_render_input <- function(field,
                             ns = identity,
                             prefix = "",
                             value = NULL,
                             read_only = FALSE) {
  input_fun <- sft_input_function(field$input_type)
  local_args <- sft_prepare_input_args(field, value = value)

  label <- if (isTRUE(field$mandatory)) {
    sft_label_mandatory(field$label)
  } else {
    field$label
  }

  input_ui <- do.call(
    input_fun,
    c(
      list(
        inputId = ns(paste0(prefix, field$id)),
        label = label
      ),
      local_args
    )
  )

  if (!isTRUE(field$editable) || isTRUE(read_only)) {
    input_ui <- shinyjs::disabled(input_ui)
  }

  input_ui
}

sft_render_html_field <- function(field) {
  shiny::HTML(field$args$content %||% "")
}

sft_render_output_field <- function(field,
                                    ns = identity,
                                    prefix = "") {
  output_id <- ns(paste0(prefix, field$id))

  label <- if (nzchar(field$label)) {
    shiny::tags$label(field$label)
  } else {
    NULL
  }

  output_ui <- switch(
    field$type,
    text_output = shiny::textOutput(outputId = output_id),
    plot_output = do.call(
      shiny::plotOutput,
      c(
        list(outputId = output_id),
        field$args
      )
    ),
    ui_output = shiny::uiOutput(outputId = output_id),
    stop("Unsupported output field type.", call. = FALSE)
  )

  shiny::tagList(
    label,
    output_ui
  )
}

#' Render a single form field
#'
#' @param field Field object created with [form_field()].
#' @param ns Namespace function. Defaults to `identity`.
#' @param prefix Optional input/output id prefix, for example `"add_"` or
#'   `"edit_"`.
#' @param value Optional value used to prefill input fields.
#' @param read_only Logical. Whether all input fields should be rendered disabled.
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' name <- form_field(id = "name", label = "Name", mandatory = TRUE)
#' ui <- fluidPage(
#'   render_field(name, prefix = "add_"),
#'   render_field(name, prefix = "edit_", value = "Ada", read_only = TRUE)
#' )
#' server <- function(input, output, session) {}
#' shinyApp(ui, server)
#' }
#' @keywords internal
render_field <- function(field,
                             ns = identity,
                             prefix = "",
                             value = NULL,
                             read_only = FALSE) {
  if (!inherits(field, "sft_field")) {
    stop("field must be a form_field object.", call. = FALSE)
  }

  if (!isTRUE(field$show)) {
    return(NULL)
  }

  field_ui <- switch(
    field$type,
    input = sft_render_input(
      field = field,
      ns = ns,
      prefix = prefix,
      value = value,
      read_only = read_only
    ),
    html = sft_render_html_field(field),
    text_output = sft_render_output_field(
      field = field,
      ns = ns,
      prefix = prefix
    ),
    plot_output = sft_render_output_field(
      field = field,
      ns = ns,
      prefix = prefix
    ),
    ui_output = sft_render_output_field(
      field = field,
      ns = ns,
      prefix = prefix
    ),
    stop("Unsupported field type.", call. = FALSE)
  )

  shiny::div(
    id = ns(paste0("sft_field_container_", prefix, field$id)),
    class = "sft-field-container",
    field_ui
  )
}

sft_render_field_column <- function(fields,
                                    ns = identity,
                                    prefix = "",
                                    values = NULL,
                                    read_only = FALSE) {
  fields <- fields[order(vapply(fields, function(field) field$pos, integer(1)))]

  shiny::tagList(
    lapply(
      fields,
      function(field) {
        render_field(
          field = field,
          ns = ns,
          prefix = prefix,
          value = sft_field_value_from_record(values, field),
          read_only = read_only
        )
      }
    )
  )
}

sft_render_field_group <- function(fields,
                                   ns = identity,
                                   prefix = "",
                                   values = NULL,
                                   read_only = FALSE) {
  cols <- sort(unique(vapply(fields, function(field) field$col, integer(1))))

  if (length(cols) <= 1L) {
    return(
      sft_render_field_column(
        fields = fields,
        ns = ns,
        prefix = prefix,
        values = values,
        read_only = read_only
      )
    )
  }

  col_width <- max(1L, floor(12 / length(cols)))

  shiny::fluidRow(
    lapply(
      cols,
      function(col_value) {
        col_fields <- Filter(
          function(field) identical(field$col, col_value),
          fields
        )

        shiny::column(
          width = col_width,
          sft_render_field_column(
            fields = col_fields,
            ns = ns,
            prefix = prefix,
            values = values,
            read_only = read_only
          )
        )
      }
    )
  )
}

sft_render_form_tabs <- function(form,
                                 fields,
                                 ns = identity,
                                 prefix = "",
                                 values = NULL,
                                 read_only = FALSE) {
  tabs <- sort(unique(vapply(fields, function(field) field$tab, integer(1))))

  if (length(tabs) <= 1L) {
    return(
      sft_render_field_group(
        fields = fields,
        ns = ns,
        prefix = prefix,
        values = values,
        read_only = read_only
      )
    )
  }

  tab_panels <- lapply(
    tabs,
    function(tab_value) {
      tab_fields <- Filter(
        function(field) identical(field$tab, tab_value),
        fields
      )

      shiny::tabPanel(
        title = sft_tab_label(
          form = form,
          fields = tab_fields,
          tab_value = tab_value
        ),
        sft_render_field_group(
          fields = tab_fields,
          ns = ns,
          prefix = prefix,
          values = values,
          read_only = read_only
        )
      )
    }
  )

  do.call(shiny::tabsetPanel, tab_panels)
}

sft_render_form_slides <- function(form,
                                   fields,
                                   ns = identity,
                                   prefix = "",
                                   values = NULL,
                                   read_only = FALSE) {
  slides <- sort(unique(vapply(fields, function(field) field$slide, integer(1))))

  if (length(slides) <= 1L) {
    return(
      sft_render_form_tabs(
        form = form,
        fields = fields,
        ns = ns,
        prefix = prefix,
        values = values,
        read_only = read_only
      )
    )
  }

  if (!requireNamespace("shinyglide", quietly = TRUE)) {
    stop(
      "Using multiple slide values requires the shinyglide package. ",
      "Install it with install.packages('shinyglide').",
      call. = FALSE
    )
  }

  slide_screens <- lapply(
    slides,
    function(slide_value) {
      slide_fields <- Filter(
        function(field) identical(field$slide, slide_value),
        fields
      )

      shinyglide::screen(
        shiny::tags$div(
          class = "sft-slide-screen",
          sft_render_form_tabs(
            form = form,
            fields = slide_fields,
            ns = ns,
            prefix = prefix,
            values = values,
            read_only = read_only
          )
        )
      )
    }
  )

  do.call(shinyglide::glide, slide_screens)
}

#' Render form fields
#'
#' Renders all active fields of a form. Input fields are stored; HTML and output
#' fields are display-only. Layout uses `slide`, `tab`, `col` and `pos`.
#'
#' @param form Object created with [form()].
#' @param ns Namespace function. Defaults to `identity`.
#' @param prefix Optional input/output id prefix, for example `"add_"` or
#'   `"edit_"`.
#' @param values Optional named list or one-row data frame used to prefill
#'   input fields.
#' @param read_only Logical. Whether all input fields should be rendered disabled.
#' @param editable_fields Optional character vector of field ids that remain
#'   editable; every other input field is rendered read-only. `NULL` keeps each
#'   field's own `editable` setting.
#'
#' @return Shiny UI.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name", mandatory = TRUE),
#'     form_field(id = "city", label = "City")
#'   )
#' )
#' ui <- fluidPage(
#'   # Render all fields prefilled, with only "name" left editable.
#'   render_form_fields(
#'     contacts,
#'     prefix = "edit_",
#'     values = list(name = "Ada", city = "London"),
#'     editable_fields = "name"
#'   )
#' )
#' server <- function(input, output, session) {}
#' shinyApp(ui, server)
#' }
#' @export
render_form_fields <- function(form,
                                   ns = identity,
                                   prefix = "",
                                   values = NULL,
                                   read_only = FALSE,
                                   editable_fields = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  # Per-user field editing: lock every input field not in editable_fields by
  # flipping its editable flag, which the field renderers already honour.
  if (!is.null(editable_fields)) {
    form$fields <- lapply(form$fields, function(field) {
      if (sft_is_input_field(field) && !(field$id %in% editable_fields)) {
        field$editable <- FALSE
      }
      field
    })
  }

  fields <- sft_visible_active_fields(form)

  if (length(fields) == 0L) {
    return(shiny::tagList())
  }

  fields <- fields[
    order(
      vapply(fields, function(field) field$slide, integer(1)),
      vapply(fields, function(field) field$tab, integer(1)),
      vapply(fields, function(field) field$col, integer(1)),
      vapply(fields, function(field) field$pos, integer(1))
    )
  ]

  shiny::tagList(
    sft_render_form_region(
      region = form$header,
      form = form,
      ns = ns,
      prefix = prefix,
      values = values
    ),
    sft_render_form_slides(
      form = form,
      fields = fields,
      ns = ns,
      prefix = prefix,
      values = values,
      read_only = read_only
    ),
    sft_render_form_region(
      region = form$footer,
      form = form,
      ns = ns,
      prefix = prefix,
      values = values
    )
  )
}

#' Collect Shiny input values for a form
#'
#' @param form Object created with [form()].
#' @param input Shiny input object.
#' @param prefix Optional input id prefix, for example `"add_"` or `"edit_"`.
#' @param editable_only Logical. Whether non-editable input fields should be
#'   omitted from the returned values. Useful for edit dialogs that may be opened
#'   read-only or include locked fields.
#'
#' @return Named list of input values.
#' @examples
#' \dontrun{
#' library(shiny)
#' contacts <- form(
#'   form_id = "contacts", table_name = "contacts",
#'   db = db_sqlite(tempfile(fileext = ".sqlite")),
#'   fields = list(
#'     form_field(id = "name", label = "Name"),
#'     form_field(id = "city", label = "City")
#'   )
#' )
#' ui <- fluidPage(
#'   render_form_fields(contacts, prefix = "add_"),
#'   actionButton("save", "Save")
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$save, {
#'     values <- collect_input_values(contacts, input, prefix = "add_")
#'     print(values)
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
collect_input_values <- function(form,
                                     input,
                                     prefix = "",
                                     editable_only = FALSE) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  fields <- if (isTRUE(editable_only)) {
    sft_editable_input_fields(form, visible_only = TRUE)
  } else {
    sft_visible_input_fields(form)
  }
  values <- list()

  for (field in fields) {
    values[[field$id]] <- input[[paste0(prefix, field$id)]]
  }

  values
}

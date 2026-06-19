# Geometry attachment for shape fields (see shape_field). Shapes are loaded
# out of band -- never through the form -- and stored as serialized text in the
# field's TEXT column, so they are backend-neutral and survive edits to a
# record's input fields untouched. sf does the geometry work and is an optional
# (Suggests) dependency: an sf object as input means the caller already has it.

sft_require_shape_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required for shape support; install it with ",
      "install.packages('", pkg, "').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Resolve the shape field to fill: the form's only shape field, or the one named
# by `shape_field` when there is more than one.
sft_resolve_shape_field <- function(form, shape_field = NULL) {
  shape_fields <- Filter(sft_is_shape_field, form$fields)

  if (length(shape_fields) == 0L) {
    stop(
      "The form has no shape field. Add shape_field() to the form.",
      call. = FALSE
    )
  }

  if (!is.null(shape_field)) {
    match <- Find(function(f) identical(f$id, shape_field), shape_fields)
    if (is.null(match)) {
      stop("No shape field with id '", shape_field, "' on the form.", call. = FALSE)
    }
    return(match)
  }

  if (length(shape_fields) > 1L) {
    stop(
      "The form has multiple shape fields; pass shape_field to choose one.",
      call. = FALSE
    )
  }

  shape_fields[[1L]]
}

# Reproject an sfc to the field CRS and serialize each geometry to the field's
# text encoding. Returns a character vector, one entry per geometry.
sft_serialize_geometry <- function(geometry, field) {
  sft_require_shape_package("sf")

  target_crs <- sf::st_crs(field$crs)
  source_crs <- sf::st_crs(geometry)

  if (!is.na(source_crs) && !is.na(target_crs) && source_crs != target_crs) {
    geometry <- sf::st_transform(geometry, target_crs)
  } else if (is.na(source_crs) && !is.na(target_crs)) {
    sf::st_crs(geometry) <- target_crs
  }

  if (identical(field$encoding, "geojson")) {
    sft_require_shape_package("geojsonsf")
    return(as.character(geojsonsf::sfc_geojson(geometry)))
  }

  as.character(sf::st_as_text(geometry))
}

#' Decode a stored shape column back to sf geometry
#'
#' Inverse of the serialization done by [attach_shapes()]. Use it to turn a
#' fetched shape column into geometry for rendering (for example on a map).
#'
#' @param values Character vector of serialized geometries from a shape column.
#'   `NA` / empty entries (records with no attached shape) become empty
#'   geometries.
#' @param field The shape field definition, from [shape_field()].
#'
#' @return An `sf` simple-feature geometry list-column (`sfc`).
#' @examples
#' # Decodes stored WKT/GeoJSON text back into sf geometry (requires the sf
#' # package, an optional dependency).
#' \dontrun{
#' fld <- shape_field("geom", encoding = "wkt", crs = 4326)
#' values <- c("POINT (0 0)", NA, "POINT (1 1)")
#' geom <- decode_shape(values, fld)
#' geom
#' }
#' @export
decode_shape <- function(values, field) {
  sft_require_shape_package("sf")

  if (!sft_is_shape_field(field)) {
    stop("field must be a shape field created with shape_field().", call. = FALSE)
  }

  values <- as.character(values)
  present <- !is.na(values) & nzchar(values)

  decode_one <- function(x) {
    if (identical(field$encoding, "geojson")) {
      sft_require_shape_package("geojsonsf")
      geojsonsf::geojson_sfc(x)
    } else {
      sf::st_as_sfc(x)
    }
  }

  geometries <- vector("list", length(values))
  empty <- sf::st_geometrycollection()

  for (i in seq_along(values)) {
    geometries[[i]] <- if (present[i]) decode_one(values[i])[[1L]] else empty
  }

  sfc <- sf::st_sfc(geometries, crs = sf::st_crs(field$crs))
  sfc
}

#' Attach fixed geometries to records
#'
#' Loads geometries from an `sf` object into a form's shape column, matching
#' features to existing records by a key. Shapes are stored as serialized text
#' and are never editable through the form, so editing a record's input fields
#' never disturbs its geometry. Run after the records exist; each updated record
#' gets an audit-log entry.
#'
#' @param form Object created with [form()].
#' @param shapes An `sf` object whose rows carry the key column(s) and geometry.
#' @param key Named character vector mapping record (database) columns to the
#'   `sf` columns that identify the same feature, e.g.
#'   `c(district_id = "WKR_NR")`. An unnamed string means the column has the
#'   same name in both.
#' @param conn Optional DBI connection.
#' @param shape_field Optional shape field id, required only when the form has
#'   more than one shape field.
#' @param user Optional user identifier for the audit log.
#'
#' @return Invisibly, a list with `attached` (number of records updated) and
#'   `unmatched` (number of `sf` features with no matching live record).
#' @examples
#' # Loads geometry from an sf object into a form's shape column, matching
#' # features to existing records by key (requires the sf package and that the
#' # records already exist in the database).
#' \dontrun{
#' frm <- form(
#'   form_id = "districts",
#'   fields = list(
#'     form_field("district_id", "District ID"),
#'     shape_field("geom", crs = 4326)
#'   ),
#'   db = db_sqlite("districts.sqlite")
#' )
#' # `shapes` is an sf object carrying a WKR_NR column and geometry:
#' attach_shapes(frm, shapes, key = c(district_id = "WKR_NR"))
#' }
#' @export
attach_shapes <- function(form,
                              shapes,
                              key,
                              conn = NULL,
                              shape_field = NULL,
                              user = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be an form object.", call. = FALSE)
  }

  sft_require_shape_package("sf")

  if (!inherits(shapes, "sf")) {
    stop("shapes must be an sf object.", call. = FALSE)
  }

  field <- sft_resolve_shape_field(form, shape_field)

  if (is.null(names(key)) || any(!nzchar(names(key)))) {
    names(key) <- unname(key)
  }
  record_cols <- names(key)
  sf_cols <- unname(key)

  missing_sf <- setdiff(sf_cols, names(shapes))
  if (length(missing_sf) > 0L) {
    stop(
      "sf object is missing key column(s): ",
      paste(missing_sf, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  conn <- sft_resolve_connection(form, conn)
  sft_ensure_schema(conn, form, user = user)

  encoded <- sft_serialize_geometry(sf::st_geometry(shapes), field)
  key_table <- sf::st_drop_geometry(shapes)

  geom_column <- field$db_column
  table_name <- form$table_name

  where_sql <- paste(
    vapply(
      record_cols,
      function(col) paste0(sft_quote_identifier(conn, col), " = ?"),
      character(1)
    ),
    collapse = " AND "
  )

  select_sql <- paste0(
    "SELECT * FROM ", sft_quote_identifier(conn, table_name),
    " WHERE ", where_sql, " AND sft_is_deleted = 0"
  )

  attached <- 0L
  unmatched <- 0L

  sft_db_with_transaction(conn, {
    for (i in seq_len(nrow(key_table))) {
      where_params <- lapply(
        sf_cols,
        function(col) sft_clean_db_value(key_table[[col]][i])
      )

      old_rows <- DBI::dbGetQuery(conn, select_sql, params = where_params)

      if (nrow(old_rows) == 0L) {
        unmatched <- unmatched + 1L
        next
      }

      for (r in seq_len(nrow(old_rows))) {
        old_record <- old_rows[r, , drop = FALSE]
        record_id <- old_record$sft_id[1]

        DBI::dbExecute(
          conn,
          paste0(
            "UPDATE ", sft_quote_identifier(conn, table_name),
            " SET ", sft_quote_identifier(conn, geom_column), " = ?, ",
            sft_quote_identifier(conn, "sft_updated_at"), " = ?, ",
            sft_quote_identifier(conn, "sft_updated_by"), " = ?",
            " WHERE sft_id = ?"
          ),
          params = list(
            encoded[i],
            sft_now(),
            sft_db_param(user),
            record_id
          )
        )

        new_record <- sft_get_record(
          conn = conn,
          form = form,
          record_id = record_id,
          include_deleted = TRUE
        )

        write_audit_log(
          conn = conn,
          form = form,
          action = "attach_shape",
          record_id = record_id,
          record_uuid = old_record$sft_uuid[1],
          old_data = old_record,
          new_data = new_record,
          changed_fields = geom_column,
          changed_by = user
        )

        attached <- attached + 1L
      }
    }
  })

  invisible(list(attached = attached, unmatched = unmatched))
}

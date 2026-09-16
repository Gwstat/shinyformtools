# Deprecation helpers. Old arguments keep working for one release: they are
# mapped onto their replacement and a warning is shown once per session per
# argument, so a running app is not flooded and a log still names every
# deprecated call site once.

.sft_deprecation_env <- new.env(parent = emptyenv())

# Warn once per session that `what` is deprecated and `instead` replaces it.
# `id` keys the once-only memory; it defaults to `what`.
sft_deprecate_warn <- function(what, instead, id = what) {
  if (isTRUE(.sft_deprecation_env[[id]])) {
    return(invisible(FALSE))
  }

  assign(id, TRUE, envir = .sft_deprecation_env)

  warning(
    what, " is deprecated in shinyformtools and will be removed in a future ",
    "release. Use ", instead, " instead.",
    call. = FALSE
  )

  invisible(TRUE)
}

# Reset the once-per-session memory (tests).
sft_reset_deprecation_warnings <- function() {
  rm(list = ls(.sft_deprecation_env), envir = .sft_deprecation_env)
  invisible(NULL)
}

# Map the deprecated arguments a function collected in `...` onto their new
# homes. `mapping` is a named list, one entry per old argument name:
#   list(bundle = "permissions", key = "can_add")  -> bundle[[key]]
#   list(bundle = NULL, key = "form_layout")       -> a plain argument
#   list(bundle = NULL, key = NULL)                -> ignored (no replacement)
# Returns list(bundles = <named list of named lists>, args = <named list>).
# Any name in `dots` that the mapping does not know is an error, so a typo does
# not silently vanish into `...` (R would have reported "unused argument").
sft_map_deprecated_args <- function(dots, mapping, fn) {
  bundles <- list()
  args <- list()

  if (length(dots) == 0L) {
    return(list(bundles = bundles, args = args))
  }

  unknown <- setdiff(names(dots), names(mapping))
  if (length(unknown) > 0L || is.null(names(dots)) || any(!nzchar(names(dots)))) {
    stop(
      "unused argument", if (length(unknown) != 1L) "s" else "", " in ", fn, "(): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  for (old in names(dots)) {
    target <- mapping[[old]]

    if (is.null(target$key)) {
      sft_deprecate_warn(
        what = paste0("`", fn, "(", old, " = )`"),
        instead = "nothing (the argument has no effect)",
        id = paste0(fn, ":", old)
      )
      next
    }

    if (is.null(target$bundle)) {
      sft_deprecate_warn(
        what = paste0("`", fn, "(", old, " = )`"),
        instead = paste0("`", fn, "(", target$key, " = )`"),
        id = paste0(fn, ":", old)
      )
      args[[target$key]] <- dots[[old]]
      next
    }

    sft_deprecate_warn(
      what = paste0("`", fn, "(", old, " = )`"),
      instead = paste0("`", fn, "(", target$bundle, " = list(", target$key, " = ))`"),
      id = paste0(fn, ":", old)
    )

    if (is.null(bundles[[target$bundle]])) {
      bundles[[target$bundle]] <- list()
    }
    bundles[[target$bundle]][[target$key]] <- dots[[old]]
  }

  list(bundles = bundles, args = args)
}

# Resolve one option bundle: validate its names against `defaults`, let the
# explicit bundle win over values that arrived through deprecated arguments,
# and fill in the defaults. A misspelt key is an error rather than a silent
# no-op, which matters most for permissions.
sft_resolve_bundle <- function(bundle, legacy, defaults, name) {
  if (is.null(bundle)) {
    bundle <- list()
  }

  if (!is.list(bundle)) {
    stop(name, " must be a named list.", call. = FALSE)
  }

  if (length(bundle) > 0L && (is.null(names(bundle)) || any(!nzchar(names(bundle))))) {
    stop("Every entry of ", name, " must be named.", call. = FALSE)
  }

  unknown <- setdiff(names(bundle), names(defaults))
  if (length(unknown) > 0L) {
    stop(
      "Unknown ", name, " entr", if (length(unknown) == 1L) "y" else "ies", ": ",
      paste(unknown, collapse = ", "), ". Allowed: ",
      paste(names(defaults), collapse = ", "), ".",
      call. = FALSE
    )
  }

  resolved <- defaults

  for (key in names(legacy)) {
    resolved[key] <- list(legacy[[key]])
  }

  for (key in names(bundle)) {
    resolved[key] <- list(bundle[[key]])
  }

  resolved
}

# The three schema functions used to take (conn, form); they now take
# (form, conn) like every other function in the package. A call in the old
# order is recognised by the classes of the two objects, swapped and warned
# about once.
sft_accept_swapped_form_conn <- function(form, conn, fn) {
  if (inherits(form, "DBIConnection") && inherits(conn, "sft_form")) {
    sft_deprecate_warn(
      what = paste0("Calling `", fn, "(conn, form)`"),
      instead = paste0("`", fn, "(form, conn)`"),
      id = paste0(fn, ":order")
    )
    return(list(form = conn, conn = form))
  }

  list(form = form, conn = conn)
}

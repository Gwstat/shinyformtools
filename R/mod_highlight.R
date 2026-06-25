# Reactive field/tab highlighting for add/edit forms.
#
# Two independent channels share one client-side handler:
#   * "highlight" - a caller-driven red glow on chosen fields (and, optionally,
#     the tab that holds them). Active in both the add and edit forms.
#   * "changed"   - an automatic blue glow on edit-form fields whose current
#     value differs from the stored record value.
#
# Each channel owns its own CSS class and CSS custom property, so they coexist
# on the same field without interfering. The colour is pushed inline by the
# client handler (via the custom property), so it is fully controlled from the
# server with no UI-side argument.

sft_highlight_field_class <- "sft-field-highlight"
sft_highlight_tab_class <- "sft-tab-highlight"
sft_highlight_css_var <- "--sft-highlight-color"

sft_changed_field_class <- "sft-field-changed"
sft_changed_tab_class <- "sft-tab-changed"
sft_changed_css_var <- "--sft-changed-color"

# Static class definitions plus the one-time custom-message handler. Injected by
# form_ui(); the colour comes through the CSS custom property set inline by the
# handler, so the defaults below are only fallbacks.
sft_highlight_css <- function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      "
      .sft-field-highlight {
        box-shadow: 0 0 0 2px var(--sft-highlight-color, #dc3545),
                    0 0 8px 3px var(--sft-highlight-color, #dc3545);
        border-radius: 4px;
        transition: box-shadow 0.15s ease-in-out;
      }
      .sft-field-changed {
        box-shadow: 0 0 0 2px var(--sft-changed-color, #2b8cff),
                    0 0 8px 3px var(--sft-changed-color, #2b8cff);
        border-radius: 4px;
        transition: box-shadow 0.15s ease-in-out;
      }
      .nav-tabs > li > a.sft-tab-highlight,
      .nav-tabs > li > a.sft-tab-changed {
        font-weight: 700;
      }
      .nav-tabs > li > a.sft-tab-highlight {
        box-shadow: inset 0 -3px 0 var(--sft-highlight-color, #dc3545);
      }
      .nav-tabs > li > a.sft-tab-changed {
        box-shadow: inset 0 -3px 0 var(--sft-changed-color, #2b8cff);
      }
      "
    )),
    shiny::tags$script(shiny::HTML(
      "
      (function() {
        // Cache the latest payload per channel (keyed by field class), so the
        // highlight can be re-applied to add/edit form elements that only enter
        // the DOM when a dialog opens - the control that drives it usually lives
        // on the page behind the (modal) dialog and is chosen beforehand.
        window.sftHighlightState = window.sftHighlightState || {};

        function clearClass(cls, cssVar) {
          if (!cls) return;
          var nodes = document.querySelectorAll('.' + cls);
          for (var i = 0; i < nodes.length; i++) {
            nodes[i].classList.remove(cls);
            if (cssVar) nodes[i].style.removeProperty(cssVar);
          }
        }

        function tabLinkForPane(pane) {
          if (!pane || !pane.id) return null;
          return document.querySelector(
            '.nav-tabs a[data-toggle=\"tab\"][href=\"#' + pane.id + '\"]'
          ) || document.querySelector('.nav-tabs a[href=\"#' + pane.id + '\"]');
        }

        function applyHighlight(message) {
          if (!message) return;

          var ids = message.ids || [];
          var fieldClass = message.fieldClass;
          var tabClass = message.tabClass;
          var cssVar = message.cssVar;
          var color = message.color;
          var highlightTab = message.highlightTab;

          clearClass(fieldClass, cssVar);
          clearClass(tabClass, cssVar);

          for (var i = 0; i < ids.length; i++) {
            var el = document.getElementById(ids[i]);
            if (!el) continue;

            if (color && cssVar) el.style.setProperty(cssVar, color);
            el.classList.add(fieldClass);

            if (highlightTab && tabClass) {
              var pane = el.closest ? el.closest('.tab-pane') : null;
              var link = tabLinkForPane(pane);
              if (link) {
                if (color && cssVar) link.style.setProperty(cssVar, color);
                link.classList.add(tabClass);
              }
            }
          }
        }

        function reapplyAll() {
          var state = window.sftHighlightState;
          for (var key in state) {
            if (Object.prototype.hasOwnProperty.call(state, key)) {
              applyHighlight(state[key]);
            }
          }
        }

        var reapplyScheduled = false;
        function scheduleReapply() {
          if (reapplyScheduled) return;
          reapplyScheduled = true;
          window.setTimeout(function() {
            reapplyScheduled = false;
            reapplyAll();
          }, 30);
        }

        function registerSftHighlightHandler() {
          if (window.sftHighlightHandlerRegistered) return;

          if (!window.Shiny) {
            window.setTimeout(registerSftHighlightHandler, 100);
            return;
          }

          window.sftHighlightHandlerRegistered = true;

          Shiny.addCustomMessageHandler('sftHighlight', function(message) {
            // Only persistent channels (the caller-driven highlight) are cached
            // for re-application on dialog open; the live 'changed' channel is
            // recomputed by the server while the dialog is open, so caching it
            // would risk flashing a stale glow on the next open.
            if (message && message.fieldClass && message.persist) {
              window.sftHighlightState[message.fieldClass] = message;
            }
            applyHighlight(message);
          });

          // Re-apply cached highlights once a dialog is shown (modal layout) or
          // when an inline form re-renders, since those elements appear late.
          if (window.jQuery) {
            jQuery(document).on('shown.bs.modal', scheduleReapply);
            jQuery(document).on('shiny:value', scheduleReapply);
          }
        }

        registerSftHighlightHandler();
      })();
      "
    ))
  )
}

# Best-effort scalar/vector normalisation for change detection. Values arrive
# from Shiny inputs (numeric, logical, Date, character, possibly multi-valued)
# and from the stored record (often character), so a plain identical() would
# report spurious changes. We collapse each side to a canonical string.
sft_norm_value <- function(x) {
  if (is.null(x)) {
    return("")
  }

  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return("")
  }

  sep <- ""

  if (is.logical(x)) {
    return(paste(as.integer(x), collapse = sep))
  }

  if (is.numeric(x)) {
    return(paste(format(x, scientific = FALSE, trim = TRUE), collapse = sep))
  }

  paste(trimws(as.character(x)), collapse = sep)
}

sft_values_differ <- function(current, original) {
  !identical(sft_norm_value(current), sft_norm_value(original))
}

# Map a set of field ids to the namespaced container element ids the client
# handler targets. add/edit forms wrap each field in
# "sft_field_container_<prefix><id>".
sft_highlight_container_ids <- function(ns, field_ids, prefixes) {
  if (length(field_ids) == 0L) {
    return(character())
  }

  ids <- character()

  for (prefix in prefixes) {
    ids <- c(
      ids,
      vapply(
        field_ids,
        function(field_id) ns(paste0("sft_field_container_", prefix, field_id)),
        character(1),
        USE.NAMES = FALSE
      )
    )
  }

  # Keep the result unnamed: a named vector serialises to a JSON *object* in the
  # custom message, and the client handler iterates it as an array (ids.length /
  # ids[i]), so names would silently break the highlight.
  unname(ids)
}

# Resolve a highlight_fields argument (NULL / character / function / reactive)
# to a character vector of field ids. Unlike sft_module_value(), an empty result
# is preserved (it means "clear all highlights").
sft_resolve_highlight_fields <- function(highlight_fields) {
  value <- if (is.function(highlight_fields)) {
    highlight_fields()
  } else {
    highlight_fields
  }

  as.character(value %||% character())
}

# Register the two highlight observers on the module session. Non-namespaced
# registrar, called from form_server(); covered by testServer tests.
sft_register_highlight <- function(input,
                                   session,
                                   form,
                                   current_edit_row,
                                   highlight_fields = NULL,
                                   highlight_tab = TRUE,
                                   highlight_color = "#dc3545",
                                   show_changed = TRUE,
                                   changed_color = "#2b8cff") {
  ns <- session$ns

  send_highlight <- function(ids, field_class, tab_class, css_var, color, do_tab, persist) {
    session$sendCustomMessage(
      type = "sftHighlight",
      # unname() so the ids serialise to a JSON array (the client iterates them
      # by index); as.list() forces an array even for a single id.
      message = list(
        ids = as.list(unname(ids)),
        fieldClass = field_class,
        tabClass = tab_class,
        cssVar = css_var,
        color = color,
        highlightTab = isTRUE(do_tab),
        persist = isTRUE(persist)
      )
    )
  }

  # Channel 1: caller-driven red glow on chosen fields, in both add and edit.
  if (!is.null(highlight_fields)) {
    shiny::observe({
      field_ids <- sft_resolve_highlight_fields(highlight_fields)

      send_highlight(
        ids = sft_highlight_container_ids(ns, field_ids, c("add_", "edit_")),
        field_class = sft_highlight_field_class,
        tab_class = sft_highlight_tab_class,
        css_var = sft_highlight_css_var,
        color = highlight_color,
        do_tab = highlight_tab,
        persist = TRUE
      )
    })
  }

  # Channel 2: automatic blue glow on edit fields whose value changed.
  if (isTRUE(show_changed)) {
    shiny::observe({
      row <- current_edit_row()

      changed_ids <- character()

      if (!is.null(row)) {
        for (field in sft_active_input_fields(form)) {
          input_value <- input[[paste0("edit_", field$id)]]

          if (is.null(input_value)) {
            next
          }

          original <- sft_field_value_from_record(row, field)

          if (sft_values_differ(input_value, original)) {
            changed_ids <- c(changed_ids, field$id)
          }
        }
      }

      send_highlight(
        ids = sft_highlight_container_ids(ns, changed_ids, "edit_"),
        field_class = sft_changed_field_class,
        tab_class = sft_changed_tab_class,
        css_var = sft_changed_css_var,
        color = changed_color,
        do_tab = highlight_tab,
        persist = FALSE
      )
    })
  }
}

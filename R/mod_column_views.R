# Column-view modal and drag/drop selection UI helpers.

# Resolve the effective column set for a named view, honouring saved/predefined
# views, the persistence setting, and whether system columns are shown. `data`
# is the already-fetched display data frame (the caller reads the reactive).
sft_resolve_column_view_columns <- function(view_name,
                                            conn,
                                            form,
                                            user,
                                            table_views,
                                            persist_column_settings,
                                            data,
                                            show_system_columns) {
  view_name <- sft_column_view_key(view_name %||% "Standard")

  saved_columns <- if (isTRUE(persist_column_settings)) {
    sft_resolve_saved_column_view(
      conn = conn,
      form = form,
      user = user,
      table_views = table_views,
      view_name = view_name
    )
  } else {
    sft_table_view_columns(
      table_views = table_views,
      view_name = view_name
    )
  }

  sft_resolve_record_columns(
    form = form,
    data = data,
    columns = saved_columns,
    show_system_columns = show_system_columns
  )
}

# Column-view names to offer in the views modal: predefined views plus (when
# persistence is on) shared saved views, always including "Standard".
sft_column_view_names <- function(table_views,
                                  persist_column_settings,
                                  conn,
                                  form) {
  predefined_view_names <- sft_table_view_names(table_views)

  if (isTRUE(persist_column_settings)) {
    unique(c(
      predefined_view_names,
      sft_available_shared_column_view_names(
        conn = conn,
        form = form
      )
    ))
  } else {
    unique(c("Standard", predefined_view_names))
  }
}

sft_modal_button_if_label <- function(labels, key) {
  label <- sft_ui_label(labels, key)

  if (is.null(label)) {
    return(NULL)
  }

  shiny::modalButton(label)
}

sft_column_settings_widget <- function(session,
                                       labels,
                                       choices,
                                       selected,
                                       root_input_id = "column_settings_widget",
                                       order_input_id = "column_settings_order") {
  ns <- session$ns
  values <- unname(choices)
  choice_labels <- names(choices)

  selected <- intersect(as.character(selected), values)
  ordered_values <- c(selected, setdiff(values, selected))

  column_data <- lapply(
    ordered_values,
    function(value) {
      index <- match(value, values)
      list(
        value = value,
        label = choice_labels[[index]],
        selected = value %in% selected
      )
    }
  )

  data_json <- jsonlite::toJSON(
    column_data,
    auto_unbox = TRUE,
    null = "null"
  )

  text_json <- jsonlite::toJSON(
    list(
      empty = sft_ui_label(labels, "column_settings_empty"),
      add = sft_ui_label(labels, "column_settings_add_column"),
      remove = sft_ui_label(labels, "column_settings_remove_column")
    ),
    auto_unbox = TRUE,
    null = "null"
  )

  root_id <- ns(root_input_id)
  input_id <- ns(order_input_id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(
      "
      .sft-column-settings-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 1rem;
        align-items: start;
      }
      .sft-column-settings-box h5 {
        margin-top: 0;
      }
      .sft-column-list {
        list-style: none;
        padding: 0.5rem;
        margin: 0;
        border: 1px solid #ddd;
        border-radius: 4px;
        min-height: 10rem;
        max-height: 45vh;
        overflow-y: auto;
        background: #fff;
      }
      .sft-column-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.5rem;
        padding: 0.35rem 0.5rem;
        margin-bottom: 0.35rem;
        border: 1px solid #ddd;
        border-radius: 4px;
        background: #f8f8f8;
      }
      .sft-column-item:last-child {
        margin-bottom: 0;
      }
      .sft-column-item[data-selected='true'] {
        cursor: grab;
      }
      .sft-column-item[data-selected='true']:active {
        cursor: grabbing;
      }
      .sft-column-item-label {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .sft-column-item-handle {
        color: #777;
        margin-right: 0.35rem;
      }
      .sft-column-item-button {
        border: 1px solid #ccc;
        border-radius: 3px;
        background: #fff;
        min-width: 2rem;
        height: 1.8rem;
        line-height: 1;
      }
      .sft-column-empty {
        color: #777;
        padding: 0.5rem;
        font-style: italic;
      }
      @media (max-width: 800px) {
        .sft-column-settings-grid {
          grid-template-columns: 1fr;
        }
      }
      "
    )),
    shiny::div(
      id = root_id,
      class = "sft-column-settings-widget",
      shiny::div(
        class = "sft-column-settings-grid",
        shiny::div(
          class = "sft-column-settings-box",
          shiny::tags$h5(sft_ui_label(labels, "column_settings_available")),
          shiny::tags$ul(
            class = "sft-column-list",
            `data-role` = "available"
          )
        ),
        shiny::div(
          class = "sft-column-settings-box",
          shiny::tags$h5(sft_ui_label(labels, "column_settings_selected")),
          shiny::tags$ul(
            class = "sft-column-list",
            `data-role` = "selected"
          )
        )
      )
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "
      (function() {
        var root = document.getElementById(%s);
        if (!root || root.dataset.sftInitialized === 'true') return;
        root.dataset.sftInitialized = 'true';

        var columns = %s;
        var texts = %s;
        var availableList = root.querySelector('[data-role=available]');
        var selectedList = root.querySelector('[data-role=selected]');
        var inputId = %s;
        var dragged = null;

        function setEmptyMessages() {
          [availableList, selectedList].forEach(function(list) {
            var old = list.querySelector('.sft-column-empty');
            if (old) old.remove();
            if (list.querySelectorAll('li.sft-column-item').length === 0) {
              var empty = document.createElement('li');
              empty.className = 'sft-column-empty';
              empty.textContent = texts.empty || 'No columns';
              list.appendChild(empty);
            }
          });
        }

        function updateInput() {
          var order = Array.prototype.map.call(
            selectedList.querySelectorAll('li.sft-column-item'),
            function(item) { return item.dataset.value; }
          );

          if (window.Shiny) {
            Shiny.setInputValue(inputId, order, {priority: 'event'});
          }

          setEmptyMessages();
        }

        function createItem(column, selected) {
          var item = document.createElement('li');
          item.className = 'sft-column-item';
          item.dataset.value = column.value;
          item.dataset.selected = selected ? 'true' : 'false';
          item.draggable = !!selected;

          var text = document.createElement('span');
          text.className = 'sft-column-item-label';

          if (selected) {
            var handle = document.createElement('span');
            handle.className = 'sft-column-item-handle';
            handle.textContent = '\u2195';
            text.appendChild(handle);
          }

          var label = document.createElement('span');
          label.textContent = column.label;
          text.appendChild(label);

          var button = document.createElement('button');
          button.type = 'button';
          button.className = 'sft-column-item-button';
          button.textContent = selected ? '\u2212' : '+';
          button.title = selected ? (texts.remove || 'Remove from table') : (texts.add || 'Show in table');
          button.addEventListener('click', function() {
            item.remove();
            if (selected) {
              availableList.appendChild(createItem(column, false));
            } else {
              selectedList.appendChild(createItem(column, true));
            }
            updateInput();
          });

          item.appendChild(text);
          item.appendChild(button);

          return item;
        }

        function getAfterElement(container, y) {
          var draggableElements = Array.prototype.slice.call(
            container.querySelectorAll('li.sft-column-item:not(.sft-dragging)')
          );

          return draggableElements.reduce(function(closest, child) {
            var box = child.getBoundingClientRect();
            var offset = y - box.top - box.height / 2;

            if (offset < 0 && offset > closest.offset) {
              return {offset: offset, element: child};
            }

            return closest;
          }, {offset: Number.NEGATIVE_INFINITY, element: null}).element;
        }

        selectedList.addEventListener('dragstart', function(event) {
          var item = event.target.closest('li.sft-column-item');
          if (!item) return;
          dragged = item;
          item.classList.add('sft-dragging');
          event.dataTransfer.effectAllowed = 'move';
        });

        selectedList.addEventListener('dragover', function(event) {
          if (!dragged) return;
          event.preventDefault();
          var after = getAfterElement(selectedList, event.clientY);
          if (after == null) {
            selectedList.appendChild(dragged);
          } else {
            selectedList.insertBefore(dragged, after);
          }
        });

        selectedList.addEventListener('drop', function(event) {
          event.preventDefault();
          updateInput();
        });

        selectedList.addEventListener('dragend', function() {
          if (dragged) dragged.classList.remove('sft-dragging');
          dragged = null;
          updateInput();
        });

        columns.forEach(function(column) {
          if (column.selected) {
            selectedList.appendChild(createItem(column, true));
          } else {
            availableList.appendChild(createItem(column, false));
          }
        });

        updateInput();
      })();
      ",
      jsonlite::toJSON(root_id, auto_unbox = TRUE),
      data_json,
      text_json,
      jsonlite::toJSON(input_id, auto_unbox = TRUE)
    )))
  )
}

sft_show_column_views_modal <- function(session,
                                        labels,
                                        choices,
                                        selected,
                                        view_names,
                                        active_view,
                                        modal_sizes,
                                        can_save = FALSE) {
  ns <- session$ns

  if (length(view_names) == 0L) {
    view_names <- "Standard"
  }

  view_names <- unique(c("Standard", view_names))
  active_view <- sft_column_view_key(active_view)

  if (!active_view %in% view_names) {
    view_names <- c(active_view, view_names)
  }

  shiny::showModal(
    shiny::modalDialog(
      title = sft_ui_label(labels, "column_selection_title"),
      size = sft_modal_size(modal_sizes, "column_selection"),
      easyClose = TRUE,
      sft_modal_css(modal_sizes, "column_selection"),
      shiny::p(sft_ui_label(labels, "column_selection_help")),
      shiny::fluidRow(
        shiny::column(
          width = if (isTRUE(can_save)) 6 else 12,
          shiny::selectInput(
            inputId = ns("column_settings_view"),
            label = sft_ui_label(labels, "column_settings_view"),
            choices = view_names,
            selected = active_view
          )
        ),
        if (isTRUE(can_save)) {
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = ns("column_settings_view_name"),
              label = sft_ui_label(labels, "column_settings_view_name"),
              value = if (identical(active_view, "Standard")) "" else active_view,
              placeholder = sft_ui_label(labels, "column_settings_view_name_placeholder")
            )
          )
        }
      ),
      shiny::p(shiny::tags$small(sft_ui_label(labels, "column_selection_free_help"))),
      shiny::uiOutput(ns("column_settings_widget_ui")),
      footer = shiny::tagList(
        sft_modal_button_if_label(labels, "cancel"),
        sft_action_button_if_label(ns, "load_column_view", labels, "column_settings_load_view"),
        if (isTRUE(can_save)) {
          sft_action_button_if_label(ns, "save_column_view", labels, "save_column_view")
        }
      )
    )
  )
}

sft_show_column_settings_modal <- function(session,
                                           labels,
                                           choices,
                                           selected,
                                           view_names,
                                           active_view,
                                           modal_sizes) {
  sft_show_column_views_modal(
    session = session,
    labels = labels,
    choices = choices,
    selected = selected,
    view_names = view_names,
    active_view = active_view,
    modal_sizes = modal_sizes,
    can_save = TRUE
  )
}

sft_show_column_selection_modal <- function(session,
                                            labels,
                                            choices,
                                            selected,
                                            view_names,
                                            active_view,
                                            modal_sizes) {
  sft_show_column_views_modal(
    session = session,
    labels = labels,
    choices = choices,
    selected = selected,
    view_names = view_names,
    active_view = active_view,
    modal_sizes = modal_sizes,
    can_save = FALSE
  )
}

# Column-view selection / column-settings reactive glue for the form module.
#
# Registers the column-settings widget renderUI and the observers that open the
# views modal, load a view, and save a shared view. Like
# sft_register_deleted_versions(), this is NOT a namespaced Shiny module: it is
# called from within form_server() with that module's own input/output/
# session, so all input ids stay in the parent namespace (behaviour-preserving).
# Parent-owned state (set_record_columns, active_column_view) and the parent's
# reactives (display_records, column_choices, current_record_columns) are
# threaded in explicitly rather than captured.
sft_register_column_settings <- function(input,
                                          output,
                                          session,
                                          form,
                                          conn,
                                          user,
                                          labels,
                                          modal_sizes,
                                          table_views,
                                          persist_column_settings,
                                          show_system_columns,
                                          can_change_column_settings,
                                          can_select_column_view,
                                          display_records,
                                          column_choices,
                                          current_record_columns,
                                          active_column_view,
                                          set_record_columns) {
  resolve_column_view_columns <- function(view_name) {
    sft_resolve_column_view_columns(
      view_name = view_name,
      conn = conn,
      form = form,
      user = sft_module_current_user(input, user),
      table_views = table_views,
      persist_column_settings = persist_column_settings,
      data = display_records(),
      show_system_columns = show_system_columns
    )
  }

  load_column_view_by_name <- function(view_name) {
    view_name <- sft_column_view_key(view_name %||% "Standard")
    resolved_columns <- resolve_column_view_columns(view_name)

    set_record_columns(resolved_columns)
    active_column_view(view_name)

    if (isTRUE(persist_column_settings)) {
      sft_set_active_column_view(
        conn = conn,
        form = form,
        user = sft_module_current_user(input, user),
        view_name = view_name
      )
    }

    shiny::removeModal()
    shiny::showNotification(
      sft_ui_label(labels, "columns_loaded"),
      type = "message"
    )
  }

  output$column_settings_widget_ui <- shiny::renderUI({
    selected_view <- sft_column_view_key(input$column_settings_view %||% active_column_view())
    selected_columns <- if (!is.null(input$column_settings_view)) {
      resolve_column_view_columns(selected_view)
    } else {
      current_record_columns()
    }

    sft_column_settings_widget(
      session = session,
      labels = labels,
      choices = column_choices(),
      selected = selected_columns
    )
  })

  shiny::observeEvent(input$column_settings_view, {
    if (sft_module_permission(can_change_column_settings, default = TRUE)) {
      view_name <- sft_column_view_key(input$column_settings_view %||% "")
      shiny::updateTextInput(
        session = session,
        inputId = "column_settings_view_name",
        value = if (identical(view_name, "Standard")) "" else view_name
      )
    }
  }, ignoreInit = TRUE)

  show_column_views_modal <- function() {
    can_select_columns <- sft_module_permission(can_select_column_view, default = TRUE)
    can_save_columns <- sft_module_permission(can_change_column_settings, default = TRUE)

    if (!isTRUE(can_select_columns) && !isTRUE(can_save_columns)) {
      shiny::showNotification(
        sft_ui_label(labels, "column_selection_not_allowed"),
        type = "warning"
      )

      return()
    }

    view_names <- sft_column_view_names(
      table_views = table_views,
      persist_column_settings = persist_column_settings,
      conn = conn,
      form = form
    )

    sft_show_column_views_modal(
      session = session,
      labels = labels,
      choices = column_choices(),
      selected = current_record_columns(),
      view_names = view_names,
      active_view = active_column_view(),
      modal_sizes = modal_sizes,
      can_save = can_save_columns
    )
  }

  shiny::observeEvent(input$open_column_settings, {
    show_column_views_modal()
  })

  shiny::observeEvent(input$open_column_selection, {
    show_column_views_modal()
  })

  shiny::observeEvent(input$load_column_view, {
    if (!sft_module_permission(can_select_column_view, default = TRUE) &&
        !sft_module_permission(can_change_column_settings, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "column_selection_not_allowed"),
        type = "warning"
      )

      return()
    }

    load_column_view_by_name(input$column_settings_view %||% "Standard")
  })

  shiny::observeEvent(input$load_column_selection_view, {
    if (!sft_module_permission(can_select_column_view, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "column_selection_not_allowed"),
        type = "warning"
      )

      return()
    }

    load_column_view_by_name(input$column_selection_view %||% "Standard")
  })

  shiny::observeEvent(input$save_column_view, {
    if (!sft_module_permission(can_change_column_settings, default = TRUE)) {
      shiny::showNotification(
        sft_ui_label(labels, "column_settings_not_allowed"),
        type = "warning"
      )

      return()
    }

    selected_columns <- input$column_settings_order

    resolved_columns <- sft_resolve_record_columns(
      form = form,
      data = display_records(),
      columns = selected_columns,
      show_system_columns = show_system_columns
    )

    view_name <- sft_column_view_key(input$column_settings_view_name %||% "")

    if (!nzchar(view_name) || identical(view_name, "Standard")) {
      shiny::showNotification(
        sft_ui_label(labels, "standard_column_view_not_overwritable"),
        type = "warning"
      )

      return()
    }

    set_record_columns(resolved_columns)
    active_column_view(view_name)

    if (isTRUE(persist_column_settings)) {
      sft_set_shared_column_view(
        conn = conn,
        form = form,
        view_name = view_name,
        columns = resolved_columns
      )
    }

    shiny::removeModal()
    shiny::showNotification(
      sft_ui_label(labels, "columns_saved"),
      type = "message"
    )
  })

  invisible(NULL)
}

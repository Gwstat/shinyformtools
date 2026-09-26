/* Grid input for shinyformtools: a table of number cells with keyboard
   navigation, block paste from a spreadsheet, live row / column sums and a
   debounced value. The value sent to R is {rows, cols, values} with one
   array per row; an empty cell is null. */
(function () {
  var DEBOUNCE_MS = 500;

  function cells(root) {
    return Array.prototype.slice.call(root.querySelectorAll("input.sft-grid-cell"));
  }

  function cellAt(root, r, c) {
    return root.querySelector('input.sft-grid-cell[data-r="' + r + '"][data-c="' + c + '"]');
  }

  function parse(text) {
    if (text === null || text === undefined) return null;
    text = String(text).trim().replace(",", ".");
    if (!text.length) return null;
    var v = Number(text);
    return isNaN(v) ? null : v;
  }

  function values(root) {
    var rows = JSON.parse(root.dataset.rows || "[]");
    var cols = JSON.parse(root.dataset.cols || "[]");
    var out = [];
    for (var r = 0; r < rows.length; r++) {
      var line = [];
      for (var c = 0; c < cols.length; c++) {
        var el = cellAt(root, r, c);
        line.push(el ? parse(el.value) : null);
      }
      out.push(line);
    }
    return { rows: rows, cols: cols, values: out };
  }

  function fmt(x) {
    return Number.isInteger(x) ? String(x) : String(Math.round(x * 100) / 100);
  }

  function sums(root) {
    if (root.dataset.sums !== "true") return;
    var v = values(root);
    var colSum = v.cols.map(function () { return 0; });
    var total = 0;
    v.values.forEach(function (line, r) {
      var rowSum = 0;
      line.forEach(function (x, c) {
        if (x !== null) { rowSum += x; colSum[c] += x; total += x; }
      });
      var out = root.querySelector('[data-sum-row="' + r + '"]');
      if (out) out.textContent = rowSum ? fmt(rowSum) : "";
      var tr = out && out.closest("tr");
      if (tr) tr.classList.toggle("sft-grid-row-filled", rowSum !== 0);
    });
    colSum.forEach(function (s, c) {
      var out = root.querySelector('[data-sum-col="' + c + '"]');
      if (out) out.textContent = s ? fmt(s) : "";
    });
    var t = root.querySelector(".sft-grid-total");
    if (t) t.textContent = total ? fmt(total) : "";
  }

  function markFilled(el) {
    el.classList.toggle("sft-grid-filled", parse(el.value) !== null && parse(el.value) !== 0);
  }

  function setValues(root, matrix) {
    if (!matrix) return;
    matrix.forEach(function (line, r) {
      (line || []).forEach(function (x, c) {
        var el = cellAt(root, r, c);
        if (!el) return;
        el.value = (x === null || x === undefined) ? "" : x;
        markFilled(el);
      });
    });
    sums(root);
  }

  function setHint(root, matrix) {
    if (!matrix) return;
    matrix.forEach(function (line, r) {
      (line || []).forEach(function (x, c) {
        var el = cellAt(root, r, c);
        if (!el) return;
        var span = el.parentNode.querySelector(".sft-grid-hint");
        var show = x !== null && x !== undefined;
        if (show && !span) {
          span = document.createElement("span");
          span.className = "sft-grid-hint";
          el.parentNode.appendChild(span);
        }
        if (span) {
          span.textContent = show ? x : "";
          span.style.display = show ? "" : "none";
        }
      });
    });
  }

  function move(root, el, dr, dc) {
    var r = parseInt(el.dataset.r, 10) + dr;
    var c = parseInt(el.dataset.c, 10) + dc;
    var target = cellAt(root, r, c);
    if (target) { target.focus(); target.select(); }
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find(".sft-grid-input"); },
    getId: function (el) { return el.id; },
    getType: function () { return "shinyformtools.grid"; },
    initialize: function (el) { sums(el); },
    getValue: function (el) { return values(el); },
    setValue: function (el, value) { setValues(el, value && value.values ? value.values : value); },
    subscribe: function (el, callback) {
      $(el).on("input.sftGrid", "input.sft-grid-cell", function (e) {
        markFilled(e.target);
        sums(el);
        callback(true);
      });
      $(el).on("change.sftGrid", "input.sft-grid-cell", function () { callback(false); });
      $(el).on("sft-grid-paste.sftGrid", function () { callback(false); });
    },
    unsubscribe: function (el) { $(el).off(".sftGrid"); },
    getRatePolicy: function () { return { policy: "debounce", delay: DEBOUNCE_MS }; },
    receiveMessage: function (el, data) {
      if (data.values) setValues(el, data.values);
      if (data.hint) setHint(el, data.hint);
      if (data.values) $(el).trigger("sft-grid-paste");
    }
  });
  Shiny.inputBindings.register(binding, "shinyformtools.grid");

  /* Enter and the arrow keys walk the grid; Tab keeps the browser default. */
  document.addEventListener("keydown", function (e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains("sft-grid-cell")) return;
    var root = el.closest(".sft-grid-input");
    if (!root) return;
    if (e.key === "Enter" || e.key === "ArrowDown") { e.preventDefault(); move(root, el, 1, 0); }
    else if (e.key === "ArrowUp") { e.preventDefault(); move(root, el, -1, 0); }
    else if (e.key === "ArrowRight") { e.preventDefault(); move(root, el, 0, 1); }
    else if (e.key === "ArrowLeft") { e.preventDefault(); move(root, el, 0, -1); }
  });

  /* A block copied from a spreadsheet lands from the focused cell onwards. */
  document.addEventListener("paste", function (e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains("sft-grid-cell")) return;
    var text = (e.clipboardData || window.clipboardData).getData("text");
    if (!text || (text.indexOf("\t") === -1 && text.indexOf("\n") === -1)) return;
    e.preventDefault();
    var root = el.closest(".sft-grid-input");
    var r0 = parseInt(el.dataset.r, 10), c0 = parseInt(el.dataset.c, 10);
    text.replace(/\r/g, "").split("\n").forEach(function (line, i) {
      if (!line.length && i > 0) return;
      line.split("\t").forEach(function (val, j) {
        var target = cellAt(root, r0 + i, c0 + j);
        if (!target) return;
        var v = parse(val);
        target.value = v === null ? "" : v;
        markFilled(target);
      });
    });
    sums(root);
    $(root).trigger("sft-grid-paste");
  });
})();

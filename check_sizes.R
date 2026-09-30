#!/usr/bin/env Rscript
# check_sizes.R
#
# Checks sizes of deprecated Bioconductor experiment data packages.
#
# Downloads:
#   - Google Sheet (CSV)  -> package list where Deprecated == TRUE
#   - Bioconductor VIEWS  -> current version + source tarball path
# Then fires an HTTP HEAD request per tarball to read Content-Length.
# Writes results.json and index.html.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

SHEET_ID    <- "1xz0GUPTpnRcyCZ6hcP8D1qFGSmOPONN4fvNcwUtQNio"
SHEET_URL   <- paste0("https://docs.google.com/spreadsheets/d/", SHEET_ID,
                      "/export?format=csv")
VIEWS_URL   <- "https://bioconductor.org/packages/3.24/data/experiment/VIEWS"
BIOC_BASE   <- "https://bioconductor.org/packages/3.24/data/experiment/"
THRESHOLD   <- 100L   # MB


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

download_text <- function(url) {
  request(url) |>
    req_headers(`User-Agent` = "bioc-size-checker/1.0") |>
    req_perform() |>
    resp_body_string()
}

head_content_length <- function(url) {
  tryCatch({
    resp <- request(url) |>
      req_headers(`User-Agent` = "bioc-size-checker/1.0") |>
      req_method("HEAD") |>
      req_timeout(15) |>
      req_perform()
    cl <- resp_header(resp, "Content-Length")
    if (!is.null(cl) && !is.na(cl)) as.numeric(cl) else NA_real_
  }, error = function(e) {
    message("  WARN HEAD failed for ", url, ": ", conditionMessage(e))
    NA_real_
  })
}


# ---------------------------------------------------------------------------
# 1. Google Sheet -> deprecated package list
# ---------------------------------------------------------------------------

cat("Downloading Google Sheet CSV...\n")
sheet <- read.csv(text = download_text(SHEET_URL), stringsAsFactors = FALSE)
deprecated <- sheet[sheet$Deprecated %in% c(TRUE, "TRUE"), , drop = FALSE]
cat(sprintf("  %d packages with Deprecated == TRUE\n", nrow(deprecated)))


# ---------------------------------------------------------------------------
# 2. VIEWS -> version + source.ver lookup
#    read.dcf() is base R and speaks this format natively.
# ---------------------------------------------------------------------------

cat("Downloading VIEWS...\n")
views_raw <- download_text(VIEWS_URL)
tmp <- tempfile(fileext = ".dcf")
writeLines(views_raw, tmp)
views <- as.data.frame(
  read.dcf(tmp, fields = c("Package", "Version", "source.ver")),
  stringsAsFactors = FALSE
)
cat(sprintf("  %d packages in VIEWS\n", nrow(views)))


# ---------------------------------------------------------------------------
# 3. HEAD requests
# ---------------------------------------------------------------------------

results <- vector("list", nrow(deprecated))

for (i in seq_len(nrow(deprecated))) {
  name     <- deprecated$Package[i]
  old_size <- if ("size" %in% names(deprecated)) deprecated$size[i] else NA_character_
  hit      <- views[views$Package == name, , drop = FALSE]

  base <- list(
    package            = name,
    old_size           = old_size,
    current_size_bytes = NULL,
    current_size_mb    = NULL,
    under_100mb        = NULL,
    tarball_url        = NULL,
    version            = NULL
  )

  if (nrow(hit) == 0L) {
    cat(sprintf("  [%s] not found in VIEWS\n", name))
    results[[i]] <- modifyList(base, list(status = "not_in_views"))
    next
  }

  source_ver <- trimws(hit$source.ver[1])
  version    <- trimws(hit$Version[1])
  base$version <- version

  if (is.na(source_ver) || source_ver == "") {
    cat(sprintf("  [%s] no source.ver in VIEWS\n", name))
    results[[i]] <- modifyList(base, list(status = "no_source_ver"))
    next
  }

  tarball_url      <- paste0(BIOC_BASE, source_ver)
  base$tarball_url <- tarball_url
  cat(sprintf("  [%s %s] HEAD %s\n", name, version, tarball_url))

  size_bytes <- head_content_length(tarball_url)

  if (!is.na(size_bytes)) {
    size_mb <- round(size_bytes / 1024 / 1024, 1)
    results[[i]] <- modifyList(base, list(
      current_size_bytes = size_bytes,
      current_size_mb    = size_mb,
      under_100mb        = size_mb < THRESHOLD,
      status             = "ok"
    ))
  } else {
    results[[i]] <- modifyList(base, list(status = "head_failed"))
  }
}


# ---------------------------------------------------------------------------
# 4. Write results.json
# ---------------------------------------------------------------------------

payload <- list(
  updated      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  threshold_mb = THRESHOLD,
  packages     = results
)

write_json(payload, "results.json", auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nWrote results.json (%d packages)\n", length(results)))


# ---------------------------------------------------------------------------
# 5. Generate index.html
# ---------------------------------------------------------------------------

pkgs    <- results
n_total <- length(pkgs)
n_ok    <- sum(vapply(pkgs, \(p) isTRUE(p$under_100mb),  logical(1)))
n_over  <- sum(vapply(pkgs, \(p) identical(p$under_100mb, FALSE), logical(1)))
n_unk   <- n_total - n_ok - n_over
updated <- payload$updated

badge <- function(p) {
  if (isTRUE(p$under_100mb))
    '<span class="badge green">&#10003; Under 100 MB</span>'
  else if (identical(p$under_100mb, FALSE))
    sprintf('<span class="badge red">&#10007; %.1f MB</span>', p$current_size_mb)
  else
    sprintf('<span class="badge gray">? %s</span>', p$status)
}

size_cell <- function(p) {
  if (!is.null(p$current_size_mb) && !is.na(p$current_size_mb)) {
    pct   <- min(p$current_size_mb / THRESHOLD * 100, 100)
    color <- if (isTRUE(p$under_100mb)) "#22c55e" else "#ef4444"
    sprintf(
      '<div class="bar-wrap"><div class="bar" style="width:%.0f%%;background:%s"></div>
       <span class="bar-label">%.1f MB</span></div>',
      pct, color, p$current_size_mb
    )
  } else {
    '<span class="muted">—</span>'
  }
}

pkg_link <- function(p) {
  if (!is.null(p$tarball_url))
    sprintf('<a href="%s" target="_blank">%s</a>', p$tarball_url, p$package)
  else
    p$package
}

# Sort: still-over first, then under, then unknown
order_key <- function(p) {
  if (identical(p$under_100mb, FALSE)) 0L
  else if (isTRUE(p$under_100mb))     1L
  else                                 2L
}
pkgs <- pkgs[order(vapply(pkgs, order_key, integer(1)))]

rows <- paste(vapply(pkgs, function(p) {
  sprintf(
    "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    pkg_link(p),
    if (!is.null(p$version) && !is.na(p$version)) p$version else "<em>—</em>",
    if (!is.null(p$old_size) && !is.na(p$old_size)) p$old_size else "—",
    size_cell(p),
    badge(p)
  )
}, character(1)), collapse = "\n")

html <- sprintf('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Bioc Experiment Package Size Tracker</title>
<style>
  :root {
    --bg:#f8fafc;--surface:#fff;--border:#e2e8f0;
    --text:#0f172a;--muted:#64748b;
    --green:#16a34a;--green-bg:#dcfce7;
    --red:#dc2626;--red-bg:#fee2e2;
    --gray-bg:#f1f5f9;
  }
  @media(prefers-color-scheme:dark){
    :root{--bg:#0f172a;--surface:#1e293b;--border:#334155;
          --text:#f1f5f9;--muted:#94a3b8;
          --green:#4ade80;--green-bg:#14532d;
          --red:#f87171;--red-bg:#7f1d1d;--gray-bg:#1e293b;}
  }
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:system-ui,sans-serif;background:var(--bg);color:var(--text);padding:1.5rem}
  h1{font-size:1.4rem;font-weight:700;margin-bottom:.25rem}
  .subtitle{color:var(--muted);font-size:.875rem;margin-bottom:1.5rem}
  .stat-row{display:flex;gap:1rem;flex-wrap:wrap;margin-bottom:1.5rem}
  .stat{background:var(--surface);border:1px solid var(--border);border-radius:.5rem;
        padding:.75rem 1.25rem;min-width:120px}
  .stat .n{font-size:1.75rem;font-weight:700;line-height:1}
  .stat .label{font-size:.75rem;color:var(--muted);margin-top:.2rem}
  .stat.green .n{color:var(--green)}.stat.red .n{color:var(--red)}
  table{width:100%%;border-collapse:collapse;background:var(--surface);
        border:1px solid var(--border);border-radius:.5rem;overflow:hidden}
  th{background:var(--gray-bg);text-align:left;padding:.6rem 1rem;
     font-size:.75rem;font-weight:600;text-transform:uppercase;
     letter-spacing:.05em;color:var(--muted);border-bottom:1px solid var(--border)}
  td{padding:.6rem 1rem;font-size:.875rem;border-bottom:1px solid var(--border)}
  tr:last-child td{border-bottom:none}
  tr:hover td{background:var(--gray-bg)}
  a{color:#3b82f6;text-decoration:none}a:hover{text-decoration:underline}
  .badge{display:inline-block;padding:.2rem .6rem;border-radius:999px;
         font-size:.75rem;font-weight:600}
  .badge.green{background:var(--green-bg);color:var(--green)}
  .badge.red{background:var(--red-bg);color:var(--red)}
  .badge.gray{background:var(--gray-bg);color:var(--muted)}
  .bar-wrap{display:flex;align-items:center;gap:.5rem}
  .bar{height:8px;border-radius:4px;min-width:2px;max-width:200px}
  .bar-label{font-size:.8rem;white-space:nowrap}
  .muted{color:var(--muted)}
</style>
</head>
<body>
<h1>Bioc Experiment Package Size Tracker</h1>
<p class="subtitle">
  Deprecated packages tracked for reduction below %d MB.&nbsp;
  Last checked: <strong>%s</strong>
</p>
<div class="stat-row">
  <div class="stat green"><div class="n">%d</div><div class="label">Under %d MB &#10003;</div></div>
  <div class="stat red">  <div class="n">%d</div><div class="label">Still over %d MB</div></div>
  <div class="stat">      <div class="n">%d</div><div class="label">Unknown / error</div></div>
  <div class="stat">      <div class="n">%d</div><div class="label">Total tracked</div></div>
</div>
<table>
<thead>
  <tr><th>Package</th><th>Version</th><th>Old Size</th><th>Current Size</th><th>Status</th></tr>
</thead>
<tbody>
%s
</tbody>
</table>
</body>
</html>',
  THRESHOLD, updated,
  n_ok, THRESHOLD, n_over, THRESHOLD, n_unk, n_total,
  rows
)

writeLines(html, "index.html")
cat("Wrote index.html\n")

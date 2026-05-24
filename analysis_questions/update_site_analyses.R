library(yaml)

script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(sub("--file=", "", file_arg))
  } else {
    getwd()
  }
})

root_dir <- normalizePath(file.path(script_dir, ".."))
registry_path <- file.path(script_dir, "analyses.yml")
index_path <- file.path(root_dir, "docs", "index.html")
blog_feed_path <- file.path(root_dir, "docs", "blog", "feed.xml")
site_url <- "https://weather.karthiks.co/"

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

is_true <- function(x) {
  isTRUE(x) || identical(tolower(as.character(x)), "true")
}

render_links <- function(links) {
  if (length(links) == 0) return(character())

  link_html <- vapply(links, function(link) {
    sprintf(
      '              <a href="%s">%s</a>',
      escape_html(link$href),
      escape_html(link$label)
    )
  }, character(1))

  c('            <div class="links">', link_html, '            </div>')
}

render_item <- function(item) {
  width <- item$image_width %||% ""
  height <- item$image_height %||% ""
  post_href <- item$blog_url

  c(
    '        <article class="analysis-item">',
    sprintf('          <a href="%s" aria-label="Read blog post: %s">', escape_html(post_href), escape_html(item$title)),
    sprintf(
      '            <img src="%s" width="%s" height="%s" loading="lazy" alt="%s">',
      escape_html(item$image),
      escape_html(width),
      escape_html(height),
      escape_html(item$image_alt %||% item$title)
    ),
    '          </a>',
    '          <div>',
    sprintf('            <a class="analysis-copy-link" href="%s">', escape_html(post_href)),
    sprintf('              <h3>%s</h3>', escape_html(item$title)),
    '              <p>',
    sprintf('                %s', escape_html(item$summary)),
    '              </p>',
    '            </a>',
    render_links(item$links),
    '          </div>',
    '        </article>'
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

items <- yaml::read_yaml(registry_path)
published <- Filter(function(item) is_true(item$publish) && is_true(item$insightful), items)

if (length(published) == 0) {
  stop("No analyses have both publish: true and insightful: true.")
}

missing_fields <- c("slug", "title", "summary", "image", "blog_url")
for (item in published) {
  missing <- missing_fields[vapply(missing_fields, function(field) {
    is.null(item[[field]]) || !nzchar(as.character(item[[field]]))
  }, logical(1))]
  if (length(missing) > 0) {
    stop("Analysis entry is missing required fields: ", paste(missing, collapse = ", "))
  }

  blog_path <- file.path(root_dir, "docs", item$blog_url)
  if (!file.exists(blog_path)) {
    stop("Analysis entry '", item$slug, "' points to missing blog post: ", item$blog_url)
  }

  blog_html <- paste(readLines(blog_path, warn = FALSE), collapse = "\n")
  if (!grepl("This post is AI-written.", blog_html, fixed = TRUE)) {
    stop("Blog post for analysis entry '", item$slug, "' is missing the AI-written disclosure.")
  }
}

if (!file.exists(blog_feed_path)) {
  stop("Missing blog RSS feed: docs/blog/feed.xml")
}

blog_feed <- paste(readLines(blog_feed_path, warn = FALSE), collapse = "\n")
for (item in published) {
  absolute_blog_url <- paste0(site_url, item$blog_url)
  if (!grepl(absolute_blog_url, blog_feed, fixed = TRUE)) {
    stop("Blog RSS feed is missing analysis entry '", item$slug, "': ", absolute_blog_url)
  }
}

html <- readLines(index_path, warn = FALSE)
start_marker <- "        <!-- ANALYSES:START -->"
end_marker <- "        <!-- ANALYSES:END -->"
start <- match(start_marker, html)
end <- match(end_marker, html)

if (is.na(start) || is.na(end) || start >= end) {
  stop("Could not find analysis markers in docs/index.html.")
}

rendered <- unlist(lapply(published, render_item), use.names = FALSE)
updated <- c(html[seq_len(start)], rendered, html[end:length(html)])
writeLines(updated, index_path)

message("Updated ", index_path, " with ", length(published), " published analyses.")

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

rss_cdata <- function(x) {
  paste0("<![CDATA[", gsub("]]>", "]]]]><![CDATA[>", x, fixed = TRUE), "]]>")
}

rss_date <- function(x) {
  format(as.POSIXct(as.Date(x), tz = "UTC"), "%a, %d %b %Y 00:00:00 GMT")
}

absolute_blog_html <- function(html) {
  blog_url <- paste0(site_url, "blog/")
  html <- gsub('src="../', paste0('src="', site_url), html, fixed = TRUE)
  html <- gsub('href="../', paste0('href="', site_url), html, fixed = TRUE)
  html <- gsub('href="./"', paste0('href="', blog_url, '"'), html, fixed = TRUE)
  html <- gsub('href="feed.xml"', paste0('href="', blog_url, 'feed.xml"'), html, fixed = TRUE)
  html
}

extract_article_html <- function(blog_path) {
  doc <- xml2::read_html(blog_path)
  article <- xml2::xml_find_first(doc, ".//article")
  if (inherits(article, "xml_missing")) {
    stop("Blog post has no <article>: ", blog_path)
  }
  absolute_blog_html(paste(as.character(xml2::xml_contents(article)), collapse = "\n"))
}

read_existing_pub_dates <- function(feed_path) {
  if (!file.exists(feed_path)) return(list())

  doc <- tryCatch(xml2::read_xml(feed_path), error = function(e) NULL)
  if (is.null(doc)) return(list())

  items <- xml2::xml_find_all(doc, ".//item")
  dates <- list()
  for (node in items) {
    guid <- xml2::xml_text(xml2::xml_find_first(node, "./guid"))
    pub_date <- xml2::xml_text(xml2::xml_find_first(node, "./pubDate"))
    if (nzchar(guid) && nzchar(pub_date)) {
      dates[[guid]] <- pub_date
    }
  }
  dates
}

render_blog_feed <- function(items, pub_dates) {
  feed_items <- unlist(lapply(items, function(item) {
    absolute_blog_url <- paste0(site_url, item$blog_url)
    blog_path <- file.path(root_dir, "docs", item$blog_url)
    pub_date <- pub_dates[[absolute_blog_url]] %||% rss_date(item$date %||% file.info(blog_path)$mtime)
    content <- extract_article_html(blog_path)

    c(
      "    <item>",
      paste0("      <title>", escape_html(item$title), "</title>"),
      paste0("      <link>", absolute_blog_url, "</link>"),
      paste0("      <guid>", absolute_blog_url, "</guid>"),
      paste0("      <pubDate>", pub_date, "</pubDate>"),
      paste0("      <description>", escape_html(item$summary), "</description>"),
      paste0("      <content:encoded>", rss_cdata(content), "</content:encoded>"),
      "    </item>"
    )
  }), use.names = FALSE)

  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<rss version=\"2.0\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\">\n",
    "  <channel>\n",
    "    <title>Bangalore Weather Blog</title>\n",
    "    <link>https://weather.karthiks.co/blog/</link>\n",
    "    <description>Short Bangalore weather analysis posts from the long-running hourly archive.</description>\n",
    "    <language>en</language>\n",
    "    <lastBuildDate>", rss_date(Sys.Date()), "</lastBuildDate>\n",
    paste(feed_items, collapse = "\n"), "\n",
    "  </channel>\n",
    "</rss>"
  )
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

blog_pub_dates <- read_existing_pub_dates(blog_feed_path)
writeLines(render_blog_feed(published, blog_pub_dates), blog_feed_path)

html <- readLines(index_path, warn = FALSE)
start_marker <- "        <!-- ANALYSES:START -->"
end_marker <- "        <!-- ANALYSES:END -->"
start <- match(start_marker, html)
end <- match(end_marker, html)

if (is.na(start) || is.na(end) || start >= end) {
  stop("Could not find analysis markers in docs/index.html.")
}

homepage_items <- head(published, 3)
rendered <- unlist(lapply(homepage_items, render_item), use.names = FALSE)
updated <- c(html[seq_len(start)], rendered, html[end:length(html)])
writeLines(updated, index_path)

message("Updated ", index_path, " with ", length(homepage_items), " homepage analyses from ", length(published), " published analyses.")
message("Updated ", blog_feed_path, " with full-content RSS items.")

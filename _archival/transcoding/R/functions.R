# R/functions.R

trim_header <- function(file) {
  require(xfun)
  gsub_file(file, "<!DOCTYPE HTML>", "")
  gsub_file(file, "<html><head>", "")
  gsub_file(file, '<link rel="shortcut icon" href="/web/icons/favicon.png">', "")
  gsub_file(file, '<link rel="start" href="/"><link rel="about" href="//databrary.org/about.html">', "")
  gsub_file(file, '<link rel="support" href="//databrary.org/support.html"><link rel="stylesheet" href="/web/all.min.css">', "")
  gsub_file(file, '<title>transcodes || Databrary</title></head><body vocab="http://schema.org"><section id="toolbar" class="toolbar"><div class="wrap toolbar-main"><div class="row"><nav class="toolbar-nav no-angular cf"><ul class="inline-block flat cf"><li><a href="/">Databrary</a></li><li><a href="//databrary.org/about.html">about</a></li><li><a href="//databrary.org/support.html">support</a></li></ul><ul class="toolbar-user inline-block flat cf"><li><a href="/profile">Your Dashboard</a></li><li><form method="POST" action="/user/logout"><button class="mini" type="submit">Logout</button></form></li></ul></nav></div></div></section><section id="main" class="main"><div class="wrap"><div class="row"><h1 class="view-title">transcodes</h1>', "")
}

clean_table_head <- function(file) {
  require(xfun)
  gsub_file(file, "||<table><thead><tr><th>", "")
  gsub_file(file, '<th>', "")
  gsub_file(file, "</th>", ",")
  gsub_file(file, '</thead><tbody>', "")
  gsub_file(file, '</tr>', "\n")
}

clean_rows <- function(file) {
  require(xfun)
  gsub_file(file, '<tr>', "")
  gsub_file(file, '</a>', "")
  gsub_file(file, '<td><form method="POST" action="/admin/transcode/[0-9]+"></form></td>', "")
  gsub_file(file, '</pre>', "")
  gsub_file(file, '<a href="/party/[0-9]+">', "")
  gsub_file(file, '<td><a href="/asset/[0-9]+">', "")
  gsub_file(file, '<td>', "")
  gsub_file(file, '</td>', ",")
  gsub_file(file, '<pre>', "")
  gsub_file(file, '<form method="POST" action="/admin/transcode/[0-9]+"><input type="submit" name="action" value="start"><input type="submit" name="action" value="fail"></form>', "")
}

open_html_as_text <- function(html_file) {
  readLines(html_file)
}


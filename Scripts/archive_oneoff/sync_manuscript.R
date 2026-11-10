#!/usr/bin/env Rscript
# Scripts/sync_manuscript.R
# Master driver for the surgical Google Drive sync: downloads the live
# "Practice and Networks" draft, performs in-place OpenXML image
# replacement (Scripts/sync_manuscript.py), and uploads the result back.
# Never re-uploads a freshly-compiled document -- only the specific
# <w:drawing> media targets identified in sync_manuscript.py are touched.

suppressPackageStartupMessages(library(googledrive))

doc_id <- "1llEbgZ8lZK7pMy3IjZFbLNoek5CK-S47MhNw91-Sa4A"
live_docx <- "draft_live.docx"
updated_docx <- "draft_updated.docx"

on.exit({
  unlink(Sys.glob("draft_*.docx"))
}, add = TRUE)

message("[1/4] Ensuring figures and tables are up to date...")
source("run_all.R")
source("Scripts/05_render_table_images.R")

message("[2/4] Downloading live manuscript from Google Drive...")
drive_auth(email = "omarlizardo@gmail.com")
drive_download(as_id(doc_id), path = live_docx, overwrite = TRUE)

message("[3/4] Performing in-place image replacement...")
exit_code <- system2("python3", args = c("Scripts/sync_manuscript.py", live_docx, updated_docx))
if (exit_code != 0) stop("Error during in-place image replacement.")

message("[4/4] Uploading updated manuscript back to Google Drive...")
drive_update(as_id(doc_id), media = updated_docx)

message("Synchronization complete.")

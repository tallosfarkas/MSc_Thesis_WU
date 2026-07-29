# ==============================================================================
# pipeline/R/04h_make_audit_workbook.R   (Stage 4 — build the BLIND human-audit workbook)
#
# Turns out/validation/snippet_audit_pool.csv into:
#   (1) a BLIND coding workbook  -> out/validation/snippet_audit_workbook.xlsx
#       (Instructions + Coding guide + Audit sheet; the rater sees ONLY the snippet text
#        and fills CCAudit {0,1} + Coding_Confidence {1,2,3}; snippets are shuffled and
#        re-id'd so order leaks no decile/score signal),
#   (2) a private KEY            -> out/validation/snippet_audit_KEY.csv
#       (rater_id -> snippet_id, Id, year, decile, GeoExposure, top_bigram) kept OUT of the
#        workbook, used only when scoring the returned file (04i).
# Run locally after pulling the pool from the cluster.  Rscript pipeline/R/04h_make_audit_workbook.R
# ==============================================================================
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({ library(data.table); library(openxlsx) })
.root <- function(){d<-normalizePath(getwd());while(d!="/"){if(file.exists(file.path(d,"pipeline/config/params.yml")))return(d);d<-dirname(d)};stop("no root")}
ROOT <- .root(); VAL <- file.path(ROOT,"out","validation"); say <- function(...) cat(sprintf(...),"\n")

pool <- fread(file.path(VAL,"snippet_audit_pool.csv"))
set.seed(42); pool <- pool[sample(.N)]                       # shuffle so order reveals nothing
pool[, rater_id := sprintf("A%03d", .I)]                     # blind id in shuffled order
fwrite(pool[, .(rater_id, snippet_id, Id, year, decile, GeoExposure, n_dict_hits, top_bigram)],
       file.path(VAL,"snippet_audit_KEY.csv"))
say("[key] wrote snippet_audit_KEY.csv (%d snippets) — keep private, do NOT send to the rater", nrow(pool))

# ---- workbook ---------------------------------------------------------------
wb <- createWorkbook()
hd <- createStyle(fontSize=12, textDecoration="bold", fgFill="#234c78", fontColour="white", halign="left", valign="center", border="TopBottomLeftRight")
wrap <- createStyle(wrapText=TRUE, valign="top", halign="left", border="TopBottomLeftRight")
ctr <- createStyle(halign="center", valign="center", border="TopBottomLeftRight")
ttl <- createStyle(fontSize=14, textDecoration="bold")
bod <- createStyle(fontSize=11, valign="top", wrapText=TRUE)

## Sheet 1: Instructions
addWorksheet(wb, "Instructions")
instr <- c(
  "GEOECONOMIC EXPOSURE — HUMAN SNIPPET AUDIT",
  "",
  "What this is: a blind check of the algorithm that scores how much each earnings call discusses",
  "geoeconomic risk. You read short text snippets and judge whether each one genuinely shows the",
  "firm's geoeconomic exposure. You do NOT see the algorithm's score — that is the point (an",
  "unbiased audit). Method follows Sautner et al. (2023, Appendix A.2) / Hassan et al. (2019).",
  "",
  "What to do (go to the 'Audit' sheet):",
  "  1. Read each snippet (column B).",
  "  2. In column C, CCAudit: type 1 if the snippet provides CLEAR evidence of the firm's",
  "     geoeconomic exposure; type 0 otherwise. (Dropdown provided.)",
  "  3. In column D, Coding_Confidence: 3 = highly confident, 2 = fairly confident, 1 = hard call.",
  "  4. Column E (notes) is optional — jot a reason for hard calls.",
  "  5. Save the file and send it back. Do not reorder or delete rows.",
  "",
  "Read the 'Coding guide' sheet FIRST — it defines what counts as geoeconomic exposure and gives",
  "examples. There are 220 snippets; budget ~1.5-2 hours. Code your honest read; there is no quota.",
  "",
  "Key idea: 'geoeconomic' = economics used as / affected by statecraft — tariffs & trade wars,",
  "sanctions, export controls, supply-chain disruption from geopolitics, reshoring / friend-shoring,",
  "national security & technology bans, critical minerals & energy security, geopolitical risk to",
  "the firm's markets or operations. A bare geographic mention ('sales in North America') is NOT,",
  "by itself, geoeconomic exposure.")
writeData(wb, "Instructions", instr); addStyle(wb, "Instructions", ttl, rows=1, cols=1)
setColWidths(wb, "Instructions", cols=1, widths=105)

## Sheet 2: Coding guide
addWorksheet(wb, "Coding guide")
guide <- data.table(
  Topic = c("CODE 1 (geoeconomic exposure) when the snippet discusses...","","","","","",
            "CODE 0 (not geoeconomic) when the snippet is...","","","","",
            "Confidence","",""),
  Detail = c(
    "Tariffs, trade war, trade barriers, import/export duties affecting the firm",
    "Sanctions, embargoes, entity lists, export controls, technology / chip bans",
    "Supply-chain disruption driven by geopolitics; reshoring, near-/friend-shoring, decoupling",
    "National security, critical minerals / rare earths, energy security, strategic reserves",
    "Geopolitical conflict / instability affecting the firm's markets, sourcing, or demand",
    "Cross-border policy / state intervention that changes the firm's competitive position",
    "Generic financials, earnings, margins, guidance with no geoeconomic angle",
    "Routine operations, products, management changes, ordinary competition",
    "A bare geographic mention (e.g. 'demand in North America') with no geoeconomic content",
    "Ordinary macro (rates, FX, inflation) with no statecraft / geopolitics angle",
    "Off-topic or boilerplate",
    "3 = highly confident the code is correct",
    "2 = fairly confident",
    "1 = hard call / genuinely ambiguous"))
writeData(wb, "Coding guide", guide); addStyle(wb, "Coding guide", hd, rows=1, cols=1:2)
addStyle(wb, "Coding guide", bod, rows=2:(nrow(guide)+1), cols=2, gridExpand=TRUE)
setColWidths(wb, "Coding guide", cols=1:2, widths=c(42, 80))

## Sheet 3: Audit (the blind coding sheet)
addWorksheet(wb, "Audit")
aud <- pool[, .(rater_id, snippet_text, CCAudit="", Coding_Confidence="", notes="")]
setnames(aud, c("rater_id","snippet_text"), c("ID","Snippet (read this)"))
writeData(wb, "Audit", aud, headerStyle=hd)
addStyle(wb, "Audit", wrap, rows=2:(nrow(aud)+1), cols=2, gridExpand=TRUE)
addStyle(wb, "Audit", ctr,  rows=2:(nrow(aud)+1), cols=c(1,3,4), gridExpand=TRUE)
setColWidths(wb, "Audit", cols=1:5, widths=c(7, 110, 10, 16, 30))
setRowHeights(wb, "Audit", rows=2:(nrow(aud)+1), heights=95)
freezePane(wb, "Audit", firstActiveRow=2)
dataValidation(wb, "Audit", col=3, rows=2:(nrow(aud)+1), type="list", value='"0,1"')
dataValidation(wb, "Audit", col=4, rows=2:(nrow(aud)+1), type="list", value='"1,2,3"')

saveWorkbook(wb, file.path(VAL,"snippet_audit_workbook.xlsx"), overwrite=TRUE)
say("[done] wrote out/validation/snippet_audit_workbook.xlsx (%d snippets, 3 sheets: Instructions / Coding guide / Audit)", nrow(pool))
say("  -> send the .xlsx to the rater; KEEP snippet_audit_KEY.csv private. Score it with 04i after return.")

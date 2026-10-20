# app.R : GEO IMPACT quarterly MPC dashboard (mock-data version)
# Styled to match the approved mockup. Run:  shiny::runApp()

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(scales)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Settings ---------------------------------------------------------------------
MIN_CELL <- 5          # suppress counts below this (confirm threshold with Yale/IRB)
ACCENT <- "#1F6F6B"; ORANGE <- "#C77A1E"; INK <- "#1B2A2F"; MUTED <- "#4B5F63"

# ---- Data (swap CSVs for real monthly aggregates; keep column names) ---------------
indicators <- read_csv("data/indicators.csv",  show_col_types = FALSE)
cascade    <- read_csv("data/cascade.csv",     show_col_types = FALSE)
qual       <- read_csv("data/qual_themes.csv", show_col_types = FALSE)

sites     <- c("All sites", sort(unique(indicators$site)))
all_months <- sort(unique(indicators$month))

# ---- Helpers ----------------------------------------------------------------------
suppress <- function(x) ifelse(!is.na(x) & x > 0 & x < MIN_CELL, NA, x)
fmt_n    <- function(x) ifelse(is.na(x), "<5", comma(x))
fmt_pct  <- function(x) ifelse(is.na(x) | is.nan(x), "-", percent(x, accuracy = 1))
safe_div <- function(a, b) ifelse(b > 0, a / b, NA_real_)

# "This quarter" = latest 3 months of data; "Last 4 quarters" = 12; "Since launch" = all
months_back <- function(w) switch(w, q = 3, q4 = 12, all = length(all_months))
window_months <- function(w, at_least = 0) {
  k <- max(months_back(w), at_least)
  tail(all_months, k)
}

summarise_ind <- function(df) {
  df |> group_by(indicator) |>
    summarise(num = sum(numerator, na.rm = TRUE), den = sum(denominator, na.rm = TRUE), .groups = "drop")
}
val <- function(s, name, col) { v <- s[[col]][s$indicator == name]; if (length(v)) v else 0 }

# ---- HTML builders (inline styles copied from the mockup) --------------------------
card_style <- "background:#FFFFFF;border:1px solid #D8DED9;border-radius:10px;padding:14px 16px"

kpi_card <- function(label, big, small = NULL) {
  div(style = "flex:1 1 220px;min-width:0;background:#FFFFFF;border:1px solid #D8DED9;border-radius:10px;padding:12px 16px",
      div(style = "font-size:12px;color:#4B5F63;font-weight:500", label),
      div(style = "display:flex;flex-wrap:wrap;align-items:baseline;gap:4px 10px",
          span(style = "font-family:'Source Serif 4',Georgia,serif;font-size:32px;font-weight:600;line-height:1.15", big),
          if (!is.null(small)) span(style = "font-size:12px;color:#4B5F63", small)))
}

bar_row <- function(label, value_text, share, track, fill) {
  w <- if (is.na(share)) 0 else round(100 * min(share, 1))
  div(div(style = "display:flex;justify-content:space-between;gap:8px;font-size:12px",
          span(label), tags$strong(value_text)),
      div(style = paste0("height:8px;background:", track, ";border-radius:4px;margin-top:3px"),
          div(style = sprintf("width:%d%%;height:8px;background:%s;border-radius:4px", w, fill))))
}

cascade_row <- function(label, def, value_text, share) {
  w <- if (is.na(share)) 0 else round(100 * min(share, 1))
  div(style = "display:flex;align-items:center;gap:8px", title = def,
      div(style = "flex:0 0 138px;font-size:12px", label),
      div(style = "flex:1 1 80px;height:10px;background:#CFE3E0;border-radius:5px",
          div(style = sprintf("width:%d%%;height:10px;background:#1F6F6B;border-radius:5px", w))),
      div(style = "flex:0 0 96px;text-align:right;font-size:12px;font-weight:600", value_text))
}

# Inline SVG line chart, same geometry as the mockup (viewBox 0 0 400 160)
line_chart_svg <- function(d, comps) {
  months <- sort(unique(d$month)); k <- length(months)
  ymax <- max(d$n[d$component == "Total"], 1)
  xs <- function(m) 34 + (match(m, months) - 1) / max(k - 1, 1) * (392 - 34)
  ys <- function(n) 135 - n / ymax * 125
  pl <- function(series, colour, dash = NULL) {
    s <- d |> filter(component == series) |> arrange(month)
    pts <- paste(sprintf("%.1f,%.1f", xs(s$month), ys(s$n)), collapse = " ")
    sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="2.5"%s/>', pts, colour,
            if (is.null(dash)) "" else sprintf(' stroke-dasharray="%s"', dash))
  }
  mid <- months[ceiling(k / 2)]
  lab <- function(x, anchor, m) sprintf('<text x="%d" y="152" text-anchor="%s" font-size="10" fill="#4B5F63">%s</text>',
                                         x, anchor, format(m, "%b %Y"))
  lines_svg <- paste0(pl("Total", INK),
                      if ("Component 1" %in% comps) pl("Component 1", ACCENT) else "",
                      if ("Component 2" %in% comps) pl("Component 2", ORANGE, "6 4") else "")
  HTML(paste0(
    '<svg viewBox="0 0 400 160" role="img" aria-label="Line chart of newly engaged contacts per month" style="width:100%;height:auto;margin-top:8px;display:block">',
    paste(sprintf('<line x1="34" y1="%d" x2="392" y2="%d" stroke="#E4E9E5"/>', c(10, 45, 80, 115), c(10, 45, 80, 115)), collapse = ""),
    '<line x1="34" y1="135" x2="392" y2="135" stroke="#9FB0B2"/>',
    '<text x="28" y="139" text-anchor="end" font-size="10" fill="#4B5F63">0</text>',
    sprintf('<text x="28" y="14" text-anchor="end" font-size="10" fill="#4B5F63">%s</text>', ymax),
    lab(34, "start", months[1]), lab(213, "middle", mid), lab(392, "end", months[k]),
    lines_svg, '</svg>'))
}

legend_item <- function(label, colour, dash = NULL) {
  span(style = "display:inline-flex;align-items:center;gap:6px",
       HTML(sprintf('<svg width="20" height="8" aria-hidden="true"><line x1="0" y1="4" x2="20" y2="4" stroke="%s" stroke-width="2.5"%s/></svg>',
                    colour, if (is.null(dash)) "" else sprintf(' stroke-dasharray="%s"', dash))),
       label)
}

pill <- function(label, selected) {
  tags$button(label, `aria-pressed` = tolower(selected),
              onclick = sprintf("Shiny.setInputValue('site','%s',{priority:'event'})", label),
              style = if (selected)
                paste0("min-height:44px;padding:0 14px;border-radius:22px;border:1px solid ", ACCENT,
                       ";background:", ACCENT, ";color:#FFFFFF;font-family:inherit;font-size:13px;font-weight:600;cursor:pointer")
              else "min-height:44px;padding:0 14px;border-radius:22px;border:1px solid #9FB0B2;background:#FFFFFF;color:#1B2A2F;font-family:inherit;font-size:13px;font-weight:500;cursor:pointer")
}

toggle_btn <- function(label, value, selected) {
  tags$button(label, `aria-pressed` = tolower(selected),
              onclick = sprintf("Shiny.setInputValue('casc_mode','%s',{priority:'event'})", value),
              style = if (selected)
                paste0("min-height:44px;padding:0 14px;border:0;background:", ACCENT, ";color:#FFFFFF;font-family:inherit;font-size:13px;font-weight:600;cursor:pointer")
              else "min-height:44px;padding:0 14px;border:0;background:#FFFFFF;color:#1B2A2F;font-family:inherit;font-size:13px;font-weight:500;cursor:pointer")
}

select_box <- function(id, label, choices) {
  sel_style <- "height:44px;min-width:150px;border:1px solid #9FB0B2;border-radius:8px;background:#FFFFFF;padding:0 10px;font-family:inherit;font-size:13px;color:#1B2A2F"
  div(style = "display:flex;flex-direction:column;gap:2px",
      tags$label(`for` = id, style = "font-size:11px;color:#4B5F63;font-weight:500", label),
      tags$select(id = paste0("sel_", id), style = sel_style,
                  onchange = sprintf("Shiny.setInputValue('%s', this.value)", id),
                  lapply(names(choices), function(n) tags$option(value = choices[[n]], n))))
}

# ---- UI ---------------------------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$title("MPC: Quarterly Implementation Metrics"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=Source+Serif+4:wght@500;600&display=swap"),
    tags$link(rel = "stylesheet", href = "dashboard.css")
  ),
  div(style = "background:#F4F6F3;color:#1B2A2F;font-family:'IBM Plex Sans',system-ui,sans-serif;font-size:13px;line-height:1.4;min-height:100vh",
      # Header
      div(style = "background:#FFFFFF;border-bottom:1px solid #D8DED9",
          div(style = "max-width:1440px;margin:0 auto;padding:12px 24px;box-sizing:border-box;display:flex;flex-wrap:wrap;align-items:center;justify-content:space-between;gap:12px 24px",
              div(
                h1(style = "margin:2px 0 0;font-family:'Source Serif 4',Georgia,serif;font-weight:600;font-size:24px;line-height:1.15",
                   "MPC: Quarterly Implementation Metrics"),
                div(style = "margin-top:2px;color:#4B5F63;font-size:12px", uiOutput("showing", inline = TRUE))),
              div(style = "display:flex;flex-wrap:wrap;align-items:center;gap:6px",
                  span(style = "font-size:12px;color:#4B5F63;font-weight:500;margin-right:2px", "Site"),
                  uiOutput("pills", inline = TRUE, style = "display:contents")),
              div(style = "display:flex;flex-wrap:wrap;gap:10px",
                  select_box("component", "MPC component",
                             c("Total (all)" = "all", "Component 1" = "Component 1", "Component 2" = "Component 2")),
                  select_box("window", "Time window",
                             c("This quarter" = "q", "Last 4 quarters" = "q4", "Since launch" = "all"))))),
      # Body
      div(style = "max-width:1440px;margin:0 auto;padding:16px 24px 20px;box-sizing:border-box",
          uiOutput("body", style = "display:flex;flex-direction:column;gap:12px")))
)

# ---- Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  site   <- reactive(input$site %||% "All sites")
  comp   <- reactive(input$component %||% "all")
  win    <- reactive(input$window %||% "q")
  mode   <- reactive(input$casc_mode %||% "raw")

  by_site <- function(d) if (site() == "All sites") d else d |> filter(site == site())

  ind_window <- reactive(indicators |> filter(month %in% window_months(win())) |> by_site())
  ind_comp <- reactive({
    d <- ind_window()
    if (comp() != "all") d <- d |> filter(component %in% c(comp(), "Total"))
    d
  })
  s <- reactive(summarise_ind(ind_comp()))

  output$showing <- renderUI({
    tagList("Showing: ", tags$strong(style = "color:#1B2A2F", site()), " | ",
            paste0(format(min(window_months(win())), "%b %Y"), " to ", format(max(all_months), "%b %Y")))
  })
  output$pills <- renderUI(lapply(sites, function(x) pill(x, x == site())))

  output$body <- renderUI({
    sm <- s()

    # ---- KPI cards ----
    eng  <- val(sm, "engaged", "num"); pwud <- val(sm, "engaged_pwud", "num")
    nogap_n <- val(sm, "gap_never_sud_assess", "num"); nogap_d <- val(sm, "gap_never_sud_assess", "den")
    link_n <- sum(sm$num[sm$indicator %in% c("link_hiv", "link_hcv", "link_sud")])
    link_d <- sum(sm$den[sm$indicator %in% c("link_hiv", "link_hcv", "link_sud")])
    ret_n <- val(sm, "retained_med", "num"); ret_d <- val(sm, "retained_med", "den")
    kpis <- div(style = "display:flex;flex-wrap:wrap;gap:12px",
                kpi_card("Reach - Newly engaged contacts",
                         paste0(fmt_n(suppress(eng)), " total, ", fmt_n(suppress(pwud)), " PWUD")),
                kpi_card("Reach - PWUD not accessing services",
                         paste0(fmt_n(suppress(nogap_n)), " (", fmt_pct(safe_div(nogap_n, nogap_d)), ")"),
                         "never assessed for SUD"),
                kpi_card("Adoption - PWUD linked to external services",
                         paste0(fmt_n(suppress(link_n)), " (", fmt_pct(safe_div(link_n, link_d)), ")"),
                         "referral made of needed"),
                kpi_card("Effectiveness - Retained on medication",
                         paste0(fmt_n(suppress(ret_n)), " (", fmt_pct(safe_div(ret_n, ret_d)), ")"),
                         "of those initiated"))

    # ---- Reach line chart ----
    chart_months <- window_months(win(), at_least = 12)
    cd <- indicators |> filter(indicator == "engaged", month %in% chart_months) |> by_site() |>
      group_by(month, component) |> summarise(n = sum(numerator), .groups = "drop")
    cd <- bind_rows(cd, cd |> group_by(month) |> summarise(n = sum(n), .groups = "drop") |> mutate(component = "Total"))
    comps <- if (comp() == "all") c("Component 1", "Component 2") else comp()
    reach_card <- div(style = paste0("flex:4 1 340px;min-width:0;", card_style),
                      div(style = "font-size:14px;font-weight:600", "Reach: newly engaged contacts per month"),
                      div(style = "font-size:12px;color:#4B5F63", "By MPC component and total"),
                      line_chart_svg(cd, comps),
                      div(style = "display:flex;flex-wrap:wrap;gap:12px;margin-top:4px;font-size:12px;color:#4B5F63",
                          legend_item("Total", INK),
                          if ("Component 1" %in% comps) legend_item("Component 1", ACCENT),
                          if ("Component 2" %in% comps) legend_item("Component 2", ORANGE, "6 4")))

    # ---- Gaps ----
    gaps <- c(gap_never_hiv_test = "Never accessed HIV testing", gap_never_hcv_test = "Never accessed HCV testing",
              gap_never_sud_assess = "Never accessed SUD assessment", gap_not_on_hiv_med = "Not on HIV PrEP / ART",
              gap_not_on_hcv_tx = "Not on HCV treatment", gap_not_on_sud_tx = "Not on SUD treatment")
    gap_card <- div(style = paste0("flex:3 1 260px;min-width:0;", card_style),
                    div(style = "font-size:14px;font-weight:600", "Reach: gaps at first contact"),
                    div(style = "font-size:12px;color:#4B5F63", "% of newly engaged PWUD"),
                    div(style = "display:flex;flex-direction:column;gap:9px;margin-top:10px",
                        lapply(names(gaps), function(k) {
                          sh <- safe_div(val(sm, k, "num"), val(sm, k, "den"))
                          bar_row(gaps[[k]], fmt_pct(sh), sh, "#F0E3CF", ORANGE)
                        })))

    # ---- Adoption (+ organizations) ----
    links <- c(link_hiv = "HIV care / PrEP", link_hcv = "HCV care", link_sud = "SUD treatment")
    org_latest <- ind_window() |> filter(indicator %in% c("org_referring", "org_receiving", "org_coordinating"),
                                         month == max(month)) |>
      group_by(indicator) |> summarise(n = sum(numerator), d = sum(denominator), .groups = "drop")
    org_row <- function(k, label, last = FALSE) {
      r <- org_latest[org_latest$indicator == k, ]
      txt <- if (nrow(r)) paste0(fmt_n(suppress(r$n)), " (", fmt_pct(safe_div(r$n, r$d)), ")") else "-"
      div(style = paste0("display:flex;justify-content:space-between;gap:8px;font-size:12px;",
                         if (last) "padding:5px 0 0" else "padding:5px 0;border-bottom:1px solid #E4E9E5"),
          span(label), tags$strong(txt))
    }
    adopt_card <- div(style = card_style,
                      div(style = "font-size:14px;font-weight:600", "Adoption: linkage and partners"),
                      div(style = "font-size:12px;color:#4B5F63", "PWUD linked to external services"),
                      div(style = "display:flex;flex-direction:column;gap:9px;margin-top:10px",
                          lapply(names(links), function(k) {
                            n <- val(sm, k, "num"); d <- val(sm, k, "den"); sh <- safe_div(n, d)
                            bar_row(links[[k]], paste0(fmt_pct(sh), " (", fmt_n(suppress(n)), " of ", fmt_n(d), ")"),
                                    sh, "#CFE3E0", ACCENT)
                          })),
                      div(style = "margin-top:12px;border-top:1px solid #E4E9E5;padding-top:8px;font-size:12px;color:#4B5F63",
                          "Community health organizations"),
                      org_row("org_referring", "Referring to MPC"),
                      org_row("org_receiving", "Receiving referrals from MPC"),
                      org_row("org_coordinating", "Coordinating care with MPC", last = TRUE))

    # ---- Cascades ----
    cd2 <- cascade |> filter(month %in% window_months(win())) |> by_site() |>
      group_by(cascade, step_order, step, definition) |>
      summarise(n = sum(n_at_step), first = sum(n_first), prior = sum(n_prior, na.rm = TRUE),
                prog = sum(n_progressed, na.rm = TRUE), .groups = "drop") |>
      mutate(share = if (mode() == "raw") safe_div(n, first) else ifelse(step_order == 1, 1, safe_div(prog, prior)),
             text  = if (mode() == "raw") paste0(fmt_n(suppress(n)), " (", fmt_pct(share), ")")
                     else if_else(step_order == 1, "start", paste0(fmt_pct(share), " of prior")))
    casc_block <- function(name) {
      d <- cd2 |> filter(cascade == name) |> arrange(step_order)
      div(style = "flex:1 1 380px;min-width:0;display:flex;flex-direction:column;gap:5px",
          div(style = "font-family:'Source Serif 4',Georgia,serif;font-size:15px;font-weight:600;border-bottom:1px solid #E4E9E5;padding-bottom:3px", name),
          lapply(seq_len(nrow(d)), function(i) cascade_row(d$step[i], d$definition[i], d$text[i], d$share[i])))
    }
    casc_card <- div(style = paste0(card_style, ";display:flex;flex-direction:column;gap:10px"),
                     div(style = "display:flex;flex-wrap:wrap;align-items:center;justify-content:space-between;gap:8px",
                         div(div(style = "font-size:14px;font-weight:600", "Effectiveness: care cascades"),
                             div(style = "font-size:12px;color:#4B5F63",
                                 if (mode() == "raw")
                                   "Raw monthly proportions: share of all patients in each cascade at each step. Hover a step for its definition."
                                 else "Unique-patient progression: of the same people followed over time, share who moved from the prior step.")),
                         div(role = "group", `aria-label` = "Cascade view",
                             style = "display:flex;border:1px solid #9FB0B2;border-radius:10px;overflow:hidden",
                             toggle_btn("Raw monthly", "raw", mode() == "raw"),
                             toggle_btn("Unique patients", "prog", mode() == "prog"))),
                     div(style = "display:flex;flex-wrap:wrap;gap:12px 24px",
                         casc_block("HIV treatment"), casc_block("HIV PrEP"),
                         casc_block("SUD"), casc_block("HCV")))

    # ---- Qual panel ----
    q <- qual |> filter(site == site())
    themes <- q |> filter(kind == "theme"); refl <- q |> filter(kind == "reflection")
    qual_panel <- div(style = "flex:3 1 340px;min-width:0;background:#F3ECDD;border:1px solid #E2D5B8;border-radius:10px;padding:16px;display:flex;flex-direction:column;gap:10px",
                      div(div(style = "font-size:14px;font-weight:600;color:#3F3015", "Qualitative Interviews"),
                          div(style = "font-size:12px;color:#5A4519", paste0("[Quarter, Year] | ", site()))),
                      lapply(seq_len(nrow(themes)), function(i)
                        div(style = "background:#FFFFFF;border:1px solid #E2D5B8;border-radius:8px;padding:12px;display:flex;flex-direction:column;gap:4px",
                            div(style = "font-family:'Source Serif 4',Georgia,serif;font-size:15px;font-weight:600", themes$title[i]),
                            div(style = "font-size:12.5px;color:#33464A", themes$summary[i]),
                            div(style = "font-size:12.5px;font-style:italic;color:#4B5F63", paste0("\"", themes$quote[i], "\"")))),
                      div(style = "background:#FFFFFF;border:1px solid #E2D5B8;border-radius:8px;padding:12px",
                          div(style = "font-size:13px;font-weight:600", "Periodic reflection highlights"),
                          tags$ul(style = "margin:4px 0 0;padding-left:18px;font-size:12.5px;color:#33464A;display:flex;flex-direction:column;gap:3px",
                                  lapply(refl$summary, tags$li))))

    tagList(
      kpis,
      div(style = "display:flex;flex-wrap:wrap;gap:12px;align-items:stretch",
          div(style = "flex:8 1 720px;min-width:0;display:flex;flex-direction:column;gap:12px",
              div(style = "display:flex;flex-wrap:wrap;gap:12px;align-items:stretch", reach_card, gap_card),
              adopt_card, casc_card),
          qual_panel),
      div(style = "font-size:12px;color:#4B5F63",
          paste0("Data sources: monthly aggregate MPC reports and qualitative data from the implementation evaluation team. Counts below ",
                 MIN_CELL, " suppressed. Last updated: [date]."))
    )
  })
}

shinyApp(ui, server)

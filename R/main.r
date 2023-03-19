##################################################
#                 How I make Eurostat maps with R
#                 Milos Popovic
#                 2023/03/19
##################################################
# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt",
    "giscoR", "eurostat"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. NUTS2 & country shapefile
#-----------------------------
# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

nuts2 <- giscoR::gisco_get_nuts(
    year = "2021",
    resolution = "3",
    nuts_level = "2"
) |>
    sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(nuts2))
head(nuts2)

cntrys <- giscoR::gisco_get_countries(
    year = "2020",
    resolution = "3",
    region = c("Europe", "Asia")
) |>
    sf::st_transform(crsLONGLAT)

names(cntrys)

# BA = Bosnia & Herzegovina
# BY = Belarus; GE = Georgia
# MD = Moldova; RU = Russia
# UA = Ukraine
non_eu_list <- c(
    "BA", "BY", "GE",
    "MD", "RU", "UA"
)
eu_list <- c(unique(nuts2$CNTR_CODE))

eu <- cntrys |>
    filter(CNTR_ID %in% eu_list)

non_eu <- cntrys |>
    filter(CNTR_ID %in% non_eu_list)

plot(sf::st_geometry(non_eu))
plot(sf::st_geometry(eu))

# 2. EUROSTAT DATA
#-----------------
# search for datasets with GDP in their title
toc <- eurostat::get_eurostat_toc()
i <- "GDP"
tab <- subset(
    toc, grepl(i, title)
)
fix(tab)

# get NUTS2-level data
get_eurostat_data <- function() {
    eurostat_df <- eurostat::get_eurostat(
        "nama_10r_2gdp",
        time_format = "num"
    ) |>
        dplyr::filter(
            unit == "PPS_EU27_2020_HAB" &
                time >= 2020
        ) |>
        dplyr::select(geo, time, values)
    names(eurostat_df)[1] <- "NUTS_ID"
    return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
    eurostat_df,
    names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
    # Replace missing values from 2021 with available values from 2020
    enriched_df <- wide_df |>
        dplyr::mutate(values = if_else(is.na(`2021`), `2020`, `2021`)) |>
        dplyr::select(NUTS_ID, values)

    # merge shp and data.frame
    df <- nuts2 |>
        dplyr::left_join(enriched_df, by = "NUTS_ID")
    return(df)
}

df <- get_enriched_table()
head(df)

# 3. LEGEND BREAKS
#-----------------
# let's find a natural interval with Jenks breaks
df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
    n = 6,
    style = "jenks"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
    labels <- c(labels, paste0(
        round(ni[i], 0),
        "-",
        round(ni[i + 1], 0)
    ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
df$cat <- cut(df$values,
    breaks = ni,
    labels = labels,
    include.lowest = T
)
levels(df$cat)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No data"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No data"
levels(df$cat)

# 4. BOUNDING BOX
#-----------------
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_bounding_box_europe <- function() {
    xmin <- -10.6600
    xmax <- 36.5500
    ymin <- 34.5000
    ymax <- 71.0500

    bbox_laea <- sf::st_sfc(
        sf::st_polygon(list(cbind(
            c(xmin, xmax, xmax, xmin, xmin),
            c(ymin, ymin, ymax, ymax, ymin)
        ))),
        crs = crsLONGLAT
    ) |> sf::st_transform(crsLAEA)

    bbox <- sf::st_bbox(bbox_laea)
    return(bbox)
}
bbox <- get_bounding_box_europe()

# 5. MAP
#-------
cols <- c(
    "#fff3b0", "#a7d08f",
    "#6ea57f", "#587573", "#4f4365",
    "#440154", "#cacaca"
)

p <- ggplot() +
    geom_sf(
        data = filter(eu, CNTR_ID == "RS"),
        color = "grey20", size = 0.15, fill = "#cacaca"
    ) +
    geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
    geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
    geom_sf(
        data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
    ) +
    coord_sf(
        crs = crsLAEA,
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    scale_fill_manual(
        name = "1000s of EUR",
        values = cols,
        drop = F
    ) +
    guides(fill = guide_legend(
        direction = "horizontal",
        keyheight = unit(1.5, units = "mm"),
        keywidth = unit(15, units = "mm"),
        title.position = "top",
        title.hjust = .5,
        label.hjust = .5,
        nrow = 1,
        byrow = T,
        reverse = F,
        label.position = "bottom"
    )) +
    labs(
        x = "©2023 Milos Popovic (https://milospopovic.net) Data: Eurostat",
        y = NULL,
        title = "GDP per capita adjusted by PPS*",
        subtitle = "",
        caption = "* The data for Albania & N. Macedonia is from 2020, the rest from 2021"
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(
            size = 11, color = "grey20",
            hjust = 0.5, vjust = 6
        ),
        axis.title.y = element_blank(),
        legend.position = c(.35, .975),
        legend.text = element_text(
            size = 12, color = "grey20"
        ),
        legend.title = element_text(
            size = 14, color = "grey20"
        ),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 20, color = "#28a584",
            hjust = .5
        ),
        plot.caption = element_text(
            size = 10, color = "grey20", hjust = .5,
            vjust = 5
        ),
        plot.margin = unit(
            c(t = 1, r = -2, b = -1, l = -2), "lines"
        ),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank()
    )

ggsave(
    filename = "eur_gdp_pc_pps_2021.png",
    width = 7, height = 8.5, dpi = 600,
    device = "png", bg = "white", p
)

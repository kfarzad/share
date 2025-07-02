### Kiarash Farzad
################################### colors #####################################
### https://sashamaps.net/docs/resources/20-colors/
myColorSimple <- grDevices::colorRampPalette(c(
    "blue3",
    "blue",
    "deepskyblue1",
    "cyan1",
    "green",
    "yellow",
    "orange",
    "orangered",
    "red"
))

myColorDiff <- grDevices::colorRampPalette(c(
    "blue",
    "white",
    "red"
))

myColorGradual <- grDevices::colorRampPalette(c(
    "yellow",
    "red"
))
# https://simple.wikipedia.org/wiki/Cardinal_direction
# https://forum.cmascenter.org/t/mcip-metcro2d-units-wind-direction
# 0 Deg = form the north = black
# 90 Deg =  from the east = blue
# 180 Deg = from the south = red
# 270 Deg = from the west =  blue

myColorWind <- grDevices::colorRampPalette(c(
    "#000000",
    "#000080",
    "#000080",
    "#0000ff",
    "#0000FF",
    "#800080",
    "#800080",
    "#ff0000",
    "#ff0000",
    "#ff8080",
    "#ff8080",
    "#ffffff",
    "#FFFFFF",
    "#808080",
    "#808080",
    "#000000"
))
##################################### trim #####################################
trim_bound <- function(definiton = "CONUS") {
    if (definiton == "CONUS") {
        df_out <- c(-125, -66.9, 24.3, 49.4)
    } else {
        df_out <- c(-136.6, -58, 18.48, 56)
    }
    return(df_out)
}
##################### create usa and world map in lcc cord #####################
### usa ###
if (!exists("file_crs")) {
    print("!!!!! file_crs is not available !!!!!")
}

png(filename = "temp.png")
usa <- maps::map(database = "state", regions = ".", fill = TRUE)
dev.off()
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa_longlat <- maptools::map2SpatialPolygons(usa,
    IDs = IDs,
    proj4string = sp::CRS("+init=epsg:4326")
)
if (exists("file_crs")) {
    usa_lcc <- sp::spTransform(x = usa_longlat, CRSobj = file_crs)
}
png(filename = "temp.png")
usac <- maps::map(database = "county", regions = ".", fill = TRUE)
dev.off()
IDs <- sapply(strsplit(usac$names, ":"), function(x) x[1])
usac_longlat <- maptools::map2SpatialPolygons(usac,
    IDs = IDs,
    proj4string = sp::CRS("+init=epsg:4326")
)
if (exists("file_crs")) {
    usac_lcc <- sp::spTransform(x = usac_longlat, CRSobj = file_crs)
}
### world ###
png(filename = "temp.png")
world <- maps::map(database = "world", regions = ".", fill = TRUE)
dev.off()
IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
world_longlat <- maptools::map2SpatialPolygons(world,
    IDs = IDs,
    proj4string = sp::CRS("+init=epsg:4326")
)
if (exists("file_crs")) {
    world_lcc <- sp::spTransform(x = world_longlat, CRSobj = file_crs)
}
rm(IDs)
try(file.remove("temp.png"), silent = TRUE)

############################# create usa city names ############################
prepare_cities <- function(
    states1 = "", cities1 = "", proj = "",
    xratiokm1 = "", yratiokm1 = "", debug = FALSE) {
    us_cities <- maps::us.cities[, c("name", "long", "lat")]
    us_cities_list <- stringr::str_split(us_cities$name, " ")
    for (i in 1:length(us_cities_list)) {
        us_cities$name[i] <- paste(
            us_cities_list[[i]]
            [1:length(us_cities_list[[i]]) - 1],
            collapse = " "
        )
        us_cities$state[i] <- us_cities_list[[i]][length(us_cities_list[[i]])]
    }

    ### city coordination corrections
    us_cities[
        us_cities[, "state"] == "MA" & us_cities[, "name"] == "Boston",
        c("lat", "long")
    ] <- c(42.36048524865876, -71.05801446156207)
    us_cities[
        us_cities[, "state"] == "MA" & us_cities[, "name"] == "Brookline",
        c("lat", "long")
    ] <- c(42.33433783423088, -71.12073123338332)
    ### end city coordination corrections
    ### add extra locations
    us_cities <- rbind(
        us_cities[, c("state", "name", "lat", "long")],
        c("MA", "Chelsea", 42.39379096288304, -71.03159885689249)
    )
    us_cities[, c("lat")] <- as.numeric(us_cities[, c("lat")])
    us_cities[, c("long")] <- as.numeric(us_cities[, c("long")])
    ### end add extra locations

    if (states1 != "" && cities1 != "") {
        us_states_final <- data.frame()
        us_cities_final <- data.frame()
        for (state in states1) {
            us_states_temp <- us_cities[us_cities[, "state"] == state, ]
            us_states_final <- rbind(us_states_final, us_states_temp)
        }
        for (city in cities1) {
            us_cities_temp <- us_states_final[
                us_states_final[, "name"] == city,
            ]
            us_cities_final <- rbind(us_cities_final, us_cities_temp)
        }
        us_cities <- us_cities_final
    }
    if (debug) {
        print(paste(us_cities))
    }

    us_cities_text <- us_cities
    sp::coordinates(us_cities) <- ~ long + lat
    sp::proj4string(us_cities) <- sp::CRS("+init=epsg:4326")
    us_cities_longlat <- us_cities
    if (exists("file_crs")) {
        us_cities_lcc <- sp::spTransform(x = us_cities_longlat, CRSobj = file_crs)
    }
    if (debug) {
        print(us_cities_longlat)
        print(us_cities_lcc)
    }
    us_cities_text$lat <- us_cities_text$lat - (yratiokm1 / 3000)
    us_cities_text$long <- us_cities_text$long + 0
    # us_cities_text$lat <- us_cities_text$lat - (yratiokm1 / 5000)
    # us_cities_text$long <- us_cities_text$long + (xratiokm1 / 1200)
    if (debug) {
        print(us_cities_text)
    }
    sp::coordinates(us_cities_text) <- ~ long + lat
    sp::proj4string(us_cities_text) <- sp::CRS("+init=epsg:4326")
    us_cities_longlat_text <- us_cities_text
    if (exists("file_crs")) {
        us_cities_lcc_text <- sp::spTransform(x = us_cities_text, CRSobj = file_crs)
    }
    if (debug) {
        print(us_cities_longlat_text)
        print(us_cities_lcc_text)
    }

    if (proj == "longlat") {
        return(list(us_cities_longlat, us_cities_longlat_text))
    } else if (proj == "lcc") {
        return(list(us_cities_lcc, us_cities_lcc_text))
    }
}

################################### USA zonal ##################################
extract_zone <- function(
    i = 0, def = "", plot_proj_mode = "longlat",
    debug = FALSE, silent = FALSE, num_reg = FALSE,
    dotfiles_path) {
    source(paste0(dotfiles_path, "/", "/models/cmaq/post/script_pkg_regions.R"))

    if (debug) {
        print(paste("zone:", i))
    }
    if (num_reg) {
        return(func_reg(def = def, num_regions = TRUE, silent = silent))
    }
    region_name <- func_reg(def = def, silent = silent)[i]
    climReg <- func_reg(
        def = def, return_region_states = TRUE,
        region_name = region_name, silent = TRUE
    )

    if (debug) {
        print(paste("climReg:", climReg))
    }
    png(filename = "temp.png")
    climRegMask <- maps::map(database = "state", regions = climReg, interior = FALSE)
    dev.off()

    border <- maptools::map2SpatialLines(climRegMask, proj4string = sp::CRS("+init=epsg:4326"))

    if (plot_proj_mode == "lcc") {
        border <- sp::spTransform(x = border, CRSobj = file_crs)
    }
    if (debug) {
        # print(climRegMask)
        # print(border)
    }
    return(border)
}


################################################################################
############################### overlay function ###############################
################################################################################

add_overlay <- function(
    fColor,
    conLim,
    df_long_lat_val,
    plot_proj_mode = "longlat",
    main_raster_crs,
    variable_cex = FALSE,
    df_cex = 1.3,
    overlay_spatial_trim = TRUE,
    trim_bound_def = "CONUS",
    debug = FALSE) {
    # https://stackoverflow.com/questions/40435047/plot-xyz-points-on-raster-layer
    if (debug) {
        print("Making Overlay")
    }
    if (anyNA(df_long_lat_val)) {
        print("There is NA values in df_long_lat_val, script will crash!")
        stop()
    }

    cex_normalized <- function(x, conLim = conLim) {
        x[x < conLim[1]] <- conLim[1]
        x[x > conLim[2]] <- conLim[2]
        x <- abs(x)
        x <- (x - min(x)) / (max(x) - min(x))
        x[x == 0] <- 0.01
        x <- x * 5
        return(x)
    }

    if (plot_proj_mode == "longlat") {
        if (debug) {
            print(paste("overlay mode is", plot_proj_mode))
        }
        inp <- df_long_lat_val
        ## Rename the columns
        colnames(inp) <- c("long", "lat", "val")

        ## Trim Extent, remove some stations
        if (overlay_spatial_trim) {
            inp <- inp[inp$long > trim_bound(trim_bound_def)[1], ]
            inp <- inp[inp$long < trim_bound(trim_bound_def)[2], ]
            inp <- inp[inp$lat > trim_bound(trim_bound_def)[3], ]
            inp <- inp[inp$lat < trim_bound(trim_bound_def)[4], ]
        }

        ## Rearrenge Colors to match
        conLimRange <- conLim[2] - conLim[1]
        bin_range <- conLimRange / length(fColor)

        fColorTemp <- character()

        ##### updated #####
        # bin_range
        if (debug) {
            print(paste("input:"))
            print(head(inp))
            print(paste("conLimRange:", conLimRange))
            print(paste("bin_range:", bin_range))
        }
        for (ii in 1:nrow(inp)) { # nrow(inp)
            if (debug) print(ii)
            for (jj in 1:length(fColor)) {
                if (debug) print(jj)
                if (inp$val[ii] >=
                    bin_range * (jj - 1) + conLim[1] &&
                    inp$val[ii] < bin_range * jj + conLim[1]) {
                    fColorTemp[ii] <- fColor[jj]
                    break
                } else if (inp$val[ii] >= conLim[2]) {
                    fColorTemp[ii] <- fColor[length(fColor)]
                    break
                } else if (inp$val[ii] <= conLim[1]) {
                    fColorTemp[ii] <- fColor[1]
                    break
                }
            }
        }

        fColor <- fColorTemp

        if (debug) {
            print(paste("fColor:", head(fColor)))
        }
        ## Make spatial points
        sp::coordinates(inp) <- ~ long + lat
        sp::proj4string(inp) <- sp::CRS("+init=epsg:4326")
        if (debug) {
            try({
                write.csv(x = inp, file = "overlay_inp_debug.csv", row.names = FALSE)
            })
        }

        if (variable_cex) {
            cex <- cex_normalized(inp$val, conLim = conLim)
        } else {
            cex <- df_cex
        }
        if (debug) {
            print(paste("cex:", head(cex)))
        }

        ## Add plot as a layer
        raster::plot(inp, bg = fColor, pch = 21, cex = cex, add = TRUE)
    }

    ############################################################################
    ########################### new mode (lcc / curve) #########################
    ############################################################################
    if (plot_proj_mode == "lcc") {
        if (debug) {
            print(paste("overlay mode is", plot_proj_mode))
        }
        inp <- df_long_lat_val
        ## Rename the columns
        colnames(inp) <- c("long", "lat", "val")

        ## Trim Extent, remove some stations
        if (overlay_spatial_trim) {
            inp <- inp[inp$long > trim_bound(trim_bound_def)[1], ]
            inp <- inp[inp$long < trim_bound(trim_bound_def)[2], ]
            inp <- inp[inp$lat > trim_bound(trim_bound_def)[3], ]
            inp <- inp[inp$lat < trim_bound(trim_bound_def)[4], ]
        }

        ## rearrenge colors to match
        conLimRange <- conLim[2] - conLim[1]
        bin_range <- conLimRange / length(fColor)
        fColorTemp <- character()

        ## bin_range
        if (debug) {
            print(paste("input:"))
            print(head(inp))
            print(paste("conLimRange:", conLimRange))
            print(paste("bin_range:", bin_range))
        }
        for (ii in 1:nrow(inp)) {
            if (debug) print(paste0("ii:", ii))
            for (jj in 1:length(fColor)) {
                if (debug) print(paste0("jj:", jj))
                if (debug) print(paste(inp$val[ii], bin_range * (jj - 1) + conLim[1], bin_range * jj + conLim[1]))
                if (inp$val[ii] >= bin_range * (jj - 1) + conLim[1] &&
                    inp$val[ii] < bin_range * jj + conLim[1]) {
                    fColorTemp[ii] <- fColor[jj]
                    break
                } else if (inp$val[ii] >= conLim[2]) {
                    fColorTemp[ii] <- fColor[length(fColor)]
                    break
                } else if (inp$val[ii] <= conLim[1]) {
                    fColorTemp[ii] <- fColor[1]
                    break
                }
            }
        }

        fColor <- fColorTemp

        if (debug) {
            print(paste("fColor:", head(fColor)))
        }

        ## Make spatial points
        sp::coordinates(inp) <- ~ long + lat
        sp::proj4string(inp) <- sp::CRS("+init=epsg:4326")
        inp <- sp::spTransform(x = inp, CRSobj = main_raster_crs)
        if (debug) {
            try({
                write.csv(x = inp, file = "overlay_inp_debug.csv", row.names = FALSE)
            })
        }

        if (variable_cex) {
            cex <- cex_normalized(inp$val, conLim = conLim)
        } else {
            cex <- df_cex
        }
        if (debug) {
            print(paste("cex:", head(cex)))
        }
        ## Add plot as a layer
        raster::plot(inp, bg = fColor, pch = 21, cex = cex, add = TRUE)
    }
}

################################################################################
################################################################################
################################################################################
############################# main ploting function ############################
################################################################################
################################################################################
################################################################################

spatial_plot <- function(raster_2_plot,
                         output_path,
                         output_name,
                         output_formats = c("png"),
                         loutput_formats_lr = TRUE,
                         output_formats_lr_res = 96,
                         output_formats_hr_res = 300,
                         specie = "",
                         unit = "",
                         df_long_lat_val,
                         df_cex = 1.3,
                         conLim,
                         conLim_mirror = FALSE,
                         title = "",
                         title_location = 1,
                         title_size = 1.6,
                         title_l2 = "",
                         fColor = myColorSimple(200),
                         colNA = "white",
                         overlay_plot_switch = FALSE,
                         overlay_spatial_trim = TRUE,
                         trim_bound_def = "CONUS",
                         variable_cex = FALSE,
                         l_legend = TRUE,
                         legend_thickness = 1,
                         legend_tick_size = 1,
                         legend_length = 0.5,
                         custom_legend = FALSE,
                         custom_legend_x = 0.1,
                         custom_legend_y = 0.05,
                         custom_legend_cex = 0.85,
                         custom_legend_names = "",
                         world_map = FALSE,
                         world_map_lwd = 1,
                         usa_map = TRUE,
                         usa_map_lwd = 1,
                         county_map = FALSE,
                         zone_map = FALSE,
                         zone_map_lwd = 2,
                         zone_def = "",
                         usa_city_names = FALSE,
                         states = "",
                         cities = "",
                         quick_stats = TRUE,
                         quick_stats_size = 1,
                         break_points = FALSE,
                         break_points_mid_white = FALSE,
                         easy = FALSE,
                         easy_fact = 10,
                         plot_mask_switch = FALSE,
                         plot_proj_mode = "longlat",
                         # nmb_nme = FALSE,
                         # nmb_nme_path = FALSE,
                         smooth_switch = TRUE,
                         output_height_inch = 8,
                         special_quick_stats = FALSE,
                         extra_border_switch = FALSE,
                         manual_adj_lab_x = 0,
                         manual_adj_lab_y = 40000,
                         log_plot = FALSE,
                         log_base = 10,
                         parm = par(mar = c(2, 2, 3, 1)),
                         osm = FALSE,
                         dotfiles_path,
                         silent = FALSE,
                         debug = FALSE) {
    ### start funciton ###
    suppressWarnings({
        timeTemp <- Sys.time()
        try(
            {
                for (i in 1:3) {
                    dev.off()
                }
            },
            silent = TRUE
        )
        if (silent) {
            debug <- FALSE
        }

        if (missing(dotfiles_path)) {
            dotfiles_paths <- c(
                paste0("~/dotfiles"),
                paste0("c:/Users/farzad.k/OneDrive - Northeastern University/git/dotfiles_hpc/"),
                paste0("~/Dropbox/git/dotfiles_hpc/"),
                NA
            )
            for (dotfiles_path in dotfiles_paths) {
                if (dir.exists(dotfiles_path)) {
                    break
                } else if (is.na(dotfiles_path)) {
                    print("dotfiles_path was not found in the list")
                    stop()
                }
            }
        }

        if (is.na(terra::crs(raster_2_plot))) {
            if (!silent) {
                print("no CRS on file! exiting!!")
            }
            stop()
        }
        if (!exists("file_crs") && !is.na(terra::crs(raster_2_plot)) && plot_proj_mode == "lcc") {
            file_crs <<- raster::proj4string(raster::crs(raster_2_plot))
            source(paste0(dotfiles_path, "/", "models/cmaq/post/script_pkg_sp_plots.R"))
        }

        if (title_location == 1 && !paste0(title_l2) == "") {
            title_location <- 2.5
        }

        if (log_plot) {
            raster_2_plot_bak <- raster_2_plot
            raster_2_plot <- log(raster_2_plot, base = log_base)
            raster_2_plot[is.infinite(raster_2_plot[])] <- NA
        }

        if (missing(conLim)) {
            conLim <- c(
                raster::cellStats(raster_2_plot, "min", na.rm = TRUE),
                raster::cellStats(raster_2_plot, "max", na.rm = TRUE)
            )
            if (break_points && conLim[1] == 0 && conLim[2] == 0) {
                conLim <- c(-99, 99)
            }
        } else if (conLim[1] == -999) {
            conLim <- c(
                raster::cellStats(raster_2_plot, "min", na.rm = TRUE),
                conLim[2]
            )
        } else if (conLim[2] == -999) {
            conLim <- c(
                conLim[1],
                raster::cellStats(raster_2_plot, "max", na.rm = TRUE)
            )
        }
        if (conLim_mirror) {
            if (missing(conLim)) {
                conLim <- c(
                    -max(
                        abs(raster::cellStats(raster_2_plot, "min", na.rm = TRUE)),
                        abs(raster::cellStats(raster_2_plot, "max", na.rm = TRUE))
                    ),
                    max(
                        abs(raster::cellStats(raster_2_plot, "min", na.rm = TRUE)),
                        abs(raster::cellStats(raster_2_plot, "max", na.rm = TRUE))
                    )
                )
                if (break_points && conLim[1] == 0) {
                    conLim <- c(-99, 99)
                }
            } else {
                conLim <- c(
                    -max(abs(conLim)),
                    max(abs(conLim))
                )
            }
        }

        if (missing(output_path)) {
            output_path <- paste0(
                stringr::str_replace_all(path.expand("~"), "\\\\", "/")
            )
        }
        if (missing(output_name)) {
            output_name <- paste0(
                "temp"
            )
        }

        if (debug) {
            print(paste("projection:", plot_proj_mode))
            print(paste("overlay:", overlay_plot_switch))
            # print(head(df_long_lat_val))
            print(paste("z limits:", conLim))
        }

        # Quick (very #quick #fast #easy #test #upscaled)
        if (easy) {
            raster_2_plot <- raster::aggregate(x = raster_2_plot, fact = easy_fact)
            if (debug) {
                print(paste("aggregate factor:", easy_fact))
                print("aggregated")
            }
        }


        if (debug) {
            print(dotfiles_path)
        }
        source(paste0(dotfiles_path, "/", "/models/cmaq/post/script_pkg_title.R"))

        make_break_func <- function(conLim = conLim, fColor = fColor,
                                    break_points = break_points,
                                    break_points_mid_white = break_points_mid_white) {
            if (!break_points) {
                break_points <- NULL
            } else {
                if (!break_points_mid_white) {
                    break_points <- c(seq(
                        from = conLim[1],
                        to = conLim[2],
                        by = (conLim[2] - conLim[1]) / length(fColor)
                    ))
                } else {
                    break_points <- c(seq(
                        from = conLim[1] - ((conLim[2] - conLim[1]) / (length(fColor) - 1)) / 2,
                        to = conLim[2] + ((conLim[2] - conLim[1]) / (length(fColor) - 1)) / 2,
                        by = (conLim[2] - conLim[1]) / (length(fColor) - 1)
                    ))
                }
            }
            return(break_points)
        }

        break_points <- make_break_func(
            conLim = conLim, fColor = fColor,
            break_points = break_points,
            break_points_mid_white = break_points_mid_white
        )

        if (osm) {
            osm_raster <- rosm::osm.raster(
                terra::ext(
                    terra::rast(
                        raster::projectRaster(
                            raster_2_plot,
                            crs = "EPSG:4326"
                        )
                    )
                ),
                quiet = TRUE
            )
        }

        ######## longlat #####
        if (plot_proj_mode == "longlat") {
            if (debug) {
                print(paste("mean before projection:", raster::cellStats(raster_2_plot, "mean", na.rm = T)))
            }

            plotRast <- raster::projectRaster(raster_2_plot,
                crs = "+init=epsg:4326", method = "bilinear"
            )
            if (log_plot) {
                plotRast_bak <- raster::projectRaster(raster_2_plot_bak,
                    crs = "+init=epsg:4326", method = "bilinear"
                )
            }
            if (debug) {
                print(plotRast)
            }
            xLim <- c(raster::extent(plotRast)[1], raster::extent(plotRast)[2])
            yLim <- c(raster::extent(plotRast)[3], raster::extent(plotRast)[4])

            if (debug) {
                print(paste(
                    "mean after projection:",
                    raster::cellStats(plotRast, "mean", na.rm = T)
                ))
            }

            ## Mask the file
            if (plot_mask_switch) {
                plotMasked <- raster::mask(plotRast, usa_longlat) # usaMask
                if (log_plot) {
                    plotMasked_bak <- raster::mask(plotRast_bak, usa_lcc)
                }
            } else {
                plotMasked <- plotRast
                if (log_plot) {
                    plotMasked_bak <- plotRast_bak
                }
            }

            if (debug) {
                print(paste(
                    "mean after conus mask:",
                    raster::cellStats(plotMasked, "mean", na.rm = T)
                ))
            }
            ## Change the values greater than upper limit to upper limit and vise versa
            plotRast_modified <- plotMasked
            plotRast_modified[plotRast_modified < conLim[1]] <- conLim[1]
            plotRast_modified[plotRast_modified > conLim[2]] <- conLim[2]
            if (debug) {
                print(paste(
                    "mean after limit changes:",
                    raster::cellStats(plotRast_modified, "mean", na.rm = T)
                ))
            }

            ######################## calculate output ratio ########################
            x_min <- raster::extent(plotRast_modified)[1] # raster_2_plot
            x_max <- raster::extent(plotRast_modified)[2]
            y_min <- raster::extent(plotRast_modified)[3]
            y_max <- raster::extent(plotRast_modified)[4]
            ratio <- (x_max - x_min) / (y_max - y_min)
            xratiokm <- abs(x_max - x_min) * 110
            yratiokm <- abs(y_max - y_min) * 110
            ratio_const <- 0.75

            if (debug) {
                print(paste(y_max, y_min))
                print(paste("ratio:", ratio))
                print(paste("yratiokm:", yratiokm))
                print(paste("ratio_const:", ratio_const))
            }
            height_value <- output_height_inch

            for (format in output_formats) {
                if (format == "pdf") {
                    pdf(
                        file = paste0(output_path, "/", output_name, ".pdf"),
                        width = height_value * ratio * ratio_const,
                        height = height_value
                    )
                } else if (format == "eps") {
                    postscript(
                        file = paste0(output_path, "/", output_name, ".eps"),
                        horizontal = FALSE, onefile = FALSE, paper = "special",
                        width = height_value * ratio * ratio_const,
                        height = height_value
                    )
                } else if (format == "png") {
                    if (loutput_formats_lr) {
                        png(
                            file = paste0(output_path, "/", output_name, ".png"),
                            res = output_formats_lr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    } else {
                        png(
                            file = paste0(output_path, "/", output_name, ".png"),
                            res = output_formats_hr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    }
                } else if (format == "tiff" || format == "tif") {
                    if (loutput_formats_lr) {
                        tiff(
                            file = paste0(output_path, "/", output_name, ".tiff"),
                            res = output_formats_lr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    } else {
                        tiff(
                            file = paste0(output_path, "/", output_name, ".tiff"),
                            res = output_formats_hr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    }
                } else {
                    return(print("unknown format"))
                }

                # png(
                #     file = paste0(output_path, "/", output_name),
                #     res = height_value / 7.7,
                #     width = height_value * ratio * 0.75,
                #     height = height_value
                # )

                # parm
                # https://stackoverflow.com/questions/20748747/how-to-increase-the-size-of-labels-of-a-legend-for-a-raster-and-make-them-bold
                raster::plot(plotRast_modified,
                    breaks = break_points,
                    col = fColor,
                    colNA = colNA,
                    zlim = conLim,
                    xlim = xLim,
                    ylim = yLim,
                    legend = l_legend,
                    legend.shrink = legend_length,
                    legend.width = legend_thickness,
                    axis.args = list(cex.axis = legend_tick_size),
                    # legend.args = list(text = "hi", font = 2, cex = 2, line = 2.5),
                    axes = FALSE,
                    interpolate = smooth_switch
                )
                if (debug) {
                    print(paste("main plot is fine"))
                }

                if (osm) {
                    osm_raster <- raster::projectRaster(osm_raster, plotRast_modified)
                    raster::plotRGB(osm_raster, add = TRUE)
                    unlink("rosm.cache", recursive = TRUE, force = TRUE)
                }

                ## Set Axis
                interval <- 10
                vet_lat <- seq(-180, 180, by = interval)
                lab_lat <- c(
                    paste0(seq(180, interval, by = -interval), "\U00B0", "W"),
                    "0", paste0(seq(interval, 180, by = interval), "\U00B0", "W")
                )
                interval <- 5
                vet_lon <- seq(-90, 90, by = interval)
                lab_lon <- c(
                    paste0(seq(90, interval, by = -interval), "\U00B0", "S"),
                    "0", paste0(seq(interval, 90, by = interval), "\U00B0", "N")
                )
                try(
                    {
                        axis(2, at = vet_lon, labels = lab_lon)
                        axis(1, at = vet_lat, labels = lab_lat)
                    },
                    silent = TRUE
                )
                ## Grid
                grid(col = "darkgrey", lty = 5, lw = 1.5)

                ## Map Layers
                if (world_map) {
                    raster::plot(world_longlat, add = TRUE, lwd = world_map_lwd)
                }
                if (usa_map) {
                    raster::plot(usa_longlat, add = TRUE, lwd = usa_map_lwd)
                }
                if (county_map) {
                    raster::plot(usac_longlat, add = TRUE, lwd = 1)
                }
                if (zone_map) {
                    if (zone_def == "") {
                        if (!silent) {
                            print("zone_def not defined")
                        }
                        return(extract_zone(
                            def = zone_def,
                            dotfiles_path = dotfiles_path
                        ))
                    }
                    for (zone in 1:extract_zone(
                        def = zone_def,
                        num_reg = TRUE, silent = TRUE,
                        dotfiles_path = dotfiles_path
                    )) {
                        # try(
                        #     {
                        temp_zone <- extract_zone(zone, zone_def,
                            plot_proj_mode = plot_proj_mode,
                            debug = debug, silent = !debug,
                            dotfiles_path = dotfiles_path
                        )
                        raster::plot(temp_zone, add = TRUE, lwd = zone_map_lwd)
                        #     },
                        #     silent = !debug
                        # )
                    }
                }
                if (debug) {
                    print("borders added")
                }

                if (usa_city_names) {
                    if (debug) {
                        print(paste("preparing usa_city_names"))
                    }
                    us_cities <- prepare_cities(
                        states1 = states, cities1 = cities,
                        proj = plot_proj_mode,
                        xratiokm1 = xratiokm, yratiokm1 = yratiokm,
                        debug = debug
                    )
                    if (debug) {
                        print(paste("us_cities:", us_cities))
                    }
                    terra::points(us_cities[[1]], pch = 16)
                    terra::text(us_cities[[2]], labels = us_cities[[2]]$name)
                    if (debug) {
                        print(paste("usa city names done"))
                    }
                }

                ## Titles
                func_make_title(
                    title = title, line_2 = title_l2,
                    unit = unit, specie = specie,
                    title_location = title_location, title_size = title_size
                )

                delta_x <- xLim[2] - xLim[1]
                delta_y <- yLim[2] - yLim[1]
                delta_ratio <- delta_x / delta_y
                if (debug) {
                    print(paste("delta ratio", delta_ratio))
                }
                if (delta_ratio < 1.5) { # 1.8
                    # print("dx/dy < 1.5")
                    text.width <- (delta_x * 1.5) / 6
                } else {
                    # print("dx/dy > 1.5, no change")
                    text.width <- delta_x / 6
                }
                # print(paste("tw:", text.width))

                ## Quick Statistics
                # is important to use the variable in the model original projection or calculate from a array instead of a raster
                if (quick_stats) {
                    raster_2_plot_maksed <- plotMasked
                    if (log_plot) {
                        raster_2_plot_maksed_bak <- plotMasked_bak
                        mi <- log(raster::cellStats(raster_2_plot_bak, "min", na.rm = TRUE), base = log_base)
                        me <- log(raster::cellStats(raster_2_plot_bak, "mean", na.rm = TRUE), base = log_base)
                        ma <- log(raster::cellStats(raster_2_plot_bak, "max", na.rm = TRUE), base = log_base)
                        su <- log(raster::cellStats(raster_2_plot_bak, "sum", na.rm = TRUE), base = log_base)
                    } else {
                        mi <- raster::cellStats(raster_2_plot_maksed, "min", na.rm = TRUE)
                        me <- raster::cellStats(raster_2_plot_maksed, "mean", na.rm = TRUE)
                        ma <- raster::cellStats(raster_2_plot_maksed, "max", na.rm = TRUE)
                        su <- raster::cellStats(raster_2_plot_maksed, "sum", na.rm = TRUE)
                    }

                    if (!special_quick_stats) {
                        legend_txt <- c(
                            paste("Min:", formatC(mi, digits = 2, format = "f")),
                            paste("Mean:", formatC(me, digits = 2, format = "f")),
                            paste("Max:", formatC(ma, digits = 2, format = "f"))
                        )
                    } else {
                        legend_txt <- c(
                            paste("Min:", formatC(mi, digits = 2, format = "f")),
                            paste("Sum:", formatC(su, digits = 2, format = "f")),
                            paste("Max:", formatC(ma, digits = 2, format = "f"))
                        )
                    }
                    legend("bottomright",
                        legend = legend_txt,
                        xjust = 0.5, horiz = TRUE, y.intersp = 0.1, x.intersp = 0.1,
                        # text.width = 10)# this is important to set the size of the box
                        text.width = text.width, cex = quick_stats_size
                    )
                }
                # text.width <- (xLim[2] - xLim[1]) * 10 / 65  # text.width <- 10 # old

                ## overlay plot
                if (overlay_plot_switch) {
                    add_overlay(
                        fColor = fColor, conLim = conLim,
                        df_long_lat_val = df_long_lat_val,
                        df_cex = df_cex,
                        main_raster_crs = sp::CRS(plotRast),
                        variable_cex = variable_cex,
                        overlay_spatial_trim = overlay_spatial_trim,
                        trim_bound_def = trim_bound_def,
                        debug = debug
                    )
                }
                if (custom_legend) {
                    par(xpd = TRUE)
                    xloc <- raster::extent(raster_2_plot_modified)[2] +
                        (raster::extent(raster_2_plot_modified)[2] -
                            raster::extent(raster_2_plot_modified)[1]) *
                            custom_legend_x
                    yloc <- (raster::extent(raster_2_plot_modified)[4] +
                        raster::extent(raster_2_plot_modified)[3]) +
                        (raster::extent(raster_2_plot_modified)[4] -
                            raster::extent(raster_2_plot_modified)[3]) *
                            custom_legend_y
                    legend(
                        x = xloc, y = yloc,
                        legend = custom_legend_names,
                        fill = fColor, cex = custom_legend_cex
                    )
                }
                ## Close Saving File
                dev.off()
            }
        }

        ############################################################################
        ########################### new mode (lcc / curve) #########################
        ############################################################################

        if (plot_proj_mode == "lcc") {
            ######################## calculate output ratio ########################
            if (extra_border_switch) {
                extra_border <- 1.05
                new_extent <- raster::extent(raster_2_plot) * extra_border
                raster_2_plot <- raster::extend(
                    x = raster_2_plot,
                    y = new_extent,
                    value = NA
                )
            }
            ######################## calculate output ratio ########################
            temp_extent <- raster::extent(raster_2_plot)
            x_min <- temp_extent[1]
            x_max <- temp_extent[2]
            y_min <- temp_extent[3]
            y_max <- temp_extent[4]
            ratio <- (x_max - x_min) / (y_max - y_min)
            xratiokm <- abs(x_max - x_min) / 1000
            yratiokm <- abs(y_max - y_min) / 1000
            ratio_const <- 0.95

            if (debug) {
                print(paste(y_max, y_min))
                print(paste("ratio:", ratio))
                print(paste("yratiokm:", yratiokm))
                print(paste("ratio_const:", ratio_const))
            }
            height_value <- output_height_inch

            ########################### make axis labels ###########################
            is_near_zero <- function(x, tol = 1e-2) {
                abs(x) < tol
            }
            ### x ###
            x_axis <- data.frame(c(x_min), seq(y_min, y_max, by = 100))
            colnames(x_axis) <- c("x", "y")
            sp::coordinates(x_axis) <- ~ x + y
            sp::proj4string(x_axis) <- sp::CRS(file_crs)
            x_axis_latlon <- sp::spTransform(x = x_axis, CRSobj = "+init=epsg:4326")
            x_axis_latlon_df <- as.data.frame(x_axis_latlon)
            x_axis_lcc_intervals <- data.frame()
            x_axis_lcc_intervals_names <- c()
            for (deg in seq(-90, 90, 10)) { # 10
                temp_diff <- abs(x_axis_latlon_df[, 2] - deg)
                temp_min <- min(temp_diff)
                temp_index <- which.min(temp_diff)

                if (is_near_zero(temp_min)) {
                    if (debug) {
                        print(paste(deg, "NS deg inserted"))
                    }
                    assign(paste0("index_", deg), temp_index)
                    x_axis_lcc_intervals <- rbind(x_axis_lcc_intervals, x_axis_latlon_df[temp_index, ])
                    x_axis_lcc_intervals_names <- c(x_axis_lcc_intervals_names, deg)
                }
            }
            if (debug) {
                print(paste("dim:", dim(x_axis_lcc_intervals)))
            }

            if (dim(x_axis_lcc_intervals)[1] != 0) {
                colnames(x_axis_lcc_intervals) <- c("lon", "lat")
                sp::coordinates(x_axis_lcc_intervals) <- ~ lon + lat
                sp::proj4string(x_axis_lcc_intervals) <- sp::CRS("+init=epsg:4326")
                x_axis_lcc_intervals <- sp::spTransform(
                    x = x_axis_lcc_intervals,
                    CRSobj = file_crs
                )
            }
            x_axis_lcc_intervals_df <- as.data.frame(x_axis_lcc_intervals)
            if (debug) {
                print(paste(x_axis_lcc_intervals_df))
                print("x okay")
            }
            ### y ###
            y_axis <- data.frame(seq(x_min, x_max, by = 100), c(y_min))
            colnames(y_axis) <- c("x", "y")
            sp::coordinates(y_axis) <- ~ x + y
            sp::proj4string(y_axis) <- sp::CRS(file_crs)
            y_axis_latlon <- sp::spTransform(x = y_axis, CRSobj = "+init=epsg:4326")
            y_axis_latlon_df <- as.data.frame(y_axis_latlon)
            y_axis_lcc_intervals <- data.frame()
            y_axis_lcc_intervals_names <- c()
            for (deg in seq(-180, 180, 10)) { # 10
                temp_diff <- abs(y_axis_latlon_df[, 1] + deg)
                temp_min <- min(temp_diff)
                temp_index <- which.min(temp_diff)

                if (is_near_zero(temp_min)) {
                    if (debug) {
                        print(paste(deg, "EW deg inserted"))
                    }
                    assign(paste0("index_", deg), temp_index)
                    y_axis_lcc_intervals <- rbind(y_axis_lcc_intervals, y_axis_latlon_df[temp_index, ])
                    y_axis_lcc_intervals_names <- c(y_axis_lcc_intervals_names, deg)
                }
            }
            if (debug) {
                print(paste("dim:", dim(y_axis_lcc_intervals)))
            }
            if (dim(y_axis_lcc_intervals)[1] != 0) {
                colnames(y_axis_lcc_intervals) <- c("lon", "lat")
                sp::coordinates(y_axis_lcc_intervals) <- ~ lon + lat
                sp::proj4string(y_axis_lcc_intervals) <- sp::CRS("+init=epsg:4326")
                y_axis_lcc_intervals <- sp::spTransform(
                    x = y_axis_lcc_intervals,
                    CRSobj = file_crs
                )
            }
            y_axis_lcc_intervals_df <- as.data.frame(y_axis_lcc_intervals)
            if (debug) {
                print(paste(y_axis_lcc_intervals_df))
                print("y okay")
            }
            ######################## create the grid in lcc ########################
            grid_lcc <- raster::raster(
                ext = raster::extent(-180, 0, 0, 90),
                res = c(10, 10)
            )
            grid_lcc <- raster::rasterToPolygons(grid_lcc)
            grid_lcc <- sp::spTransform(grid_lcc, CRSobj = file_crs)
            if (debug) {
                print(paste("grids created"))
            }
            ####################### mask cover outside usa #########################
            if (plot_mask_switch) {
                raster_2_plot_maksed <- raster::mask(raster_2_plot, usa_lcc) # usaMask
                if (log_plot) {
                    raster_2_plot_masked_bak <- raster::mask(raster_2_plot_bak, usa_lcc)
                }
            } else {
                raster_2_plot_maksed <- raster_2_plot
                if (log_plot) {
                    raster_2_plot_masked_bak <- raster_2_plot_bak
                }
            }
            ########################### fix min and max ############################
            raster_2_plot_modified <- raster_2_plot_maksed
            raster_2_plot_modified[raster_2_plot_modified < conLim[1]] <- conLim[1]
            raster_2_plot_modified[raster_2_plot_modified > conLim[2]] <- conLim[2]

            ########################## make and save plot ##########################
            for (format in output_formats) {
                if (format == "pdf") {
                    pdf(
                        file = paste0(output_path, "/", output_name, ".pdf"),
                        width = height_value * ratio * ratio_const,
                        height = height_value
                    )
                } else if (format == "eps") {
                    postscript(
                        file = paste0(output_path, "/", output_name, ".eps"),
                        horizontal = FALSE, onefile = FALSE, paper = "special",
                        width = height_value * ratio * ratio_const,
                        height = height_value
                    )
                } else if (format == "png") {
                    if (loutput_formats_lr) {
                        png(
                            file = paste0(output_path, "/", output_name, ".png"),
                            res = output_formats_lr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    } else {
                        png(
                            file = paste0(output_path, "/", output_name, ".png"),
                            res = output_formats_hr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    }
                } else if (format == "tiff" || format == "tif") {
                    if (loutput_formats_lr) {
                        tiff(
                            file = paste0(output_path, "/", output_name, ".tiff"),
                            res = output_formats_lr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    } else {
                        tiff(
                            file = paste0(output_path, "/", output_name, ".tiff"),
                            res = output_formats_hr_res,
                            units = "in",
                            width = height_value * ratio * ratio_const,
                            height = height_value
                        )
                    }
                } else {
                    return(print("unknown format"))
                }

                if (debug) {
                    print(raster_2_plot_modified)
                }

                raster::plot(raster_2_plot_modified,
                    breaks = break_points,
                    col = fColor,
                    colNA = colNA,
                    zlim = conLim,
                    xlim = c(x_min, x_max),
                    ylim = c(y_min, y_max),
                    legend = l_legend,
                    legend.shrink = legend_length,
                    legend.width = legend_thickness,
                    axis.args = list(cex.axis = legend_tick_size),
                    # legend.args = list(text = "hi", font = 2, cex = 2, line = 2.5),
                    axes = FALSE,
                    interpolate = smooth_switch
                )

                if (osm) {
                    osm_raster <- raster::projectRaster(osm_raster, plotRast_modified)
                    raster::plotRGB(osm_raster, add = TRUE)
                    unlink("rosm.cache", recursive = TRUE, force = TRUE)
                }

                if (debug) {
                    print(paste("plot made"))
                }

                if (world_map) {
                    raster::plot(world_lcc, add = TRUE, lwd = world_map_lwd)
                }
                if (usa_map) {
                    raster::plot(usa_lcc, add = TRUE, lwd = usa_map_lwd)
                }
                if (county_map) {
                    raster::plot(usac_lcc, add = TRUE, lwd = 1)
                }
                if (zone_map) {
                    if (zone_def == "") {
                        if (!silent) {
                            print("zone_def not defined")
                        }
                        return(extract_zone(
                            def = zone_def,
                            dotfiles_path = dotfiles_path
                        ))
                    }
                    for (zone in 1:extract_zone(
                        def = zone_def,
                        num_reg = TRUE, silent = TRUE,
                        dotfiles_path = dotfiles_path
                    )) {
                        # try(
                        #     {
                        temp_zone <- extract_zone(zone, zone_def,
                            plot_proj_mode = plot_proj_mode, debug = debug,
                            silent = !debug, dotfiles_path = dotfiles_path
                        )
                        raster::plot(temp_zone, add = TRUE, lwd = zone_map_lwd)
                        #         },
                        #         silent = !debug
                        #     )
                    }
                }
                if (debug) {
                    print("borders added")
                }
                # https://www.benjaminbell.co.uk/2018/02/quick-guide-to-line-types-lty-in-r.html
                # https://www.colorhexa.com/6e6e6e
                raster::plot(grid_lcc,
                    add = TRUE,
                    border = "#6e6e6e", lwd = 1.2, lty = c(3)
                )

                if (debug) {
                    print("grid added")
                }
                if (usa_city_names) {
                    us_cities <- prepare_cities(
                        states1 = states, cities1 = cities,
                        proj = plot_proj_mode,
                        xratiokm1 = xratiokm, yratiokm1 = yratiokm,
                        debug = debug
                    )
                    terra::points(us_cities[[1]], pch = 16)
                    terra::text(us_cities[[2]], labels = us_cities[[2]]$name)
                }

                try(
                    {
                        axis(2,
                            at = x_axis_lcc_intervals_df[, 2] + manual_adj_lab_y, # + 10000,
                            labels = paste0(
                                x_axis_lcc_intervals_names,
                                "\U00B0", "N"
                            )
                        )
                        axis(1,
                            at = y_axis_lcc_intervals_df[, 1] + manual_adj_lab_x, # - 25000,
                            labels = paste0(
                                y_axis_lcc_intervals_names,
                                "\U00B0", "W"
                            )
                        )
                    },
                    silent = TRUE
                )

                func_make_title(
                    title = title, line_2 = title_l2,
                    unit = unit, specie = specie,
                    title_location = title_location, title_size = title_size
                )

                delta_x <- x_max - x_min

                if (ratio < 1.5) {
                    text.width <- (delta_x * 1.5) / 6
                } else {
                    text.width <- delta_x / 6
                }
                if (debug) {
                    print(paste("tw:", text.width))
                }

                ## Quick Statistics
                # is important to use the variable in the model original projection or calculate from a array instead of a raster
                if (quick_stats) {
                    if (log_plot) {
                        mi <- log(raster::cellStats(raster_2_plot_masked_bak, "min", na.rm = TRUE), base = log_base)
                        me <- log(raster::cellStats(raster_2_plot_masked_bak, "mean", na.rm = TRUE), base = log_base)
                        ma <- log(raster::cellStats(raster_2_plot_masked_bak, "max", na.rm = TRUE), base = log_base)
                        su <- log(raster::cellStats(raster_2_plot_masked_bak, "sum", na.rm = TRUE), base = log_base)
                    } else {
                        mi <- raster::cellStats(raster_2_plot_maksed, "min", na.rm = TRUE)
                        me <- raster::cellStats(raster_2_plot_maksed, "mean", na.rm = TRUE)
                        ma <- raster::cellStats(raster_2_plot_maksed, "max", na.rm = TRUE)
                        su <- raster::cellStats(raster_2_plot_maksed, "sum", na.rm = TRUE)
                    }

                    if (!special_quick_stats) {
                        legend_txt <- c(
                            paste("Min:", formatC(mi, digits = 2, format = "f")),
                            paste("Mean:", formatC(me, digits = 2, format = "f")),
                            paste("Max:", formatC(ma, digits = 2, format = "f"))
                        )
                    } else {
                        legend_txt <- c(
                            paste("Min:", formatC(mi, digits = 2, format = "f")),
                            paste("Sum:", formatC(su, digits = 2, format = "f")),
                            paste("Max:", formatC(ma, digits = 2, format = "f"))
                        )
                    }
                    legend("bottomright",
                        legend = legend_txt,
                        xjust = 0.5, horiz = TRUE, y.intersp = 0.1, x.intersp = 0.1,
                        # text.width = 10)# this is important to set the size of the box
                        text.width = text.width, cex = quick_stats_size
                    )
                }
                ## overlay plot
                if (overlay_plot_switch) {
                    add_overlay(
                        fColor = fColor, conLim = conLim,
                        plot_proj_mode = plot_proj_mode,
                        df_long_lat_val = df_long_lat_val,
                        df_cex = df_cex,
                        main_raster_crs = file_crs,
                        variable_cex = variable_cex,
                        overlay_spatial_trim = overlay_spatial_trim,
                        trim_bound_def = trim_bound_def,
                        debug = debug
                    )
                }
                if (custom_legend) {
                    par(xpd = TRUE)
                    xloc <- raster::extent(raster_2_plot_modified)[2] +
                        (raster::extent(raster_2_plot_modified)[2] -
                            raster::extent(raster_2_plot_modified)[1]) *
                            custom_legend_x
                    yloc <- (raster::extent(raster_2_plot_modified)[4] +
                        raster::extent(raster_2_plot_modified)[3]) +
                        (raster::extent(raster_2_plot_modified)[4] -
                            raster::extent(raster_2_plot_modified)[3]) *
                            custom_legend_y
                    legend(
                        x = xloc, y = yloc,
                        legend = custom_legend_names,
                        fill = fColor, cex = custom_legend_cex
                    )
                }
                dev.off()
            }
            try(file.remove("temp.png"), silent = TRUE)
            if (!silent) {
                print(paste0(
                    length(output_formats), " plot(s) made in: ",
                    format(Sys.time() - timeTemp, digits = 2)
                ))
            }
            rm(timeTemp)
        }
    })
}

print("KF plot script was called succefully")

################################################################################
################################################################################
##################################### test #####################################
################################################################################
################################################################################
if (FALSE) {
    source(paste0(R.utils::getParent(this.path::this.path()), "/script_pkg_read_nc.R"))
    pm25 <- in_nc_out_rast(input_path = R.utils::getParent(this.path::this.path()), input_nc_name = "test.nc")
    source(paste0(R.utils::getParent(this.path::this.path()), "/script_pkg_vars_spec.R"))
    source(paste0(R.utils::getParent(this.path::this.path()), "/script_pkg_sp_plots.R"))
    variable <- ""
    # func_get_var_spec(variable = variable, debug = TRUE)
    spatial_plot(
        raster_2_plot = pm25,
        unit = func_get_var_spec(variable = variable),
        specie = func_get_var_spec(variable = variable, rqst = "name"),
        conLim = c(0, func_get_var_spec(variable = variable, rqst = "lim_abs")),
        plot_proj_mode = "lcc",
        overlay_plot_switch = FALSE,
        l_legend = TRUE,
        fColor = myColorSimple(200),
        title = "Title",
        quick_stats = TRUE,
        plot_mask_switch = FALSE,
        output_name = paste0("PM2.5"),
        output_path = paste0("~/")
    )
}


################################################################################
################################################################################
################################# recycle bin ##################################
################################################################################
################################################################################

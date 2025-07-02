in_nc_out_rast <- function(input_path,
                           input_nc_name,
                           variable,
                           file_full_path,
                           outp_vars = FALSE,
                           outp_vars_show = TRUE,
                           outp_time = FALSE,
                           outp_info = FALSE,
                           outp_zero = FALSE,
                           outp_nan = FALSE,
                           plot = FALSE,
                           dotfiles_path,
                           band = 1,
                           average = FALSE,
                           average_method = 1,
                           silent = TRUE,
                           debug = FALSE,
                           outp_keep_intact = FALSE,
                           plot_extra_arg, ...) {
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
            }
        }
    }
    if (missing(file_full_path)) {
        file_full_path <- paste0(input_path, "/", input_nc_name)
    }
    input_nc_file <- ncdf4::nc_open(file_full_path)
    file_variables <- unlist(base::attributes(input_nc_file$var))
    file_variables <- file_variables[!file_variables == "TFLAG" &
        !file_variables == "X" &
        !file_variables == "Y" &
        !file_variables == "ETFLAG" &
        !file_variables == "longitude" &
        !file_variables == "latitude" &
        !file_variables == "xcoord" &
        !file_variables == "ycoord" &
        !file_variables == "stkheight" &
        !file_variables == "stkdiam" &
        !file_variables == "stktemp" &
        !file_variables == "stkspeed" &
        !file_variables == "pigflag" &
        !file_variables == "saoverride" &
        !file_variables == "flowrate" &
        !file_variables == "plumerise" &
        !file_variables == "plume_bottom" &
        !file_variables == "plume_top"]

    if (outp_vars || outp_time || outp_info || outp_zero || outp_nan) {
        variable <- file_variables[1]
    } else {
        if (length(file_variables) == 1) {
            if (silent) if (Sys.info()[["sysname"]] == "Windows") sink(file = "NUL", append = FALSE) else sink(file = "/dev/null", append = FALSE)
            print(paste("only one variable", file_variables[1], "is available"))
            if (silent) sink()
            variable <- file_variables[1]
        } else if (missing(variable)) {
            return(print("'variable' is missing, give the variable or outp_vars"))
        }
    }

    if (outp_vars) {
        colnames(file_variables) <- NULL
        if (outp_vars_show) {
            print("included variables:")
            print(as.character(file_variables))
        }
        return(file_variables)
    }

    NCOLS <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "NCOLS")[2])
    NROWS <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "NROWS")[2])
    P_ALP <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "P_ALP")[2])
    P_BET <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "P_BET")[2])
    P_GAM <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "P_GAM")[2])
    XCENT <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "XCENT")[2])
    YCENT <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "YCENT")[2])
    XORIG <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "XORIG")[2])
    YORIG <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "YORIG")[2])
    XCELL <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "XCELL")[2])
    YCELL <- as.numeric(ncdf4::ncatt_get(input_nc_file, 0, "YCELL")[2])

    unit <- trimws(input_nc_file$var[[variable]]$units)
    if (debug) {
        print(paste("unit:", unit))
    }
    if (!is.null(unit) && length(unit) > 0) {
        if (unit == "ppmv" || unit == "ppm") {
            unit_multi <- 1000
        } else {
            unit_multi <- 1
        }
    }

    if (outp_time) {
        time <- ncdf4::ncvar_get(input_nc_file, "TFLAG")
        # return(TFLAG)
        if (length(dim(time)) == 3) {
            time_c <- c()
            for (t in 1:length(c(time[1, 1, ]))) {
                time_c <- c(time_c, paste0(time[1, 1, t], stringr::str_pad(time[2, 1, t], 6, "left", 0)))
            }
            time_c <- list(time, time_c, format(as.POSIXct(time_c, format = "%Y%j%H%M%S"), "%Y-%m-%d %H:%M:%S"))
        } else if (length(dim(time)) == 2) {
            time_c <- list(time, paste0(time[1, 1], time[2, 1]), NULL)
        } else {
            print("TFLAG dimention not supported")
        }
        return(time_c)
    }
    ncdf4::nc_close(input_nc_file)

    file_crs <- paste0(
        "+proj=lcc", " ",
        "+lat_1=", P_ALP, " ",
        "+lat_2=", P_BET, " ",
        "+lat_0=", YCENT, " ",
        "+lon_0=", XCENT, " ",
        "+x_0=0", " ",
        "+y_0=0", " ",
        "+datum=WGS84 +a=6370000 +b=6370000 +units=m +no_defs"
    )
    file_crs <<- raster::proj4string(sp::CRS(file_crs))
    file_extent <- raster::extent(
        XORIG,
        XORIG + NCOLS * XCELL,
        YORIG,
        YORIG + NROWS * YCELL
    )

    if (outp_info) {
        return(list(file_crs, file_extent))
    }

    if (tolower(band) != "all") {
        if (silent) if (Sys.info()[["sysname"]] == "Windows") sink(file = "NUL", append = FALSE) else sink(file = "/dev/null", append = FALSE)
        print("your nc file may have more bands, you can set band = 'all'")
        input_raster_file <- raster::raster(file_full_path,
            varname = paste0(variable), band = band
        )
        if (silent) sink()
    } else {
        if (silent) if (Sys.info()[["sysname"]] == "Windows") sink(file = "NUL", append = FALSE) else sink(file = "/dev/null", append = FALSE)
        input_raster_file <- raster::stack(file_full_path,
            varname = paste0(variable)
        )
        if (silent) sink()
        if (average) {
            if (average_method == 1) {
                input_raster_file <- raster::calc(input_raster_file,
                    mean,
                    na.rm = TRUE
                )
            } else if (average_method == 2) {
                temp <- input_raster_file[1]
                for (i in 2:raster::nlayers(input_raster_file)) {
                    temp <- temp + input_raster_file[i]
                }
                input_raster_file <- temp / raster::nlayers(input_raster_file)
            }
        }
    }

    raster::extent(input_raster_file) <- file_extent
    terra::crs(input_raster_file) <- file_crs

    if (!outp_keep_intact) { # for unit correction for now
        input_raster_file <- input_raster_file * unit_multi
    }

    if (outp_zero || outp_nan) {
        if (outp_zero) {
            input_raster_file <- raster::raster(
                nrows = NROWS,
                ncols = NCOLS,
                crs = file_crs,
                ext = file_extent,
                vals = 0
            )
            return(input_raster_file)
        }
        if (outp_nan) {
            input_raster_file <- raster::raster(
                nrows = NROWS,
                ncols = NCOLS,
                crs = file_crs,
                ext = file_extent,
                vals = NA
            )
            return(input_raster_file)
        }
    } else {
        if (plot) {
            source(paste0(dotfiles_path, "/models/cmaq/post/script_pkg_sp_plots.R"))
            do.call(what = spatial_plot, args = list(
                raster_2_plot = input_raster_file, output_path = input_path,
                output_name = paste0(input_nc_name, "_", variable),
                plot_mask_switch = FALSE, ...
            ))
        }
        return(input_raster_file)
    }
}

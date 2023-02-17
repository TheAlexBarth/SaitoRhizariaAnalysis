# Rhizaria analysis
rm(list = ls())
library(EcotaxaTools)
library(ggplot2)


rhiz_data <- readRDS('./data/00_rhizaria.rds')

# Rhizaria renaming

rhiz_data <- rhiz_data |> 
  add_zoo(names_to, col_name = 'name',
          new_names = c('Acantharea','Phaeodaria',
                        'Collodaria','Foraminifera',
                        'Rhizaria'))

rhiz_data <- rhiz_data |> 
  add_zoo(biovolume, col_name = 'biovol',
          shape = 'ellipsoid',pixel_mm = unique(rhiz_data$meta$acq_pixel))


# create depth breaks
create_depth_bins <- function(par_file) {
  
  max_d <- max(par_file$depth)
  
  if(max_d < 200) {
    return(seq(0,200,40))
  } else if (max_d < 1300) {
    
    return(c(seq(0,200,40), seq(300,1300,100)))
  } else {
    dbreaks <- c(seq(0,200,40),
                 seq(300,1400,100),
                 seq(1600, max_d, 200))
    
    if(max_d > max(dbreaks)) {
      dbreaks <- c(dbreaks, max_d)
    }
    
    return(dbreaks)
  }
}

dbreaks_list <- lapply(rhiz_data$par_files, create_depth_bins)

# Get counts 
rhiz_counts <- list()
for(cast in names(rhiz_data$zoo_files)) {
  rhiz_counts[[cast]] <- uvp_zoo_conc(rhiz_data,
                                      cast_name = cast,
                                      breaks = dbreaks_list[[cast]])|> 
    bin_format()
}


# get biovolumes
rhiz_bv <- list()
for(cast in names(rhiz_data$zoo_files)) {
  rhiz_bv[[cast]] <- uvp_zoo_conc(rhiz_data,
                                  cast_name = cast,
                                  breaks = dbreaks_list[[cast]],
                                  func_col = 'biovol',
                                  func = 'sum') |> 
    bin_format()
}




# rename profileid to correct ctdid
names(rhiz_counts) <- rhiz_data$meta$ctd_origfilename
names(rhiz_bv) <- rhiz_data$meta$ctd_origfilename


# Quick plotting 
rhiz_colors <- c(
  `Phaeodaria`= gg_cbb_col(1),
  `Collodaria` = gg_cbb_col(2)[2],
  `Acantharea`= gg_cbb_col(3)[3],
  `Foraminifera`= gg_cbb_col(4)[4],
  `Rhizaria`= gg_cbb_col(5)[5]
)

rhiz_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(rhiz_colors[cols])
  } else {
    return(rhiz_colors[cols])
  }
}

plot_cast <- function(profile, ylab, subtitle) {
  ggplot(profile) + 
    geom_rect(aes(xmin = min_d,
                  xmax = max_d,
                  ymax = conc_m3,
                  ymin = 0,
                  fill = group),
              alpha = 0.75) +
    facet_wrap(~group)+
    scale_fill_manual(values = rhiz_cols(unique(profile$group)))+
    scale_x_reverse()+
    coord_flip()+
    labs(x = 'Depth [m]', y = ylab,
         subtitle = subtitle, fill = "", color = "")+
    guides(fill = 'none')+
    theme_bw()
}

pdf('./plots/rhizaria_counts.pdf')
for(cast in names(rhiz_counts)) {
  plot_cast(rhiz_counts[[cast]], ylab = 'num/m^3', subtitle = cast) |> 
    print()
}
dev.off()

pdf('./plots/rhizara_bv.pdf')
for(cast in names(rhiz_bv)) {
  plot_cast(rhiz_bv[[cast]], ylab = 'mm^3/m^3', subtitle = cast) |> 
    print()
}
dev.off()



#format data to save
for(cast in names(rhiz_counts)) {
  stationid <- rhiz_data$meta$stationid[which(rhiz_data$meta$ctd_origfilename == cast)]
  rhiz_counts[[cast]][['stationid']] = stationid
  rhiz_bv[[cast]][['stationid']] <- stationid
}

rhiz_counts |> 
  list_to_tib() |> 
  write.csv('./data/rhizaria_bin_counts.csv',
            row.names = F)
  

rhiz_bv |> 
  list_to_tib() |> 
  write.csv('./data/rhizaria_bin_biovolume.csv',
            row.names = F)


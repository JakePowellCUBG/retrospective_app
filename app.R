library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(htmlwidgets)
library(shinyWidgets)
library(colourpicker)

add_alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, grDevices::col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">POWO</a>',val)
}

regions_plot <- function(event_data, ylim, add_annotate = FALSE, alpha = 1){
  p = plot_ly()
  xvalues = NULL
  if(nrow(event_data) > 0){
    for(i in 1:nrow(event_data)){
      xmin = lubridate::decimal_date(as.Date(event_data[i,2]))
      xmax = lubridate::decimal_date(as.Date(event_data[i,3]))
      if(xmin == xmax){
        p <- p %>% plotly::add_lines(x = c(xmin,xmax), y = ylim, name = event_data[i,4],  hoverinfo = 'text', hovertext = event_data[i,4], inherit = FALSE, showlegend = FALSE, line=list(width=3, color = add_alpha(event_data[i,1], alpha = alpha) ))
      }else{
        p <- p %>% plotly::add_polygons(x = c(xmin, xmin,xmax,xmax), y = c(ylim, rev(ylim)), name = event_data[i,4],  inherit = FALSE, showlegend = FALSE, line=list(width=1, color = event_data[i,1]), fillcolor=add_alpha(event_data[i,1], alpha = alpha), hoverinfo = 'text', hovertext = event_data[i,4])
      }
    xvalues = c(xvalues,(xmin+xmax)/2)

    }

    if(add_annotate){
      a <- list(
        x = xvalues,
        y = rep(ylim[2]-(ylim[2]/7), length(xvalues)),
        text = event_data[,4],
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        textangle = -90,
        xanchor = 'center',
        textfont = list(size = rep(8, length(xvalues)))
        # arrowhead = 7,
        # ax = 20,
        # ay = -40
      )
      p <- p %>% layout(annotations = a)
    }
  }




  return(p)
}

exist_at_date <- function(date, acc, ItemStatusDate, ItemStatusType){

  date = as.Date(date)
  # Accession date.
  pre_date = rep(NA,length(acc)) ; pre_date_char = rep(NA,length(acc))
  pre_date[which(acc > 1650)] =as.Date(paste(acc[which(acc > 1650)], '01', '01', sep = "-"), "%Y-%m-%d")
  pre_date_char[which(acc > 1650)] = paste(acc[which(acc > 1650)], '01', '01', sep = "-")

  # use the current day, unless not existing then use the date of that entry.
  post_date = rep(as.character(Sys.Date()),length(acc))
  post_date[ItemStatusType == 'NotExisting'] = ItemStatusDate[ItemStatusType == 'NotExisting']
  # If only a year is given assume it occurs on the 31st of Dec.
  post_date[stringr::str_length(post_date) == 4] = paste( post_date[stringr::str_length(post_date) == 4], '12', '31', sep = "-")
  # If only a year and month is given assume the day is the 28th.
  post_date[stringr::str_length(post_date) == 7] = paste( post_date[stringr::str_length(post_date) == 7], '28', sep = "-")
  dates = data.frame(pre = pre_date_char, post = post_date, pre_date = pre_date, post_date = as.Date(post_date, "%Y-%m-%d"))

  # Vector of whether the plant is existing on the date.
  out = matrix(NA, nrow = length(acc), ncol = length(date))
  for(i in 1:length(date)){
    existing_on_date = rep(FALSE, length(acc))
    existing_on_date[which(date[i] >= dates$pre_date & date[i] < dates$post_date)] = TRUE

    out[,i] = existing_on_date
  }

  out = data.frame(out)
  names(out) = date

  return(out)
}

get_chart_data_turnover <- function(data_working, data_plant_existing, turnover_type, turnover_quantity, turnover_type_of_chart, years){
  # setup output
  output <- list()

  # STEP 1: What type of turnover are we looking at.
  if(turnover_type == 'Loss'){
    DeathYear = as.numeric(stringr::str_sub(string = data_working$DeathDate, start = 1,end = 4))
    inYear = DeathYear
    output$turnover_type = 'Lost'
  }
  if(turnover_type == 'Gain'){
    inYear = data_working$AccYear
    output$turnover_type = 'New'
  }
  # This is where all the wanted data for charts is created.
  if(turnover_type %in% c('Gain','Loss')){
    # For Gain accessions we only have the field AccYear to know what are Gain records each year.

    # Get data for Items and Accessions charts.
    if(turnover_quantity == 'Items'){
      # Nothing needed.
      output$turnover_type_data = 'Items'
    }
    if(turnover_quantity == 'Accessions' && turnover_type == 'Gain'){
      # Need to reduce to only Gain accessions, by definition all accessions should have the same accession year.
      # Therefore we only need to strip the records to unique accessions.
      items = data_working$ItemAccNoFull
      accessions = unlist(lapply(stringr::str_split(items,pattern = '\\*'),function(x){x[1]}))
      unique_accessions = unique(accessions)

      match_to_best = match(unique_accessions,accessions)

      data_working = data_working[match_to_best,]
      data_plant_existing = data_plant_existing[match_to_best,]
      inYear = inYear[match_to_best]

      output$turnover_type_data = 'Accessions'
    }
    if(turnover_quantity == 'Accessions' && turnover_type == 'Loss'){
      # Need to reduce to unique accessions where the record kept has the most recent death year (if the accession has died).
      # Variable inYear contains the years that items die.
      items = data_working$ItemAccNoFull
      accessions = unlist(lapply(stringr::str_split(items,pattern = '\\*'),function(x){x[1]}))
      unique_accessions = unique(accessions)

      # Order the items where the lower indices have the most recent death date.
      inYear_dummy = inYear
      inYear_dummy[is.na(inYear_dummy)] = 4000
      ordered_items = order(inYear_dummy,decreasing = T)

      #Match to the ordered accessions.
      match_to_best = match(unique_accessions,accessions[ordered_items])

      #Reduce the data to unique accessions with most recent death date.
      data_working = data_working[ordered_items[match_to_best],]
      data_plant_existing = data_plant_existing[ordered_items[match_to_best],]
      inYear = inYear[ordered_items[match_to_best]]

      output$turnover_type_data = 'Accessions'
    }
    if(turnover_quantity %in% c('Items', 'Accessions') && turnover_type_of_chart == 'Number Over Time'){
      new_objects = lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        no_objects = nrow(garden_current)

        return(no_objects)
      })
      wanted = data.frame(year = years, no_wanted = unlist(new_objects))

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity %in% c('Items', 'Accessions') && turnover_type_of_chart == 'Divided into Type of Taxa'){
      new_objects = data.frame(t(data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Breakdown of infrageneric diversity.
        breakdown_infrageneric = table(garden_current$infrageneric_level)
        indet = sum(grepl('0', garden_current$infrageneric_level))
        hort = sum(grepl('5|6', garden_current$infrageneric_level))
        infra =  sum(grepl('2|3|4', garden_current$infrageneric_level))
        species =  sum(grepl('1', garden_current$infrageneric_level))

        return(c(species, infra, hort, indet))
      }))))
      names(new_objects) = c('Species','Infraspecific', 'Horticultral','Indeterminate')

      wanted = data.frame(year = years, new_objects)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity %in% c('Items', 'Accessions') && turnover_type_of_chart == 'Divided into Provenance'){
      unique_provenance_values = unique(data_working$ProvenanceCode)

      new_objects = data.frame(t(data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Breakdown of provenance.
        provenance = table(garden_current$ProvenanceCode)

        no_prov = as.numeric(provenance[match(unique_provenance_values, names(provenance))])
        no_prov[is.na(no_prov)] = 0

        return(no_prov)
      }))))
      names(new_objects) = unique_provenance_values

      wanted = data.frame(year = years, new_objects)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity %in% c('Items', 'Accessions') && turnover_type_of_chart == 'Divided into Endemic Species'){
      unique_values = unique(data_working$endemic)

      new_objects = data.frame(t(data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Breakdown of endemic
        endemic = table(garden_current$endemic)

        no_endemic = as.numeric(endemic[match(unique_values, names(endemic))])
        no_endemic[is.na(no_endemic)] = 0

        return(no_endemic)
      }))))
      names(new_objects) = unique_values

      wanted = data.frame(year = years, new_objects)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity %in% c('Items', 'Accessions') && turnover_type_of_chart == 'Divided into Threatened Species'){
      unique_values = unique(data_working$threatened)

      new_objects = data.frame(t(data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Breakdown of endemic
        threatened = table(garden_current$threatened)

        no_threatened = as.numeric(threatened[match(unique_values, names(threatened))])
        no_threatened[is.na(no_threatened)] = 0

        return(no_threatened)
      }))))
      names(new_objects) = unique_values

      wanted = data.frame(year = years, new_objects)

      output$turnover_wanted_data = wanted
    }


    # Create the raw number of 'quantity' accessioned each year
    if(turnover_quantity == 'Taxa'  && turnover_type_of_chart %in% c('Number Accessioned Over Time','Number Lost Over Time')){
      output$turnover_type_data = 'Taxa'

      new_objects = lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        Taxa = unique(garden_current$good_name)
        no_objects <- length(Taxa)

        return(no_objects)
      })
      wanted = data.frame(year = years, no_wanted = unlist(new_objects))

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Species'  && turnover_type_of_chart %in% c('Number Accessioned Over Time','Number Lost Over Time')){
      output$turnover_type_data = 'Species'
      # restrict to only species.
      # restrict to only species.
      indices = grepl('1',data_working$infrageneric_level)
      data_working = data_working[indices,]
      data_plant_existing = data_plant_existing[indices,]
      inYear = inYear[indices]

      new_objects = lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        species = unique(garden_current$good_name)
        no_objects <- length(species)

        return(no_objects)
      })
      wanted = data.frame(year = years, no_wanted = unlist(new_objects))

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Genera'  && turnover_type_of_chart %in% c('Number Accessioned Over Time','Number Lost Over Time')){
      output$turnover_type_data = 'Genera'

      new_objects = lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        genus = unique(garden_current$genus)
        no_objects <- length(genus)

        return(no_objects)
      })
      wanted = data.frame(year = years, no_wanted = unlist(new_objects))

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Families' && turnover_type_of_chart %in% c('Number Accessioned Over Time','Number Lost Over Time')){
      output$turnover_type_data = 'Families'

      new_objects = lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        family = unique(garden_current$family)
        no_objects <- length(family)

        return(no_objects)
      })
      wanted = data.frame(year = years, no_wanted = unlist(new_objects))

      output$turnover_wanted_data = wanted
    }

    # For taxa create the line chart split by the type of taxa.
    if(turnover_quantity == 'Taxa' && turnover_type_of_chart == 'Divided into Type of Taxa'){
      # Reduce data working to only unique taxa.
      unique_taxa = unique(data_working$good_name)

      match_to_best = match(unique_taxa,data_working$good_name)

      data_working = data_working[match_to_best,]
      data_plant_existing = data_plant_existing[match_to_best,]
      inYear = inYear[match_to_best]

      # Get the number of each infraspecific category.
      new_objects = data.frame(t(data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Breakdown of infrageneric diversity.
        breakdown_infrageneric = table(garden_current$infrageneric_level)
        indet = sum(grepl('0', garden_current$infrageneric_level))
        hort = sum(grepl('5|6', garden_current$infrageneric_level))
        infra =  sum(grepl('2|3|4', garden_current$infrageneric_level))
        species =  sum(grepl('1', garden_current$infrageneric_level))

        return(c(species, infra, hort, indet))
      }))))
      names(new_objects) = c('Species','Infraspecific', 'Horticultral','Indeterminate')

      wanted = data.frame(year = years, new_objects)

      output$turnover_wanted_data = wanted
    }

    # Create the New 'quantity' added to the collection each year (not in collection previous year, added in shown year)
    if(turnover_quantity == 'Taxa' && turnover_type_of_chart == 'Number of New to Collection' && turnover_type == 'Gain'){
      #Set name to Taxa for title of the chart.
      output$turnover_type_data = 'Taxa'

      #Get all the Taxa from the database.
      all_Taxa = unique(data_working$good_name)
      all_Taxa_without_author = data_working$sanitised_taxon[match(all_Taxa, data_working$good_name)]
      #Get which Taxa are accessioned each year.
      Taxa_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        Taxa = unique(garden_current$good_name)

        return(all_Taxa %in% Taxa)
      }))
      rownames(Taxa_accessioned_each_year) = all_Taxa
      colnames(Taxa_accessioned_each_year) = years

      #Get the Taxa in the collection each year
      Taxa_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        Taxa = unique(garden_current$good_name)

        return(all_Taxa %in% Taxa)
      }))





      new_to_year = rep(NA, ncol(Taxa_in_collection_each_year)-1)
      new_to_year_Taxa = rep(NA, ncol(Taxa_in_collection_each_year)-1)
      for(i in 2:ncol(Taxa_in_collection_each_year)){
        new_in_year = Taxa_accessioned_each_year[,i]  &  !Taxa_in_collection_each_year[,i-1]
        new_to_year[i-1] = sum(new_in_year)
        new_to_year_Taxa[i-1] = paste0(all_Taxa_without_author[new_in_year],collapse=', ')
      }


      wanted = data.frame(year = years[-1], new_to_year = new_to_year, new_Taxa = new_to_year_Taxa)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Species' && turnover_type_of_chart == 'Number of New to Collection' && turnover_type == 'Gain'){
      #Set name to Species for title of the chart.
      output$turnover_type_data = 'Species'
      # restrict to only species.
      indices = grepl('1',data_working$infrageneric_level)
      data_working = data_working[indices,]
      data_plant_existing = data_plant_existing[indices,]
      inYear = inYear[indices]

      #Get all the Species from the database.
      all_Species = unique(data_working$good_name)
      all_species_without_author = data_working$sanitised_taxon[match(all_Species, data_working$good_name)]
      #Get which Species are accessioned each year.
      Species_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        Species = unique(garden_current$good_name)

        return(all_Species %in% Species)
      }))
      rownames(Species_accessioned_each_year) = all_Species
      colnames(Species_accessioned_each_year) = years

      #Get the Species in the collection each year
      Species_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        Species = unique(garden_current$good_name)

        return(all_Species %in% Species)
      }))





      new_to_year = rep(NA, ncol(Species_in_collection_each_year)-1)
      new_to_year_Species = rep(NA, ncol(Species_in_collection_each_year)-1)
      for(i in 2:ncol(Species_in_collection_each_year)){
        new_in_year = Species_accessioned_each_year[,i]  &  !Species_in_collection_each_year[,i-1]
        new_to_year[i-1] = sum(new_in_year)
        new_to_year_Species[i-1] = paste0(all_species_without_author[new_in_year],collapse=', ')
      }


      wanted = data.frame(year = years[-1], new_to_year = new_to_year, new_Species = new_to_year_Species)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Genera' && turnover_type_of_chart == 'Number of New to Collection' && turnover_type == 'Gain'){
      #Set name to genera for title of the chart.
      output$turnover_type_data = 'Genera'

      #Get all the genera from the database.
      all_genera = unique(data_working$genus)

      #Get which genera are accessioned each year.
      genera_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        genera = unique(garden_current$genus)

        return(all_genera %in% genera)
      }))
      rownames(genera_accessioned_each_year) = all_genera
      colnames(genera_accessioned_each_year) = years

      #Get the genera in the collection each year
      genera_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        genera = unique(garden_current$genus)

        return(all_genera %in% genera)
      }))

      new_to_year = rep(NA, ncol(genera_in_collection_each_year)-1)
      new_to_year_genera = rep(NA, ncol(genera_in_collection_each_year)-1)
      for(i in 2:ncol(genera_in_collection_each_year)){
        new_in_year = genera_accessioned_each_year[,i]  &  !genera_in_collection_each_year[,i-1]
        new_to_year[i-1] = sum(new_in_year)
        new_to_year_genera[i-1] = paste0(all_genera[new_in_year],collapse=', ')
      }


      wanted = data.frame(year = years[-1], new_to_year = new_to_year, new_genera = new_to_year_genera)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Families' && turnover_type_of_chart == 'Number of New to Collection' && turnover_type == 'Gain'){
      #Set name to families for title of the chart.
      output$turnover_type_data = 'Families'

      #Get all the families from the database.
      all_families = unique(data_working$family)

      #Get which families are accessioned each year.
      families_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        family = unique(garden_current$family)

        return(all_families %in% family)
      }))
      rownames(families_accessioned_each_year) = all_families
      colnames(families_accessioned_each_year) = years

      #Get the families in the collection each year
      families_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        family = unique(garden_current$family)

        return(all_families %in% family)
      }))





      new_to_year = rep(NA, ncol(families_in_collection_each_year)-1)
      new_to_year_families = rep(NA, ncol(families_in_collection_each_year)-1)
      for(i in 2:ncol(families_in_collection_each_year)){
        new_in_year = families_accessioned_each_year[,i]  &  !families_in_collection_each_year[,i-1]
        new_to_year[i-1] = sum(new_in_year)
        new_to_year_families[i-1] = paste0(all_families[new_in_year],collapse=', ')
      }


      wanted = data.frame(year = years[-1], new_to_year = new_to_year, new_families = new_to_year_families)

      output$turnover_wanted_data = wanted
    }

    # Create the New 'quantity' lost to the collection each year (In collection previous year, died/lost in shown year)
    if(turnover_quantity == 'Taxa' && turnover_type_of_chart == 'Number of Lost to Collection' && turnover_type == 'Loss'){
      #Set name to Taxa for title of the chart.
      output$turnover_type_data = 'Taxa'

      #Get all the Taxa from the database.
      all_Taxa = unique(data_working$good_name)
      all_Taxa_without_author = data_working$sanitised_taxon[match(all_Taxa, data_working$good_name)]
      #Get which Taxa are accessioned each year.
      Taxa_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        Taxa = unique(garden_current$good_name)

        return(all_Taxa %in% Taxa)
      }))
      rownames(Taxa_accessioned_each_year) = all_Taxa
      colnames(Taxa_accessioned_each_year) = years

      #Get the Taxa in the collection each year
      Taxa_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        Taxa = unique(garden_current$good_name)

        return(all_Taxa %in% Taxa)
      }))

      loss_to_year = rep(NA, ncol(Taxa_in_collection_each_year)-1)
      loss_to_year_Taxa = rep(NA, ncol(Taxa_in_collection_each_year)-1)
      for(i in 1:(ncol(Taxa_in_collection_each_year)-1)){
        # To decide if death of group has occurred in year y we:
        #     group has death in y & group not in collection in year (y+1)
        loss_in_year = Taxa_accessioned_each_year[,i]  &  !Taxa_in_collection_each_year[,i+1]
        loss_to_year[i-1] = sum(loss_in_year)
        loss_to_year_Taxa[i-1] = paste0(all_Taxa_without_author[loss_in_year],collapse=', ')
      }

      wanted = data.frame(year = years[-length(years)], loss_to_year = loss_to_year, loss_Taxa = loss_to_year_Taxa)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Species' && turnover_type_of_chart == 'Number of Lost to Collection' && turnover_type == 'Loss'){
      #Set name to Species for title of the chart.
      output$turnover_type_data = 'Species'
      # restrict to only species.
      indices = grepl('1',data_working$infrageneric_level)
      data_working = data_working[indices,]
      data_plant_existing = data_plant_existing[indices,]
      inYear = inYear[indices]

      #Get all the Species from the database.
      all_Species = unique(data_working$good_name)
      all_species_without_author = data_working$sanitised_taxon[match(all_Species, data_working$good_name)]
      #Get which Species are accessioned each year.
      Species_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        Species = unique(garden_current$good_name)

        return(all_Species %in% Species)
      }))
      rownames(Species_accessioned_each_year) = all_Species
      colnames(Species_accessioned_each_year) = years

      #Get the Species in the collection each year
      Species_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        Species = unique(garden_current$good_name)

        return(all_Species %in% Species)
      }))

      loss_to_year = rep(NA, ncol(Species_in_collection_each_year)-1)
      loss_to_year_Species = rep(NA, ncol(Species_in_collection_each_year)-1)
      for(i in 1:(ncol(Species_in_collection_each_year)-1)){
        # To decide if death of group has occurred in year y we:
        #     group has death in y & group not in collection in year (y+1)
        loss_in_year = Species_accessioned_each_year[,i]  &  !Species_in_collection_each_year[,i+1]
        loss_to_year[i] = sum(loss_in_year)
        loss_to_year_Species[i] = paste0(all_species_without_author[loss_in_year],collapse=', ')
      }

      wanted = data.frame(year = years[-length(years)], loss_to_year = loss_to_year, loss_Species = loss_to_year_Species)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Genera' && turnover_type_of_chart == 'Number of Lost to Collection' && turnover_type == 'Loss'){
      #Set name to genera for title of the chart.
      output$turnover_type_data = 'Genera'

      #Get all the genera from the database.
      all_genera = unique(data_working$genus)

      #Get which genera are accessioned each year.
      genera_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        genera = unique(garden_current$genus)

        return(all_genera %in% genera)
      }))
      rownames(genera_accessioned_each_year) = all_genera
      colnames(genera_accessioned_each_year) = years

      #Get the genera in the collection each year
      genera_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        genera = unique(garden_current$genus)

        return(all_genera %in% genera)
      }))

      loss_to_year = rep(NA, ncol(genera_in_collection_each_year)-1)
      loss_to_year_genera = rep(NA, ncol(genera_in_collection_each_year)-1)
      for(i in 1:(ncol(genera_in_collection_each_year)-1)){
        # To decide if death of group has occurred in year y we:
        #     group has death in y & group not in collection in year (y+1)
        loss_in_year = genera_accessioned_each_year[,i]  &  !genera_in_collection_each_year[,i+1]
        loss_to_year[i] = sum(loss_in_year)
        loss_to_year_genera[i] = paste0(all_genera[loss_in_year],collapse=', ')
      }

      wanted = data.frame(year = years[-length(years)], loss_to_year = loss_to_year, loss_genera = loss_to_year_genera)

      output$turnover_wanted_data = wanted
    }
    if(turnover_quantity == 'Families' && turnover_type_of_chart == 'Number of Lost to Collection' && turnover_type == 'Loss'){
      #Set name to families for title of the chart.
      output$turnover_type_data = 'Families'

      #Get all the families from the database.
      all_families = unique(data_working$family)

      #Get which families are accessioned each year.
      families_accessioned_each_year = data.frame(lapply(years, function(year){
        # Accessions for the year
        garden_current = data_working[which(inYear == year),]

        # Number of objects.
        family = unique(garden_current$family)

        return(all_families %in% family)
      }))
      rownames(families_accessioned_each_year) = all_families
      colnames(families_accessioned_each_year) = years

      #Get the families in the collection each year
      families_in_collection_each_year = data.frame(lapply(data_plant_existing, function(x){
        garden_current = data_working[x,]

        # Number of objects.
        family = unique(garden_current$family)

        return(all_families %in% family)
      }))


      loss_to_year = rep(NA, ncol(families_in_collection_each_year)-1)
      loss_to_year_families = rep(NA, ncol(families_in_collection_each_year)-1)
      for(i in 1:(ncol(families_in_collection_each_year)-1)){
        # To decide if death of group has occurred in year y we:
        #     group has death in y & group not in collection in year (y+1)
        loss_in_year = families_accessioned_each_year[,i]  &  !families_in_collection_each_year[,i+1]
        loss_to_year[i] = sum(loss_in_year)
        loss_to_year_families[i] = paste0(all_families[loss_in_year],collapse=', ')
      }

      wanted = data.frame(year = years[-length(years)], loss_to_year = loss_to_year, loss_families = loss_to_year_families)

      output$turnover_wanted_data = wanted
    }
    return(output)
  }

}


ui <- fluidPage(#theme = shinytheme("united"),
  shinyjs::useShinyjs(),
  headerPanel('Change over time app'),
  sidebarPanel(HTML('<h4>Current Selection:</h4>'),
               uiOutput('summary'),
               hr(),

               tabsetPanel(type = "tabs",
                           tabPanel("Chart Options",
                                    HTML('Choose the year range to show for charts/graphs in <b>Whole Collection</b> and <b>Turnover</b>.'),
                                    fluidRow(
                                      column(
                                        width = 4, selectInput(inputId = 'single_value_min_year', label = HTML('Min Year:'), choices = NULL, selected = NULL, multiple = FALSE)),
                                      column(
                                        width = 4, selectInput(inputId = 'single_value_max_year', label = HTML('Max Year:'), choices = NULL, selected = NULL, multiple = FALSE)),
                                    ),
                                    hr(),
                                    HTML('Chose the colour scheme for charts with more than one line. <br> We use colours from the <a href="https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html">viridis package</a>.'),
                                    fluidRow(
                                      column(
                                        width = 4, selectInput(inputId = 'chart_colour', label = 'Colour Scheme:',choices = c('viridis', 'cividis', 'inferno', 'plasma', 'turbo', 'rocket', 'mako'), selected = 'viridis', multiple = FALSE)),
                                      column(
                                        width = 4,prettySwitch(inputId = "chart_colour_reverse", label = "Reverse Colours", fill = TRUE, status = "primary", inline=TRUE))
                                      ),
                                    prettySwitch(inputId = "add_annotate", label = "Add event annotations", fill = TRUE, status = "primary", inline=TRUE),
                                    shinyjs::hidden(actionButton(inputId = 'button',label = 'button'))
                           ),

                           tabPanel("Filters",
                                    selectInput(inputId = 'provenance', label = 'Provenance:',choices = NULL, selected = NULL, multiple = TRUE),
                                    selectInput(inputId = 'infra', label = 'Type of Taxa:',choices = NULL, selected = NULL, multiple = TRUE),
                                    selectInput(inputId = 'filter_family', label = 'Family:',choices = NULL, selected = NULL, multiple = TRUE),
                                    HTML('<h5>Geography Controls:</h5>'),
                                    fluidRow(
                                      column(
                                        width = 4, selectInput(inputId = 'region_levels', label = HTML('Level:'),choices = c('Level 1', 'Level 2', 'Level 3'), selected = 'Level 3', multiple = FALSE)),
                                      column(
                                        width = 8, selectInput(inputId = 'region', label = HTML('Region:'),choices = NULL, selected = NULL, multiple = TRUE))),
                                    fluidRow(
                                      column(
                                        width = 4, selectInput(inputId = 'Geography_Native_global', label = 'Location type:',choices = c('All', 'Naturally occurring only', 'Introduced only'), selected = 'Naturally occurring only', multiple = FALSE)),
                                      column(
                                        width = 4,prettySwitch(inputId = "extinct_switch_global", label = "Include Extinct", fill = TRUE, status = "primary", inline=TRUE),
                                        prettySwitch(inputId = "doubtful_locations_switch_global", label = "Include Doubtful", fill = TRUE, status = "primary", inline=TRUE))
                                    ),
                                    hr(),
                                    selectInput(inputId = 'threatened', label = 'Threatened:',choices = NULL, selected = NULL, multiple = FALSE),
                                    conditionalPanel(condition ="input.threatened == 'Threatened'",
                                                     selectInput(inputId = 'threatened_cat', label = 'Threatened Category:',choices = NULL, selected = NULL, multiple = TRUE)),
                                    selectInput(inputId = 'endemic', label = HTML('Endemic: <font color="#D3D3D3"> (Level 3, Native, not-extinct, not-doubtful) </font>'),choices = NULL, selected = NULL, multiple = FALSE),
                                    selectInput(inputId = 'enrichment_status', label = 'POWO Match Status:',choices = c('All Records', 'POWO Matched', 'No Match'), selected = 'All Records', multiple = FALSE),
                                    sliderInput(inputId = 'extant', label = 'Number of Collections Globally:', min = 0, max = 1, value = c(0,1), sep = ''),
                                    hr())
                           ),
  ),

  # Main panel for displaying outputs ----
  mainPanel(

    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(id = 'main_tabs', type = "tabs",
                tabPanel("Info", HTML('<h3>Welcome to the change over time app!</h3> This app provides a tool for examining how a living collection has varied over time.
                                      <br> The sidebar provides summary details of the selected data, controls global parameters that filter the data and default time window. To change the default time window or chart colours go to the <b>Chart Options</b> tab. Controls for filtering the data is found in the <b>Filters</b> tab. The Global Filters are: <br> <b>Provenance:</b> Filter the provenance of the items. <br> <b>Type of Taxa:</b> Filter the collection by the type of taxa. Note that taxa can be tagged as multiple types such as <i> Salix × fragilis f. vitellina </i> as both hybrid and forma. In this case, the taxa will be kept by either restricting to hybrids or forma. <br> <b>Geography controls:</b> This contains a selection of controls to filter the taxa by their distribution. The level allows a choice between Level 1 (Continents), Level 2 (larger regions) and Level 3 (smaller regions) using wgsrpd3 breakdown (<a href="https://www.tdwg.org/standards/wgsrpd/">see here</a>). Changing the level will automatically change the regions you can select. Using the region box you can filter the taxa by one or more areas. The location of a taxon can be described as naturally occurring or introduced, extinct or not and location doubtful or not. By default we select only naturally occuring locations that are neither extinct or doubtful. To change these settings you can update <b>location type</b> for naturally occurring/introduced and the switches to allow extinct or doubtful areas. <br> <b>Threatened:</b> Filter by threatened status of Taxa (using IUCN redlist). If "Threatened" is selected another filter will appear below where you can choose between threatened categories. <br> <b> Endemic:</b> Filter by endemic speices, where we define a species as endemic if its distriubtion is a single level 3 area which is neither extinct or doubtful.  <br> <b>POWO match status:</b> Filter by records which matched to Plants of the World Online (POWO) or not. <br> <b>Number of Global Collections: </b> Filter the collection by the number of collections a taxon is found in worldwide using BGCI records (Older version of plantsearch).  <br><br>
                                      The app also contains four tabs: <br>
                                      <b>Info:</b> Explains how to use the app. <br>
                                      <b>Data:</b> Shows the underlying data. <br>
                                      <b>Whole Collection:</b> This tab produces charts/graphs showing how the living collection has changed over time  <br>
                                      <b>Turnover:</b> This tab produces charts/graphs detailing the turnover of items and accessions in the collection  <br>
                                      <b>Options:</b> this tab allows users to add historic events which will be shown in the graphics.   <br>')),

                tabPanel("Data",
                         HTML('This tab is to display the <b>filtered</b> data as used in creating the charts in <b>Whole Collection</b>. By default all data is shown, from the date of the first accession to the present. This can be changed using the <b>Show</b> dropdown menu. If "Choose Date" is selected a date picker will appear and you can view what was living in the collection at a particular date. Moreover, if you choose "Data with Missing info" this will show all the data which has either missing accession or death date. <br><br> Note that: <br> - For the charts the items in the collection for each year is taken on the first of January. <br> - Since we only have the year of accession we assume all items are accessioned on the first of January. <br> - The death date corresponds to when the item is recorded as dead/removed/not found. If only a year is given we set the death date to the 31st of December of that year. If a year and month are given we set the day to the 28th.'),
                         hr(),
                         fluidRow(
                           column(
                             width = 6, selectInput(inputId = 'data_filter', label = HTML('Show:'), choices = c('All', 'Choose Date', 'Data with missing info'), selected = 'All', multiple = FALSE)),
                           column(

                             width = 6,
                             conditionalPanel(condition ="input.data_filter == 'Choose Date'",
                                              dateInput(inputId = 'data_select', label ='Choose Date:', value = NA)))
                         ),

                         DT::DTOutput(outputId = 'table')),

                tabPanel("Whole Collection",
                         HTML('This tab is to view how the collection has changed over time using different metrics and filters. <br> Below you can control the quantity ued to produce the plots in  <b>Breakdown By</b>. After chosing the quantity you can use <b> Chart</b> to choose which graphic to show. <br> Moreover you can filter the data using the sidebar as explained in the info tab and add historical events using the <b>Options</b> tab. '),
                         hr(),
                         fluidRow(
                           column(
                             width = 4, selectInput(inputId = 'single_value_value_type', label = HTML('Breakdown By:'), choices = c('Items', 'Accessions', 'Taxa', 'Species', 'Genera', 'Families'), selected = 'Accessions', multiple = FALSE)),
                           column(
                             width = 4, selectInput(inputId = 'single_value_chart', label = HTML('Chart:'), choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species'), selected = 'Number Over Time', multiple = FALSE))
                         ),
                         hr(),
                         uiOutput('change_time_single_value', width = '100%', height = '100%')),

                tabPanel("Turnover",
                         HTML('This tab is to view the turnover in the collection collection over time using different metrics and filters. <br> Below you can control the quantity to consider using <b>Breakdown By</b> select input. For each quanitity there are a selection of available charts to show. These can be selected using the <b>Chart</b> input. Moreover you can filter the data using the sidebar as explained in the info tab and add historical events using the <b>Options</b> tab. '),
                         hr(),
                         fluidRow(
                           column(
                             width = 4, selectInput(inputId = 'turnover_type', label = HTML('Turnover:'), choices = c('Gain', 'Loss', 'Net'), selected = 'Gain', multiple = FALSE)),
                           column(
                             width = 4, selectInput(inputId = 'turnover_quantity', label = HTML('Breakdown By:'), choices = c('Items', 'Accessions', 'Taxa', 'Species', 'Genera', 'Families'), selected = 'Accessions', multiple = FALSE)),
                           column(
                             width = 4, selectInput(inputId = 'turnover_type_of_chart', label = HTML('Chart:'), choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species'), selected = 'Number Over Time', multiple = FALSE))
                         ),
                         hr(),
                         uiOutput('change_time_turnover', width = '100%', height = '100%')),

                tabPanel("Options",
                         hr(),
                         HTML('<h3>Add events through time:</h3>'),
                         HTML('Current events: <br>'),
                         DT::DTOutput(outputId = 'eventTable'),
                         hr(),
                         HTML('To add events select the start and end (optional) date and describe the event, click "add" to include the event.'),
                         fluidRow(
                           column(
                             width = 1, colourpicker::colourInput(inputId = 'event_colour', label ='Colour:', value = '#E5E5E5', showColour = "background", palette = "limited")),
                           column(
                             width = 2, dateInput(inputId = 'event_start', label ='Start:')),
                           column(
                             width = 2, dateInput(inputId = 'event_end', label ='End:', value = NA)),
                           column(
                             width = 4, textInput(inputId = 'event_detail', label ='Event:')),
                           column(
                             width = 1, actionButton(inputId = 'event_add', label ='Add', style = 'margin-top:25px'))
                           ),
                         HTML('Or you can upload a ".csv" file containing the event information. This must have four columns named: colour, start, end and event in that order. The start and end must be dates in the format yyyy-mm-dd. The colours must be a HEX code for a colour.'),
                         fileInput(inputId = "upload_event_details", label = "Upload event details", placeholder = 'must be .csv file', accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                         hr(),

                         HTML('<br> To delete events, click them on the table above and click the delete button.<br>'),
                         actionButton(inputId = 'event_delete', label ='Delete'),
                         hr(),
                         HTML('<br> To include default events click the button below. <br> This includes: WW1, WW2 and covid outbreak.<br>'),
                         actionButton(inputId = 'event_add_default', label ='Add default'),
                         hr()

                         )
)

  )
)


server <- function(input, output, session){
  #---- User inputted events in time -------------------------------
  # Set up the data frame for storing user inputted events in time.
  df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("colour","start", "end", "event"))

  event_values <- reactiveValues(dfWorking = df)

  # Observe event (action button) for removing rows from user inputted events in time.
  observeEvent(input$event_delete,{
    print('In add delete')
    print(input$event_start)
    print(input$event_end)
    print(input$event_detail)
    if (!is.null(input$eventTable_rows_selected)) {

      event_values$dfWorking <- event_values$dfWorking[-as.numeric(input$eventTable_rows_selected),]
    }
  })

  # Observe event (action button) for adding a new row from user inputted events in time.
  observeEvent(input$event_add,{

    print('In add event')
    print(input$event_start)
    print(input$event_end)
    print(input$event_detail)

    if(!is.null(input$event_start)){
        if(length(input$event_end) == 0){
          end = input$event_start
        }else{
          end = input$event_end
        }
        print(paste0(as.character(input$event_colour),as.character(input$event_start),as.character(end),input$event_detail,collapse ='---'))
        new_row = c(as.character(input$event_colour), as.character(input$event_start), as.character(end), input$event_detail)

        df = event_values$dfWorking
        df[nrow(df)+1,] = new_row
        event_values$dfWorking = df

        updateDateInput(session, inputId = 'event_start', value = Sys.Date())
        updateDateInput(session, inputId = 'event_end', value = NA)
        updateTextInput(session, inputId = 'event_detail', value = NULL)

    }else{
      text = 'Require start date!'
      print(text)

    }
  })

  # Observe event (action button) for adding a new row from user inputted events in time.
  observeEvent(input$event_add_default,{

    print('In event_add_default')
    #default events.
    default_events = data.frame(matrix(c('#E5E5E5','1914-07-28', '1918-11-11', 'World War One',
                                         '#E5E5E5','1939-09-01', '1945-09-02', 'World War Two',
                                         '#E5E5E5','2020-01-31', '2022-02-15', 'Covid Pandemic',
                                         '#E5E5E5','2014-10-12', '2014-10-12', '<a href=" https://en.wikipedia.org/wiki/Nagoya_Protocol">Nagoya <br>Protocol</a>'
                                         ), nrow = 4,ncol = 4, byrow = TRUE))

    # Get the current events
    df = event_values$dfWorking

    # Find if any default events are already in event table.
    default_events = default_events[which(!default_events[,3] %in% df[,3]),]
    print(default_events)
    #Add default events if not already in event data frame.
    if(nrow(default_events) >0){
      df[(nrow(df)+1):(nrow(df)+nrow(default_events)),] = default_events

    }

    event_values$dfWorking = df

  })

  # Get the data uploaded
  get_event_details <- observeEvent(input$upload_event_details,{
    print('in upload event data')
    upload_event_data = NULL
    if (is.null(input$upload_event_details)) {
      upload_event_data = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("colour","start", "end", "event"))
    } else {
      upload_event_data = read.csv(input$upload_event_details$datapath)
    }
    print(head(upload_event_data))
    if(nrow(upload_event_data) > 0 & ncol(upload_event_data) == 4){
      df = event_values$dfWorking
      df[(nrow(df)+1):(nrow(df)+nrow(upload_event_data)),] = upload_event_data
      event_values$dfWorking = df
    }
  })

  # Display the current user inputted events in time.
  output$eventTable <- renderDT({
    print('Render event table')
    data_to_show = event_values$dfWorking
    if(nrow(data_to_show) == 0){
      datatable(setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("colour","start", "end", "event")), rownames = FALSE, escape = FALSE,
                options = list(
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '40px', targets = c('colour')),
                                    list(width = '80px', targets = c('start','end')),
                                    list(width = '500px', targets = c('event')))
                )
      )
    }else{
      rownames(data_to_show) = NULL
      datatable(data_to_show, rownames = FALSE, escape = FALSE,
                options = list(
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '40px', targets = c('colour')),
                                    list(width = '80px', targets = c('start','end')),
                                    list(width = '500px', targets = c('event')))
                )
      ) %>%
        formatStyle("colour", backgroundColor = styleEqual(levels = data_to_show$colour,values = data_to_show$colour))
    }


  })
  #---- (END) User inputted events in time -------------------------------

  #---- Load and setup the enriched report ---------------------
  # Load the enriched report.
  load('CUBG_enriched_report.rda')

  # Load the geometries for the different wgsrpd3 levels.
  load('wgsrpd3_level3_simp.rda')
  load('wgsrpd3_level2_simp.rda')
  load('wgsrpd3_level1_simp.rda')

  # Load code matching between level 1/2 to level 3.
  load('level1_and_2_wgsrpd3_namecodes_matched_to_level3.RData')

  regions_names = wgsrpd3_level3_simp$name
  areas_names = wgsrpd3_level2_simp$name
  continents_names = wgsrpd3_level1_codes$name

  # load all geo areas
  # wgsrpd3 = wgsrpd3_level3_simp

  # Convert geometries (level1,2,3) into a format accepted by plotly
  # mapp = sf::st_cast(wgsrpd3_level3_simp, "MULTIPOLYGON")
  # geo_sf = geojsonsf::sf_geojson(mapp)
  # data = rjson::fromJSON(geo_sf)
  # feat = data$features
  # for(i in 1:length(feat)){
  #   feat[[i]]$id = feat[[i]]$properties$code
  # }
  # data$features = feat
  # data_level3 = data
  #
  # mapp = sf::st_cast(wgsrpd3_level2_simp, "MULTIPOLYGON")
  # geo_sf = geojsonsf::sf_geojson(mapp)
  # data = rjson::fromJSON(geo_sf)
  # feat = data$features
  # for(i in 1:length(feat)){
  #   feat[[i]]$id = feat[[i]]$properties$code
  # }
  # data$features = feat
  # data_level2 = data
  #
  # mapp = sf::st_cast(wgsrpd3_level1_simp, "MULTIPOLYGON")
  # geo_sf = geojsonsf::sf_geojson(mapp)
  # data = rjson::fromJSON(geo_sf)
  # feat = data$features
  # for(i in 1:length(feat)){
  #   feat[[i]]$id = feat[[i]]$properties$code
  # }
  # data$features = feat
  # data_level1 = data


  report = enriched_report

  # Set those not in BGCI (i.e no_gardens = NA) to 0 gardens
  report$no_gardens[is.na(report$no_gardens)] = 0

  #Add parts of the enriched report.
  # Endemic or not endemic
  report$endemic = rep('Not Endemic', nrow(report))
  endemic_index = which(stringr::str_length(report$POWO_Dist_000_area_code_l3) ==3)
  report$endemic[endemic_index] = 'Endemic'

  # Go to full provenance code.
  report$ProvenanceCode[report$ProvenanceCode == 'G'] = 'Garden'
  report$ProvenanceCode[report$ProvenanceCode == 'U'] = 'Unknown'
  report$ProvenanceCode[report$ProvenanceCode == 'W'] = 'Wild'
  report$ProvenanceCode[report$ProvenanceCode == 'Z'] = 'Wild-Derived'

  # Cleaned name with author.
  report$good_name = paste0(report$sanitised_taxon, ' ', report$extracted_author)

  # Threatened or not threatened.
  report$threatened = rep('Not Threatened', nrow(report))
  threat_index = which(report$redList_category %in% c('VU', 'EN', 'CR', 'EW', 'EX'))
  report$threatened[threat_index] = 'Threatened'

  # Best Genus.
  genus = report$POWO_genus
  genus[is.na(genus)] = report$Genus[is.na(genus)]
  report$genus = genus

  #Best Family.
  family = report$POWO_family
  family[is.na(family)] = report$Family[is.na(family)]
  family[is.na(family)] = 'NA'
  report$family = family

  # Species according to POWO.
  report$species = stringr::str_to_title(report$POWO_species)

  # Enrichment status of an item
  enrichment_status = rep('',nrow(report))
  enrichment_status[!is.na(report$POWO_plant_name_id)] = paste0(enrichment_status[!is.na(report$POWO_plant_name_id)], ' POWO')
  enrichment_status[!is.na(report$no_gardens)] = paste0(enrichment_status[!is.na(report$no_gardens)], ' BGCI')
  enrichment_status[!is.na(report$redList_match_taxon_name)] = paste0(enrichment_status[!is.na(report$redList_match_taxon_name)], ' IUCN')
  report$enrichment_status = enrichment_status

  # Item death date.
  death = rep(NULL, nrow(report))
  death[report$ItemStatusType == "NotExisting"] = report$ItemStatusDate[report$ItemStatusType == "NotExisting"]
  report$DeathDate = death

  report_care = report[,match(c('ItemAccNoFull', 'good_name', 'AccYear', 'DeathDate', 'POWO_web_address', 'ProvenanceCode', 'endemic', 'threatened', 'redList_category', 'no_gardens',
                                'POWO_Dist_000_area_code_l3',
                                "POWO_Dist_010_area_code_l3",
                                "POWO_Dist_001_area_code_l3",
                                "POWO_Dist_011_area_code_l3",
                                "POWO_Dist_100_area_code_l3",
                                "POWO_Dist_110_area_code_l3",
                                "POWO_Dist_101_area_code_l3",
                                'infrageneric_level', 'genus', 'family', 'species', 'enrichment_status',
                                'ItemStatusDate', 'ItemStatusType', 'GenusSpecies', 'POWO_taxon_name', 'POWO_taxon_authors','POWO_genus','POWO_species', 'sanitised_taxon'), names(report))]

  powo_link = rep(NA, nrow(report_care))
  powo_link[!is.na(report_care$POWO_web_address)] = createLink(report_care$POWO_web_address[!is.na(report_care$POWO_web_address)])
  powo_link[is.na(report_care$POWO_web_address)] = 'Not Matched to POWO'
  report_care$POWO_web_address <- powo_link


  # Update the UI for the inputted data (year and option)
  updateSliderInput(session, inputId = 'extant', min = min(report$no_gardens,na.rm = T), max = max(report$no_gardens,na.rm = T), step=1, value =c(min(report$no_gardens,na.rm = T), max(report$no_gardens,na.rm = T)))
  updateSelectInput(session, inputId = 'provenance', choices = unique(report$ProvenanceCode), selected = unique(report$ProvenanceCode))
  updateSelectInput(session, inputId = 'infra', choices = c('Indeterminate', 'Species', 'Subspecies', 'Variety', 'Forma', 'Cultivar', 'Hybrid'), selected = NULL)
  updateSelectInput(session, inputId = 'threatened', choices = c('All',unique(report$threatened)), selected = 'All')
  all_families = unique(report_care$family)
  all_families = sort(all_families[!is.na(all_families)])
  updateSelectInput(session, inputId = 'filter_family', choices = all_families, selected = NULL)


  ordered_redlist_options_short = c('VU', 'EN', 'CR', 'EW', 'EX')
  ordered_redlist_options = c('Vulnerable', 'Endangered', 'Critically Endangered', 'Extinct in the Wild', 'Extinct')
  toShow = ordered_redlist_options[which(ordered_redlist_options_short %in% report$redList_category)]
  updateSelectInput(session, inputId = 'threatened_cat', choices = toShow, selected = NULL)
  updateSelectInput(session, inputId = 'endemic', choices = c('All',unique(report$endemic)), selected = 'All')
  updateSelectInput(session, inputId = 'region', choices = regions_names, selected = NULL)


  values <- reactiveValues(data = data.frame(1), wanted_data = NA, type_data = NA,
                           turnover_wanted_data = NA, turnover_type_data = NA, turnover_type = NA,
                           input_turnover_type_of_chart = 'Number Over Time', input_turnover_quantity = 'Accessions', input_turnover_type = 'Gain')

  min_year = min(enriched_report$AccYear[enriched_report$AccYear>1750],na.rm = T) + 1
  year_cur = as.numeric(format(Sys.Date(),'%Y'))

  years = min_year:year_cur
  updateSelectInput(session, inputId = 'single_value_min_year', choices = min_year:year_cur, selected = min_year)
  updateSelectInput(session, inputId = 'single_value_max_year', choices = min_year:year_cur, selected = year_cur)
  updateSelectInput(session, inputId = 'single_value_value_type', selected = 'Items')

  #---- (END) Load and setup the enriched report -------------------------------

  #---- Filtering the data ----------------------
  # On change of global settings update the current data (the filtered data)
  observeEvent(input$region_levels, {
    print('------------------')
    print('in change region codes')
    if(input$region_levels == 'Level 1'){
      print('Change to Level 1 (continents)')
      updateSelectInput(session, inputId = 'region', choices = continents_names, selected = NULL)

    }
    if(input$region_levels == 'Level 2'){
      print('Change to Level 1 (Areas)')
      updateSelectInput(session, inputId = 'region', choices = areas_names, selected = NULL)

    }
    if(input$region_levels == 'Level 3'){
      print('Change to Level 3 (Regions)')
      updateSelectInput(session, inputId = 'region', choices = regions_names, selected = NULL)

    }

  })

  # On change of global settings update the current data (the filtered data)
  observeEvent(c(input$extant,
                 input$provenance,
                 input$infra,
                 input$threatened,
                 input$endemic,
                 input$threatened_cat,
                 input$region,
                 input$Geography_Native_global,
                 input$extinct_switch_global,
                 input$doubtful_locations_switch_global,
                 input$enrichment_status,
                 input$filter_family), {
                   print('------------------')
                   print('in update filters')
                   # print(input)
                   # print(input$provenance)
                   # print(input$infra)
                   # print(input$threatened)
                   # print(input$endemic)
                   # print(input$threatened_cat)
                   # print(input$region)
                   # print(input$Geography_Native_global)
                   # print(input$extinct_switch_global)
                   # print(input$doubtful_locations_switch_global)
                   # print(input$enrichment_status)
                   print('------------------')
                   if(!is.null(input$extant != 0)){
                     current = report_care[which(report_care$ProvenanceCode %in% input$provenance),]
                     current = current[which(current$no_gardens >= input$extant[1] & current$no_gardens <= input$extant[2]),]
                     if(!is.null(input$infra)){
                       selected = c(0:6)[match(input$infra, c('Indeterminant', 'Species', 'Subspecies', 'Variety', 'Forma', 'Cultivar', 'Hybrid'))]
                       pattern = paste0(selected,collapse='|')
                       current = current[which(grepl(pattern,current$infrageneric_level)),]
                     }
                     if(input$endemic != 'All'){
                       current = current[which(current$endemic %in% input$endemic),]
                     }
                     if(input$threatened != 'All'){
                       current = current[which(current$threatened %in% input$threatened),]
                     }
                     if(!is.null(input$threatened_cat)){
                       selected_threat_cat = ordered_redlist_options_short[match(input$threatened_cat, ordered_redlist_options)]
                       current = current[which(current$redList_category %in% selected_threat_cat),]
                     }
                     if(!is.null(input$region)){
                       if(input$region_levels == 'Level 3'){
                         region_code = wgsrpd3_level3_simp$code[match(input$region,wgsrpd3_level3_simp$name)]
                       }
                       if(input$region_levels == 'Level 2'){
                         region_code = paste0(wgsrpd3_level2_codes$level3codes[match(input$region,wgsrpd3_level2_codes$name)],collapse =', ')
                         region_code = unlist(stringr::str_split(region_code, ', '))
                       }
                       if(input$region_levels == 'Level 1'){
                         region_code = paste0(wgsrpd3_level1_codes$level3codes[match(input$region,wgsrpd3_level1_codes$name)],collapse =', ')
                         region_code = unlist(stringr::str_split(region_code, ', '))
                       }



                       want = c('000','010','001','011','100','110','101')
                       if(input$Geography_Native_global == 'Introduced only'){
                         want = want[grepl('^1',want)]
                       }else if(input$Geography_Native_global == 'Naturally occurring only'){
                         want = want[grepl('^0',want)]
                       }

                       ## B) Extinct
                       if(!input$extinct_switch_global){
                         want = want[!grepl('010|011|110',want)]
                       }
                       ## C) Doubtful
                       if(!input$doubtful_locations_switch_global){
                         want = want[!grepl('001|011|101',want)]
                       }
                       # print(want)
                       level3_wanted = current[,grepl(paste0(want,collapse='|'), names(current))]
                       if(length(want) > 1){
                         level3codes =do.call("paste", c(level3_wanted, sep = ", "))
                       }else{
                         level3codes = level3_wanted
                       }
                       indices = which(grepl(paste0(region_code, collapse='|'),level3codes))

                       current = current[indices,]
                     }
                     if(input$enrichment_status != 'All Records'){
                       if(input$enrichment_status == 'POWO Matched'){
                         current = current[which(grepl('POWO',current$enrichment_status)),]
                       }else{
                         current = current[which(!grepl('POWO',current$enrichment_status)),]
                       }
                     }
                     if(!is.null(input$filter_family)){
                       print('boyah')
                       current = current[which(current$family %in% input$filter_family),]
                     }

                     # print(head(current))
                     values$data = current


                     years = min_year:year_cur
                     date = paste0(years, '-01-01')
                     plant_existing = exist_at_date(date, acc = current$AccYear,
                                                              ItemStatusDate = current$ItemStatusDate,
                                                              ItemStatusType = current$ItemStatusType)
                     values$plant_existing = plant_existing

                   }
                   print('complete')
                   print('------------------')

                 })
  #---- (END) Filtering the data -------------------------------

  #---- User conditional select values -------------------------------

  observeEvent(input$single_value_min_year,{
    print('In change min year')
    print(input$single_value_min_year)
    print(input$single_value_max_year)
    if(input$single_value_max_year != '' & input$single_value_max_year != ''){
      updateSelectInput(session, inputId = 'single_value_max_year', choices = (as.numeric(input$single_value_min_year)+1):as.numeric(year_cur), selected = as.numeric(input$single_value_max_year))
    }

    })

  observeEvent(input$single_value_max_year,{
    print('In change max year')
    if(input$single_value_max_year != '' & input$single_value_max_year != ''){
      updateSelectInput(session, inputId = 'single_value_min_year', choices = as.numeric(min_year):(as.numeric(input$single_value_max_year)+1), selected = as.numeric(input$single_value_min_year))
    }
  })

  observeEvent(input$single_value_value_type,{
    print('In change to single_value_type')
    print(input$single_value_value_type)
    if(input$single_value_value_type %in% c('Items', 'Accessions')){
      updateSelectInput(session, inputId = 'single_value_chart', choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species'), selected = 'Number Over Time')
    }else if(input$single_value_value_type %in% c('Species', 'Genera', 'Families')){
      updateSelectInput(session, inputId = 'single_value_chart', choices = c('Number Over Time'), selected = 'Number Over Time')
    }
    else if(input$single_value_value_type %in% c('Taxa')){
      updateSelectInput(session, inputId = 'single_value_chart', choices = c('Number Over Time', 'Divided into Type of Taxa'), selected = 'Number Over Time')
    }
  })

  observeEvent(c(input$turnover_type,
                 input$turnover_quantity),{

    print('Update turnover inputs')
    inputted_values = list(turnover_type = input$turnover_type,
                           turnover_quantity = input$turnover_quantity,
                           turnover_type_of_chart = input$turnover_type_of_chart)
    # Change to turnover type
    if(inputted_values$turnover_type != values$input_turnover_type){
      print('In change to turnover_type')
      print(input$turnover_type)
      if(input$turnover_type == 'Gain'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          click('button')

          # Nothing required
        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families')){
          if(input$turnover_type_of_chart %in% c('Number Lost Over Time', 'Number of Lost to Collection')){
            choices = c('Number Accessioned Over Time', 'Number of New to Collection')
            selected = choices[match(input$turnover_type_of_chart, c('Number Lost Over Time', 'Number of Lost to Collection'))]
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = selected)
          }
          if(input$turnover_type_of_chart %in% c('Number of Net to Collection')){
            choices = c('Number Accessioned Over Time', 'Number of New to Collection')
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number of New to Collection')
          }
        }
        if(input$turnover_quantity %in% c('Taxa')){
          if(input$turnover_type_of_chart %in% c('Number Lost Over Time', 'Number of Lost to Collection', 'Divided into Type of Taxa')){
            choices = c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa')
            selected = choices[match(input$turnover_type_of_chart, c('Number Lost Over Time', 'Number of Lost to Collection', 'Divided into Type of Taxa'))]
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = selected)
          }
          if(input$turnover_type_of_chart %in% c('Number of Net to Collection')){
            choices = c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa')
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number of New to Collection')
          }
        }

      }
      if(input$turnover_type == 'Loss'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          click('button')

          #Nothing required
        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families')){
          if(input$turnover_type_of_chart %in% c('Number Accessioned Over Time', 'Number of New to Collection')){
            choices = c('Number Lost Over Time', 'Number of Lost to Collection')
            selected = choices[match(input$turnover_type_of_chart, c('Number Accessioned Over Time', 'Number of New to Collection'))]
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = selected)
          }
          if(input$turnover_type_of_chart %in% c('Number of Net to Collection')){
            choices = c('Number Lost Over Time', 'Number of Lost to Collection')
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number of Lost to Collection')
          }
        }
        if(input$turnover_quantity %in% c('Taxa')){
          if(input$turnover_type_of_chart %in% c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa')){
            choices = c('Number Lost Over Time', 'Number of Lost to Collection', 'Divided into Type of Taxa')
            selected = choices[match(input$turnover_type_of_chart, c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa'))]
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = selected)
          }
          if(input$turnover_type_of_chart %in% c('Number of Net to Collection')){
            choices = c('Number Lost Over Time', 'Number of Lost to Collection', 'Divided into Type of Taxa')
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number of Lost to Collection')
          }
        }

      }
      if(input$turnover_type == 'Net'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          click('button')

          # Nothing required
        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families', 'Taxa')){
          updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = 'Number of Net to Collection', selected = 'Number of Net to Collection')
        }
      }
    }
    if(inputted_values$turnover_quantity != values$input_turnover_quantity){
      print('In change to turnover_quantity')
      print(input$turnover_quantity)
      if(input$turnover_type == 'Gain'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species')
          if(input$turnover_type_of_chart %in% choices){
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = input$turnover_type_of_chart)
            click('button')

            # Nothing required
          }else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number Over Time')
          }

        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families')){
          if(input$turnover_type_of_chart %in% c('Number Accessioned Over Time', 'Number of New to Collection')){
            click('button')

            # Nothing required
          }else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = c('Number Accessioned Over Time', 'Number of New to Collection'), selected = 'Number Accessioned Over Time')
          }
        }
        if(input$turnover_quantity %in% c('Taxa')){
          choices = c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa')
          if(input$turnover_type_of_chart %in% choices){
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices,selected = input$turnover_type_of_chart)
            click('button')

            # Nothing required
          } else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = c('Number Accessioned Over Time', 'Number of New to Collection', 'Divided into Type of Taxa'), selected = 'Number Accessioned Over Time')
          }

        }
      }
      if(input$turnover_type == 'Loss'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species')
          if(input$turnover_type_of_chart %in% choices){
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices,selected = input$turnover_type_of_chart)
            click('button')
            # Nothing required
          } else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number Over Time')
          }

        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families')){
          choices = c('Number Lost Over Time', 'Number of Lost to Collection')
          if(input$turnover_type_of_chart %in% choices){
            click('button')

            # Nothing required
          } else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number Lost Over Time')
          }

        }
        if(input$turnover_quantity %in% c('Taxa')){
          choices = c('Number Lost Over Time', 'Number of Lost to Collection', 'Divided into Type of Taxa')
          if(input$turnover_type_of_chart %in% choices){
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = input$turnover_type_of_chart)
            click('button')

            # Nothing required
          } else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number Lost Over Time')        }


        }
      }
      if(input$turnover_type == 'Net'){
        if(input$turnover_quantity %in% c('Items', 'Accessions')){
          choices = c('Number Over Time', 'Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species')
          if(input$turnover_type_of_chart %in% choices){
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = input$turnover_type_of_chart)
            click('button')

            # Nothing required
          }else{
            updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = choices, selected = 'Number Over Time')
          }

        }
        if(input$turnover_quantity %in% c('Species', 'Genera', 'Families', 'Taxa')){
          updateSelectInput(session, inputId = 'turnover_type_of_chart', choices = 'Number of Net to Collection', selected = 'Number of Net to Collection')
        }
      }
    }

    #Update charts.
    # report_cur = values$data
    # if(nrow(report_cur)!=0){
    # print('clicked button')
    # click('button')
    # }

    values$input_turnover_type_of_chart = input$turnover_type_of_chart
    values$input_turnover_type = input$turnover_type
    values$input_turnover_quantity = input$turnover_quantity
    })

  observeEvent(input$turnover_type_of_chart,{
    print('Change turnover_type_of_chart')
    print(input$turnover_type_of_chart)
    print('-----------')

    click('button')
  })

  observeEvent(input$main_tabs,{
    print('Change tab')
    if(input$main_tabs %in% c('Whole Collection', 'Turnover'))
    click('button')
    # if(is.na(values$turnover_wanted_data[1])){
    #   if(input$main_tabs == 'Turnover'){
    #     print('Click Button')
    #
    #   }
    # }
  })
  #---- (END) User conditional select values -------------------------------

  #---- User graph/chart options  --------------------------
  observeEvent(input$chart_colour, {
    print('change colour scheme')
    colourScheme <- function(n, alpha = 1, direction = 1){
      if(input$chart_colour == 'viridis'){
        return(viridis::viridis(n, alpha = alpha, direction = direction))
      } else if(input$chart_colour == 'inferno'){
        return(viridis::inferno(n, alpha = alpha, direction = direction))
      }  else if(input$chart_colour == 'mako'){
        return(viridis::mako(n, alpha = alpha, direction = direction))
      } else if(input$chart_colour == 'plasma'){
        return(viridis::plasma(n, alpha = alpha, direction = direction))
      }else if(input$chart_colour == 'turbo'){
        return(viridis::turbo(n, alpha = alpha, direction = direction))
      }else if(input$chart_colour == 'cividis'){
        return(viridis::cividis(n, alpha = alpha, direction = direction))
      }else if(input$chart_colour == 'rocket'){
        return(viridis::rocket(n, alpha = alpha, direction = direction))
      }
    }
    colourScheme <<- colourScheme
  })
  #---- (END) User graph/chart options -------------------------------

  #---- Get information for plots.  --------------------------
  observeEvent(c(input$extant,
                 input$provenance,
                 input$infra,
                 input$threatened,
                 input$endemic,
                 input$threatened_cat,
                 input$region,
                 input$Geography_Native_global,
                 input$extinct_switch_global,
                 input$doubtful_locations_switch_global,
                 input$enrichment_status,
                 input$filter_family,
                 input$main_tabs,
                 input$single_value_value_type,
                 input$single_value_chart,
                 input$button),{

    print('In create data for charts')
       if(nrow(values$data)==0){
      print('no data!')

    }else{
      print('we have data')
      print(paste0('Charts for ', input$main_tabs))

      # Get the data needed for the main collection tab.
      if(input$main_tabs == 'Whole Collection'){

        # Create the data for the single value quantities
        if(input$single_value_value_type == 'Items' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of items
            no_items = nrow(garden_current)
            return(list(number = no_items))
          })
          type_data = 'Items'
        }
        if(input$single_value_value_type == 'Accessions' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of Accessions
            accessions = unlist(lapply(stringr::str_split(garden_current$ItemAccNoFull,pattern = '\\*'),function(x){x[1]} ))
            no_accessions = length(unique(accessions))
            return(list(number = no_accessions))
          })
          type_data = 'Accessions'
        }
        if(input$single_value_value_type == 'Taxa' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of Taxa
            taxa = data.frame(sanitsed_name = garden_current$good_name, powo_name = paste0(garden_current$POWO_taxon_name, ' ', garden_current$POWO_taxon_authors))
            taxa$powo_name[taxa$powo_name == 'NA NA'] = NA
            use_taxa = taxa$powo_name
            use_taxa[is.na(use_taxa)] = taxa$sanitsed_name[is.na(use_taxa)]
            no_taxa <- length(unique(use_taxa))
            return(list(number = no_taxa))
          })
          type_data = 'Taxa'
        }
        if(input$single_value_value_type == 'Species' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of Species
            #No species (#remove cultivars, indeterminants, hybrids and those without infrageneric level)
            POWO_GenusSpecies = rep(NA, nrow(garden_current))
            has_POWO = grepl('POWO',garden_current$enrichment_status)
            POWO_GenusSpecies[has_POWO] = paste0(garden_current$POWO_genus[has_POWO], ' ', garden_current$POWO_species[has_POWO])
            species_data = data.frame(original = garden_current$GenusSpecies, POWO = POWO_GenusSpecies, infra = garden_current$infrageneric_level)
            species_only = species_data[!grepl('5|6|0',species_data$infra),]
            species_only = species_only[!is.na(species_only$infra),]
            use_species = species_only$POWO
            use_species[is.na(use_species)] = species_only$original[is.na(use_species)]
            no_species = length(unique(use_species))

            return(list(number = no_species))
          })
          type_data = 'Species'
        }
        if(input$single_value_value_type == 'Genera' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of items
            no_items = length(unique(garden_current$genus))
            return(list(number = no_items))
          })
          type_data = 'Genera'
        }
        if(input$single_value_value_type == 'Families' && input$single_value_chart %in% c('Number Over Time')){
          time_series_info = lapply(values$plant_existing, function(x){
            garden_current = values$data[x,]

            # Number of items
            no_items = length(unique(garden_current$family))
            return(list(number = no_items))
          })
          type_data = 'Families'
        }
        if(input$single_value_chart %in% c('Number Over Time')){
          no_wanted = unlist(lapply(time_series_info, function(x){x$number}))
          wanted_data = data.frame(year = as.numeric(stringr::str_extract(names(no_wanted),'[0-9]{4}')), no_wanted = no_wanted)
          wanted_data$text = paste0('Date: ',names(no_wanted), ' <br>', type_data,': ', wanted_data$no_wanted)

          values$wanted_data = wanted_data
          values$type_data = type_data
        }

        if(input$single_value_value_type == 'Items' && !input$single_value_chart %in% c('Number Over Time')){
          working_with_data = values$data
          working_with_plant_existing = values$plant_existing
        }
        if(input$single_value_value_type == 'Accessions' && !input$single_value_chart %in% c('Number Over Time')){
          # reduce report to accessions and choose max deathdate. We don't consider accession year as this by definition should be the same for each accession.
          items = values$data$ItemAccNoFull
          accessions = unlist(lapply(stringr::str_split(items,pattern = '\\*'),function(x){x[1]}))
          unique_accessions = unique(accessions)

          data_to_group = data.frame(accessions = accessions, death_date = values$data$DeathDate)
          data_to_group$death_date[is.na(data_to_group$death_date)] = '3000-01-12'

          order_index = rev(order(data_to_group$death_date))
          data_to_group = data_to_group[order_index,]

          match_to_best = match(unique_accessions,data_to_group$accessions)

          working_with_data = values$data[order_index[match_to_best],]
          working_with_plant_existing = values$plant_existing[order_index[match_to_best],]

        }
        if(input$single_value_value_type == 'Taxa' && !input$single_value_chart %in% c('Number Over Time')){
          # reduce report to Taxa. Need to be careful as a taxa could be alive in multiple chunks.
          data_to_group = values$data

          taxa_lifespan <- data_to_group |>
            dplyr::group_by(.data$good_name) |>
            dplyr::summarise(acc_year = toString(.data$AccYear),
                             death_date = toString(.data$DeathDate),
                             infrageneric_level = .data$infrageneric_level[1]
            ) |>
            dplyr::ungroup()

          alive_each_year = data.frame(t(data.frame(pbapply::pblapply(taxa_lifespan$good_name, function(x){
            index_of_taxa = which(data_to_group$good_name == x)
            apply(BB[index_of_taxa,],MARGIN = 2,FUN = any)
          }))))

          working_with_data = taxa_lifespan
          working_with_plant_existing = alive_each_year

        }
        if(input$single_value_value_type %in% c('Items','Accessions', 'Taxa') && input$single_value_chart == 'Divided into Type of Taxa'){
          time_series_info = lapply(working_with_plant_existing, function(x){
            garden_current = working_with_data[x,]

            # Breakdown of infrageneric diversity.
            breakdown_infrageneric = table(garden_current$infrageneric_level)
            return(list(breakdown_infrageneric = breakdown_infrageneric))
          })
          type_data = 'Infrageneric Diversity'

          no_items = data.frame(t(data.frame(lapply(time_series_info, function(x){
            details = x$breakdown_infrageneric

            indet = sum(as.numeric(details[grepl('0',names(details))]))
            species = sum(as.numeric(details[grepl('1',names(details))]))
            infra = sum(as.numeric(details[grepl('2|3|4',names(details))]))
            hort = sum(as.numeric(details[grepl('5|6',names(details))]))

            return(c(species, infra, hort, indet))
          }))))
          names(no_items) = c('Species', 'Infraspecific', 'Horticultral', 'Indeterminate')
          prop = round(no_items/rowSums(no_items)*100,digits=2)
          names(prop) =paste0(names(prop),'_prop')
          infra_data = data.frame(year = years, no_items, prop)
          infra_data$text = paste0('Date: ', names(time_series_info),
                                   ' <br>', 'Species: ', infra_data$species,
                                   ' <br>', 'Infraspecific: ', infra_data$infra,
                                   ' <br>', 'Horticultral: ', infra_data$hort,
                                   ' <br>', 'Indeterminate: ', infra_data$indet)

          values$wanted_data = infra_data
          values$type_data = type_data
        }
        if(input$single_value_value_type %in% c('Items','Accessions') && input$single_value_chart == 'Divided into Provenance'){
          time_series_info = lapply(working_with_plant_existing, function(x){
            garden_current = working_with_data[x,]

            # Breakdown of provenance.
            breakdown = table(garden_current$ProvenanceCode)

            return(list(breakdown = breakdown))
          })
          type_data = 'Provenance'

          #Get the unique Provenance codes for the collection
          unique_provenance_values = unique(values$data$ProvenanceCode)

          # Get the number of plants with provenance code for each year.
          no_items = data.frame(t(data.frame(lapply(time_series_info, function(x){
            details = x$breakdown

            no_prov = as.numeric(details[match(unique_provenance_values, names(details))])
            no_prov[is.na(no_prov)] = 0

            return(no_prov)
          }))))
          names(no_items) = unique_provenance_values


          prop = round(no_items/rowSums(no_items)*100,digits=2)
          names(prop) =paste0(names(prop),'_prop')
          prov_data = data.frame(year = years, no_items, prop)

          values$wanted_data = prov_data
          values$type_data = type_data
        }
        if(input$single_value_value_type %in% c('Items','Accessions') && input$single_value_chart == 'Divided into Endemic Species'){
          time_series_info = lapply(working_with_plant_existing, function(x){
            garden_current = working_with_data[x,]

            # Breakdown of threatened
            breakdown = table(garden_current$threatened)

            return(list(breakdown = breakdown))
          })
          type_data = 'Threatened Species'

          #Get the unique Provenance codes for the collection
          unique_no_threatened_values = unique(values$data$threatened)[!is.na(unique(values$data$threatened))]

          # Get the number of plants with provenance code for each year.
          no_items = data.frame(t(data.frame(lapply(time_series_info, function(x){
            details = x$breakdown

            no_threatened = as.numeric(details[match(unique_no_threatened_values, names(details))])
            no_threatened[is.na(no_threatened)] = 0

            return(no_threatened)
          }))))
          names(no_items) = unique_no_threatened_values

          prop = round(no_items/rowSums(no_items)*100,digits=2)
          names(prop) =paste0(names(prop),'_prop')
          threatened_data = data.frame(year = years, no_items, prop)

          values$wanted_data = threatened_data
          values$type_data = type_data
        }
        if(input$single_value_value_type %in% c('Items','Accessions') && input$single_value_chart == 'Divided into Threatened Species'){
          time_series_info = lapply(working_with_plant_existing, function(x){
            garden_current = working_with_data[x,]

            # Breakdown of threatened
            breakdown = table(garden_current$threatened)

            return(list(breakdown = breakdown))
          })
          type_data = 'Threatened Species'

          unique_no_threatened_values = unique(values$data$threatened)[!is.na(unique(values$data$threatened))]

          # Get the number of plants with provenance code for each year.
          no_items = data.frame(t(data.frame(lapply(time_series_info, function(x){
            details = x$breakdown

            no_threatened = as.numeric(details[match(unique_no_threatened_values, names(details))])
            no_threatened[is.na(no_threatened)] = 0

            return(no_threatened)
          }))))
          names(no_items) = unique_no_threatened_values

          prop = round(no_items/rowSums(no_items)*100,digits=2)
          names(prop) =paste0(names(prop),'_prop')
          threatened_data = data.frame(year = years, no_items, prop)

          values$wanted_data = threatened_data
          values$type_data = type_data
        }
      }

      if(input$main_tabs == 'Turnover'){
        # remove data that has faulty accession year or death date.
        data_working = values$data
        data_plant_existing = values$plant_existing

        deathYear = unlist(stringr::str_extract(data_working$DeathDate,'^[0-9]{4}'))
        faulty_index = which(data_working$AccYear <1650 | is.na(data_working$AccYear) | deathYear < 1650 )

        data_working = data_working[-faulty_index,]
        data_plant_existing = data_plant_existing[-faulty_index,]

        if(input$turnover_type %in% c('Gain','Loss')){
          info = get_chart_data_turnover(data_working, data_plant_existing, isolate(input$turnover_type), isolate(input$turnover_quantity), isolate(input$turnover_type_of_chart), years)
          values$turnover_type = info$turnover_type
          values$turnover_type_data = info$turnover_type_data
          values$turnover_wanted_data = info$turnover_wanted_data
        }

        if(input$turnover_type == 'Net'){
          if(isolate(input$turnover_quantity) %in% c('Items', 'Accessions')){
            info_gain = get_chart_data_turnover(data_working, data_plant_existing, 'Gain', isolate(input$turnover_quantity), isolate(input$turnover_type_of_chart), years)
            info_loss = get_chart_data_turnover(data_working, data_plant_existing, 'Loss', isolate(input$turnover_quantity), isolate(input$turnover_type_of_chart), years)
          }else{
            info_gain = get_chart_data_turnover(data_working, data_plant_existing, 'Gain', isolate(input$turnover_quantity), 'Number of New to Collection', years)
            info_loss = get_chart_data_turnover(data_working, data_plant_existing, 'Loss', isolate(input$turnover_quantity), 'Number of Lost to Collection', years)
          }

          values$turnover_type = input$turnover_type
          values$turnover_type_data = list(gain = info_gain$turnover_type_data, loss = info_loss$turnover_type_data)
          values$turnover_wanted_data = list(gain = info_gain$turnover_wanted_data, loss = info_loss$turnover_wanted_data)
        }

      }



    }
  })
  #---- (END) Get information for plots.  --------------------------

  #---- Produce the datatable. ---------------------
  output$table = renderDT({
    dat_for_table = values$data
    if(input$data_filter == 'Choose Date'){
      if(length(input$data_select)>0){
        exist_on_date = unlist(exist_at_date(as.character(input$data_select), acc = dat_for_table$AccYear,
                                ItemStatusDate = dat_for_table$ItemStatusDate,
                                ItemStatusType = dat_for_table$ItemStatusType))
        dat_for_table = dat_for_table[exist_on_date,]
      }
    }
    if(input$data_filter ==  'Data with missing info'){
      deathYear = unlist(stringr::str_extract(dat_for_table$DeathDate,'^[0-9]{4}'))
      dat_for_table = dat_for_table[which(dat_for_table$AccYear <1650 | is.na(dat_for_table$AccYear) | deathYear < 1650 ),]
    }

    names(dat_for_table) = c("Item", "Report name", "Accession Year", "Death Date", "Link to POWO", "Provenance",
                             "Endemic", "Threatened", "RedList Category", "# Collections Globally",
                             'Native, not-extinct, not-doubtful',
                             "Native, extinct, not-doubtful",
                             "Native, not-extinct, doubtful",
                             "Native, extinct, doubtful",
                             "Introduced, not-extinct, not-doubtful",
                             "Introduced, extinct, not-doubtful",
                             "Introduced, not-extinct, doubtful",
                             "Infraspecific Level", "Genus", "Family", "Species",                    "Enrichment status", "Item Status Date", "Item Status Type", "Genus Species", "POWO taxon name", "POWO taxon authors",         "POWO genus", "POWO species" )

    dat_for_table = dat_for_table[,c(1,2,5,3:4,6:ncol(dat_for_table))]


    datatable(dat_for_table, rownames = FALSE, options = list(scrollY = '70vh', scrollX =  TRUE, pageLength =  200), escape = FALSE, filter="top")})

  # ==== Produce the summary plotly. ====
  output$summary = renderUI({
    print('Create summary figure')
    report_cur = values$data
    if(nrow(report_cur)==0){
      print('no data!')
      return()
    }else{
      print('we have data')

      # Number of Items.
      no_items = nrow(report_cur)

      # Number of Accessions.
      no_accessions = length(unique(unlist(lapply(stringr::str_split(report_cur$ItemAccNoFull,pattern = '\\*'),function(x){x[1]} ))))

      # Number of taxa
      no_taxa = length(unique(report_cur$good_name))

      #No families.
      no_families <- length(unique(report_cur$family))

      #No genus
      no_genus <- length(unique(report_cur$genus))


      #No species
      no_species = length(unique(report_cur$good_name[which(!grepl('0|5|6',report_cur$infrageneric_level))]))

      # Number matched to POWO.
      no_match_powo = sum(grepl('POWO', report_cur$enrichment_status), na.rm=T)
      match_powo_percent = round(no_match_powo/no_items*100, digits=2)

      message = paste0('- <b>', format(no_items,big.mark=','), '</b> Items in the collection. <br> - <b>',
                       format(no_accessions,big.mark=','),'</b> Accessions in the collection. <br> - <b>',
                       format(no_taxa,big.mark=','),'</b> Taxa. <br>  - <b>',
                       format(no_species,big.mark=','),'</b> Species. <br>  - <b>',
                       format(no_genus,big.mark=','),'</b> Genera. <br>  - <b>',
                       format(no_families,big.mark=','),'</b> Families. <br> Of the selected records <b>',match_powo_percent,'%</b> (',format(no_match_powo,big.mark=','),') were matched to POWO. <br>')
      HTML(message)
    }
  })

  # ==== Produce the whole collection plotly. ====
  output$change_time_single_value = renderUI({
    input$chart_colour

    print('Create Whole Collection figure')
    if(nrow(values$data)==0){
      print('no data!')
      return()
    }else{

      wanted_data = values$wanted_data
      type_data = values$type_data

      if(isolate(input$single_value_chart) %in% c('Number Over Time')){
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$no_wanted)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~no_wanted, text = ~text, hoverinfo = 'text', inherit = FALSE, line = list(color = 'rgb(22, 96, 167)'))
        # fig <- plot_ly(items_data, x = ~year, y = ~no_items, type = 'scatter', mode = 'lines', text = ~text, hoverinfo = 'text')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of ', type_data))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            )
          ))
        )
      }
      if(!isolate(input$single_value_chart) %in% c('Number Over Time')){
        infra_data = wanted_data
        infra_data1 = infra_data[,!grepl('_prop',names(infra_data))]
        infra_data1 = infra_data1[,names(infra_data1) != 'text']
        max_value = max(infra_data1[,-1],na.rm=T)
        infra_columns =2:(ncol(infra_data1))
        # colours = viridis::inferno(length(infra_columns))
        if(input$chart_colour_reverse){
          colours = colourScheme(n=length(infra_columns))
        }else{
          colours = colourScheme(length(infra_columns), direction = -1)
        }
        infra_labels = names(infra_data1)[infra_columns]
        infra_labels = stringr::str_replace_all(infra_labels,pattern = '\\.',' ')

        fig1 = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max_value), add_annotate = input$add_annotate)
        # fig <- plot_ly(infra_data1, x = ~year)
        for(i in 1:length(infra_columns)){
          fig1 <- fig1 %>% add_trace(x =infra_data1[,1], y = infra_data1[,infra_columns[i]], name = infra_labels[i], mode = 'lines',type = 'scatter', line = list(color = colours[i]), inherit = FALSE)
        }
        fig1 <- fig1 %>% layout(title = "",
                                xaxis = list(title = "Year"),
                                yaxis = list (title = paste0("Number of ", input$single_value_value_type),
                                              hoverformat = ",.0f"))
        fig1 <- fig1 %>% layout(hovermode = 'x unified')

      fig1 <- fig1  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

      ## Grouped species, hort etc RAW NUMBERS FOUR GRAPH OPTIONS.
      infra_data1 = infra_data[,c(1, which(grepl('prop', names(infra_data))))]
      infra_data1[,-1] = infra_data1[,-1]/100
      infra_columns =2:(ncol(infra_data1))
      if(input$chart_colour_reverse){
        colours = colourScheme(length(infra_columns))
      }else{
        colours = colourScheme(length(infra_columns), direction = -1)
      }
      infra_labels = stringr::str_remove(names(infra_data1)[infra_columns],pattern = '_prop')
      infra_labels = stringr::str_replace_all(infra_labels,pattern = '\\.',' ')

      max_value = max(infra_data1[,-1],na.rm=T)

      fig2 = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max_value), add_annotate = input$add_annotate)
      for(i in 1:length(infra_columns)){
        fig2 <- fig2 %>% add_trace(x =infra_data1[,1], y = infra_data1[,infra_columns[i]], name = infra_labels[i], mode = 'lines',type = 'scatter', line = list(color = colours[i]), inherit = FALSE)
      }
      fig2 <- fig2 %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title =  paste0("Proportion of ", input$single_value_value_type),
                                            tickformat = ".0%",
                                            hoverformat = '.2%'))
      fig2 <- fig2 %>% layout(hovermode = 'x unified')
      fig2 <- fig2  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

      p = htmltools::browsable(
        tagList(list(
          tags$div(h3(paste0('Number of ', input$single_value_value_type, ' by ', type_data))),
          tags$div(
            style = 'width:100%;display:block;float:left;',
            fig1
          ),
          tags$div(h3(paste0('Proportion of ', input$single_value_value_type, ' by ', type_data))),
          tags$div(
            style = 'width:100%;display:block;float:left;',
            fig2
          )
        ))
      )
      }


    p
    }

  })

  # ==== Produce the turnover plotly. ====
  output$change_time_turnover = renderUI({
    print('Create turnover figure')

    if(nrow(values$data)==0){
      print('no data!')
      return()
    }else{

      wanted_data = values$turnover_wanted_data
      type_data = values$turnover_type_data
      print(dim(wanted_data))

      if(isolate(input$turnover_type_of_chart) %in% c('Number Over Time') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$no_wanted)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~no_wanted, inherit = FALSE, name ='')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))


          p = htmltools::browsable(
            tagList(list(
              tags$div(h3(paste0('Number of ', values$turnover_type,' ', type_data))),
              tags$div(
                style = 'width:100%;display:block;float:left;',
                fig
              )
            ))
          )

      }
      if(isolate(input$turnover_type_of_chart) %in% c('Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        # Get the names of the different lines.
        name_of_traces = names(wanted_data)
        name_of_traces = stringr::str_replace_all(name_of_traces,pattern = '\\.',' ')


        # Define the colours used for the lines.
        if(input$chart_colour_reverse){
          colours = colourScheme(n=length(2:ncol(wanted_data)))
        }else{
          colours = colourScheme(length(2:ncol(wanted_data)), direction = -1)
        }

        # Create plot for the number of new objects over time.
        fig1 = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(unlist(wanted_data[,-1]))), add_annotate = input$add_annotate)
        for(i in 2:ncol(wanted_data)){
          fig1 <- fig1 %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = wanted_data[,i], inherit = FALSE, name = name_of_traces[i], line = list(color = colours[i-1]))
        }
        fig1 <- fig1 %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig1 <- fig1 %>% layout(hovermode = 'x unified')
        fig1 <- fig1 %>% layout(legend = list(orientation = 'h', x = 0, y = 1.1))
        fig1 <- fig1  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))


        # Create plot for the proportion of new objects over time.
        prop_data = data.frame(year = wanted_data$year, wanted_data[,-1]/rowSums(wanted_data[,-1]))

        fig2 = regions_plot(event_data = event_values$dfWorking, ylim = c(0, 1), add_annotate = input$add_annotate)

        for(i in 2:ncol(prop_data)){
          fig2 <- fig2 %>% add_trace(type = 'scatter', mode = 'lines', data = prop_data, x = ~year, y = prop_data[,i], inherit = FALSE, name = name_of_traces[i], line = list(color = colours[i-1]))
        }
        fig2 <- fig2 %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Proportion of ",type_data),
                                            tickformat = ".0%",
                                            hoverformat = '.2%'))
        fig2 <- fig2 %>% layout(hovermode = 'x unified')
        fig2 <- fig2 %>% layout(legend = list(orientation = 'h', x = 0, y = 1.1))
        fig2 <- fig2  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))
        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of New ', type_data, ' Over Time ', isolate(input$turnover_type_of_chart) ))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig1
            ),
            tags$div(h3(paste0('Proportion of New ', type_data, ' Over Time ', isolate(input$turnover_type_of_chart) ))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig2
            )
          ))
        )
      }
      if(isolate(input$turnover_type_of_chart) %in% c('Number Accessioned Over Time') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$no_wanted)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~no_wanted, inherit = FALSE, name ='')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of ', type_data, ' With New Items By Year'))),
            tags$div(htmltools::p(paste0('Of the items accessioned each year, the number of ', type_data, ' represented in the gain. Note that this does not mean the ',type_data, ' were not in the collection previsouly collection, only that new items have been added.'))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            )
          ))
        )


      }
      if(isolate(input$turnover_type_of_chart) %in% c('Number of New to Collection') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        # Produce the chart.
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$new_to_year)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~new_to_year, inherit = FALSE, name ='')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        #Create linked table.
        names(wanted_data) = c('Year', 'Number', type_data)
        wanted_data = wanted_data[nrow(wanted_data):1,]
        dt  = datatable(wanted_data, rownames = FALSE, options = list(scrollY = '70vh', scrollX =  TRUE, pageLength =  200), escape = FALSE)

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of New ', type_data, ' Added to the Collection Each Year'))),
            tags$div(htmltools::p(paste0('By New ', type_data, ' we mean that it did not exist in the previous year. Therefore, we may find new ', type_data , ' reoccur over time, if they are removed in the preceding years. '))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            ),
            tags$div(htmltools::p('')),
            tags$div(htmltools::p('Linked table')),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              dt
            )
          ))
        )


      }
      if(isolate(input$turnover_type_of_chart) %in% c('Number Lost Over Time') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$no_wanted)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~no_wanted, inherit = FALSE, name ='')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of ', type_data, ' With Items Lost By Year'))),
            tags$div(htmltools::p(paste0('Of the items lost each year, the number of ', type_data, ' represented in the loss. Note that this does not mean the ',type_data, ' have been lost to the collection, only that some items are no longer existing.'))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            )
          ))
        )


      }
      if(isolate(input$turnover_type_of_chart) %in% c('Number of Lost to Collection') && isolate(input$turnover_type) %in% c('Gain','Loss')){
        # Produce the chart.
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(0, max(wanted_data$loss_to_year)), add_annotate = input$add_annotate)
        fig <- fig %>% add_trace(type = 'scatter', mode = 'lines', data = wanted_data, x = ~year, y = ~loss_to_year, inherit = FALSE, name ='')
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        #Create linked table.
        names(wanted_data) = c('Year', 'Number', type_data)
        wanted_data = wanted_data[nrow(wanted_data):1,]
        dt  = datatable(wanted_data, rownames = FALSE, options = list(scrollY = '70vh', scrollX =  TRUE, pageLength =  200), escape = FALSE)

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Number of ', type_data, ' Lost to the Collection Each Year'))),
            tags$div(htmltools::p(paste0('By ', type_data, ' lost to the collection each year we mean that in the given year there was recorded losses and those families did not exist the following year.'))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            ),
            tags$div(htmltools::p('')),
            tags$div(htmltools::p('Linked table')),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              dt
            )
          ))
        )


      }

      # Outputs for Net turnovers.
      if(isolate(input$turnover_type_of_chart) %in% c('Number Over Time') && isolate(input$turnover_type) %in% c('Net')){

        net = wanted_data$gain$no_wanted - wanted_data$loss$no_wanted
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(min(net), max(net)), add_annotate = input$add_annotate, alpha = 0.4)

        text = paste0('Date: ', wanted_data$gain$year,
                      '<br> Gain:', wanted_data$gain$no_wanted,
                      '<br> Loss:', wanted_data$loss$no_wanted,
                      '<br> Net:', net)
        plus_minus = net > 0
        cols = rep('#FF0000', length(plus_minus))
        cols[plus_minus] = '#0000FF'

        fig <- fig %>% add_trace(type = 'bar', x = wanted_data$gain$year, y = net, inherit = FALSE, name ='', hovertext = text, hoverinfo = 'text',  marker = list(color = cols))
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Net Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig %>% layout(showlegend = FALSE)
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))


        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Net Number of ', type_data[[1]]))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            )
          ))
        )

      }
      if(isolate(input$turnover_type_of_chart) %in% c('Number of Net to Collection') && isolate(input$turnover_type) %in% c('Net')){
        net = wanted_data$gain$new_to_year - wanted_data$loss$loss_to_year
        fig = regions_plot(event_data = event_values$dfWorking, ylim = c(min(net), max(net)), add_annotate = input$add_annotate, alpha = 0.4)

        text = paste0('Date: ', wanted_data$gain$year,
                      '<br> Gain:', wanted_data$gain$new_to_year,
                      '<br> Loss:', wanted_data$loss$loss_to_year,
                      '<br> Net:', net)
        plus_minus = net > 0
        cols = rep('#FF0000', length(plus_minus))
        cols[plus_minus] = '#0000FF'

        fig <- fig %>% add_trace(type = 'bar', x = wanted_data$gain$year, y = net, inherit = FALSE, name ='', hovertext = text, hoverinfo = 'text', marker = list(color = cols))
        fig <- fig %>% layout(title = "",
                              xaxis = list(title = "Year"),
                              yaxis = list (title = paste0("Net Number of ",type_data)))
        fig <- fig %>% layout(hovermode = 'x')
        fig <- fig %>% layout(showlegend = FALSE)
        fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))

        #Create linked table.
        table_data = data.frame(Year = wanted_data$gain$year, gain = wanted_data$gain[[3]], loss = wanted_data$loss[[3]])
        names(table_data) = c('Year', 'Gained', 'Lost')
        table_data = table_data[nrow(table_data):1,]
        dt  = datatable(table_data, rownames = FALSE, options = list(scrollY = '70vh', scrollX =  TRUE, pageLength =  200), escape = FALSE)

        p = htmltools::browsable(
          tagList(list(
            tags$div(h3(paste0('Net Number of ', type_data[[1]]))),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              fig
            ),
            tags$div(htmltools::p('')),
            tags$div(htmltools::p('Linked table')),
            tags$div(
              style = 'width:100%;display:block;float:left;',
              dt
            )
          ))
        )


      }
      if(isolate(input$turnover_type_of_chart) %in% c('Divided into Type of Taxa', 'Divided into Provenance', 'Divided into Endemic Species', 'Divided into Threatened Species') && isolate(input$turnover_type) %in% c('Net')){
        gain = wanted_data$gain
        loss = wanted_data$loss
        # Get the names of the different lines.
        name_of_traces = unique(c(names(gain),names(loss)))
        name_of_traces = name_of_traces[!name_of_traces == 'year']
        # name_of_traces = stringr::str_replace_all(name_of_traces,pattern = '\\.',' ')

        # create the net turnover for each.
        figures= list()
        for(i in 1:length(name_of_traces)){
          gain_no = as.numeric(unlist(gain[match(name_of_traces[i], names(gain))]))
          loss_no = as.numeric(unlist(loss[match(name_of_traces[i], names(loss))]))

          net = gain_no - loss_no
          fig = regions_plot(event_data = event_values$dfWorking, ylim = c(min(net), max(net)), add_annotate = input$add_annotate, alpha = 0.4)


          text = paste0('Date: ', wanted_data$gain$year,
                        '<br> Gain:', gain_no,
                        '<br> Loss:',  loss_no,
                        '<br> Net:', net)
          plus_minus = net > 0
          cols = rep('#FF0000', length(plus_minus))
          cols[plus_minus] = '#0000FF'

          fig <- fig %>% add_trace(type = 'bar', x = wanted_data$gain$year, y = net, inherit = FALSE, name ='', hovertext = text, hoverinfo = 'text', marker = list(color = cols))
          fig <- fig %>% layout(title = "",
                                xaxis = list(title = "Year"),
                                yaxis = list (title = paste0("Net Number of ",type_data[[1]])))
          fig <- fig %>% layout(hovermode = 'x')
          fig <- fig %>% layout(showlegend = FALSE)
          fig <- fig  %>%layout(xaxis = list(range=c(as.numeric(input$single_value_min_year),as.numeric(input$single_value_max_year))))
          figures[[i]] = fig

        }
        name_of_traces = stringr::str_replace_all(name_of_traces,pattern = '\\.',' ')

        to_eval = paste0("tags$div(h3(paste0('Net ", type_data[[1]], " Over Time For ",name_of_traces,"' ))), tags$div(style = 'width:100%;display:block;float:left;', figures[[",1:length(figures),"]] ), ", collapse ='')
        to_eval = stringr::str_sub(to_eval,start = 1,end = -3)
        to_eval = paste0('htmltools::browsable(
          tagList(list(',to_eval,
          '          ))
        )', collapse='')

      p=  eval(parse(text=to_eval))

      }


      p
    }

  })

}

shinyApp(ui,server)

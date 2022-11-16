'vcColors' <- function() {
	var_cols = c("Hotspot" = "#BA252C",
		     "Nonstop_Mutation" = "#8C4C8C",
		     "Nonsense_Mutation" = "#8C4C8C",
		     "Missense_Mutation" = "#4EA147",
		     "Frame_Shift_Ins" = "#3D75A5",
		     "Frame_Shift_Del" = "#3D75A5",
		     "Splice_Site" = "#F2E943",
		     "In_Frame_Ins" = "#E6772B",
		     "In_Frame_Del" = "#E6772B",
		     "Translation_Start_Site" = "#97522E",
		     "IGR" = "#B2DF8A",
		     "Silent" = "#FB9A99",
		     "RNA" = "#FDBF6F",
		     "Intron" = "#CAB2D6",
		     "ITD" = "#9E0142",
		     "Multi_Hit" = "#000000",
		     "Amp" = "#EE82EE",
		     "Del" = "#4169E1",
		     "Complex_Event" = "#7B7060",
		     "pathway" = "#535C68")
	return(invisible(var_cols))
}

'mafLegend' = function(path = ".") {
	var_cols = vcColors()
	key_cols = dplyr::tibble(variant_classification = names(var_cols),
				 variant_color = as.character(var_cols)) %>%
		   dplyr::filter(!(variant_classification %in% c("Nonsense_Mutation", "Frame_Shift_Del", "In_Frame_Del", "IGR", "Silent", "RNA", "Intron", "ITD", "Multi_Hit", "Amp", "Del", "Complex_Event", "pathway"))) %>%
		   dplyr::mutate(variant_classification = case_when(
			    variant_classification == "Nonstop_Mutation" ~ "Truncating SNV",
			    variant_classification == "Missense_Mutation" ~ "Missense SNV",
			    variant_classification == "Frame_Shift_Ins" ~ "Frameshift Indel",
			    variant_classification == "Splice_Site" ~ "Splice site",
			    variant_classification == "In_Frame_Ins" ~ "In-frame Indel",
			    variant_classification == "Translation_Start_Site" ~ "Translation start site",
			    TRUE ~ variant_classification
		    )) %>%
		   dplyr::bind_rows(dplyr::tibble(variant_classification = "Loss of Heterozygosity",
					 	  variant_color = "#000000")) %>%
		   dplyr::mutate(variant_classification = factor(variant_classification, levels = c("Hotspot", "Truncating SNV", "Missense SNV", "Frameshift Indel", "In-frame Indel", "Splice site", "Translation start site", "Loss of Heterozygosity"))) %>%
		   dplyr::mutate(variant_shape = case_when(
			   variant_classification == "Loss of Heterozygosity" ~ 1,
			   TRUE ~ 0
		   )) %>%
		   dplyr::mutate(variant_shape = factor(variant_shape))
	manual_cols = key_cols %>% .[["variant_color"]]
	names(manual_cols) = key_cols %>% .[["variant_classification"]]
	plot_ = ggplot(data = key_cols, aes(x = variant_classification, y = variant_color, fill = variant_classification)) +
		geom_point(stat = "identity", shape = 22, size = 5, color = "black") +
		scale_fill_manual(values = manual_cols) +
		guides(fill = guide_legend(title = "Somatic mutation type")) +
		theme_classic()
	legend_ = cowplot::get_legend(plot_)
	grid.newpage()
	grid.draw(legend_)
}

library(purrr)

get_model_figs <- function(exp_obs_model,
                           morphology_obs_model,
                           morphology_exp_model,
                           dat,
                           ylim_vals = c(-3,3)){
  
  mods <- list(exp_obs_model,
               morphology_obs_model,
               morphology_exp_model)
  
  dats <- list(dat,
               dat |> filter(Type.of.Study..Experimental_ObservatioNAl. == "Observational"),
               dat |> filter(Type.of.Study..Experimental_ObservatioNAl. == "Experimental")
  )
  
  groups <- list("Type.of.Study..Experimental_ObservatioNAl.",
                 "Single.or.Multi.Stipe",
                 "Single.or.Multi.Stipe")
  
  coefs <- pmap(list(mods, dats,groups), make_one_model_summary_tab)
  
  pmap(list(coefs, dats, groups),
       make_model_fig,
       ylim_vals = ylim_vals)
}


make_one_model_summary_tab <- function(mod, dat, group_var){
  group_var_ <- syms(group_var)
  
  #coef table
  ctab <- coef(summary(mod))
  ctab$Type <- rownames(ctab)
  levels(factor(ctab$Type))
  ctab$Type <- gsub(group_var, "", ctab$Type)
  
  dat_summary <- dat %>% group_by(!!!group_var_) %>%
    dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
    dplyr::rename(Type = group_var)
  
  ctab <- left_join(ctab, dat_summary) |>
    mutate(type_with_samplesize = glue("{`Type`}\n({n})"))
  
  ctab
}

#make_one_model_summary_tab(totalHedges, totalabund, "Type.of.Study..Experimental_ObservatioNAl.")

make_model_fig <- function(ctab, dat, group_var, ylim_vals = c(-3,3)){
  group_var_ <- sym(group_var)
  ggplot(ctab, aes(x=Type, y=estimate)) +
    geom_jitter(data=dat, mapping=aes(x = !!(group_var_), 
                                      y = Hedges.G), 
                alpha=0.7,
                color = "grey",
                position=position_jitter(width = .1), 
                size=3) +
    
    geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
    geom_linerange(mapping = aes(ymin=ci.lb, ymax=ci.ub),
                   position=position_dodge(width=0.5),
                   linewidth = 1) +
    geom_hline(yintercept=0, lwd=1.4, lty=2) +
    ylim(ylim_vals) +
    ylab("Hedge's G") + xlab("Study Type") +
    scale_x_discrete(labels = ctab$type_with_samplesize)
}

#make_model_fig(ctab, dat, "Type.of.Study..Experimental_ObservatioNAl.")

library(patchwork)
make_3_panel_fig <- function(figs){
  (figs[[1]] + labs(subtitle = "A. Pooled Data")) + 
    (figs[[2]] + labs(y = NULL, 
                              subtitle = "B. Observational Data Only")) + 
    (figs[[3]] + labs(y = NULL,
                              subtitle = "C. Experimental Data Only"))
  
}

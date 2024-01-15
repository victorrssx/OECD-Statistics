  
  ########################################################
  ################                        ################
  ################          OCDE          ################
  ################       14/12/2023       ################
  ################                        ################
  ########################################################
  
  
  pacman::p_load(tidyverse, rvest, xml2, # tidyverse e adjacentes
                 OECD, # usar o repositório Fork: install_github("VIG-RM/OECD") 
                 janitor, lubridate, ggtext, ggrepel, extrafont, scales, ggalt, zoo,
                 countrycode, gghighlight, ggimage, glue, grid, png)
  pacman::p_loaded()
  
  options(timeout = max(1000, getOption("timeout")))
  
  ## Por quê usar install_github("VIG-RM/OECD")? ----
  
   # Ao que parece, o pacote OECD deixou de ser atualizado pelo criador original. Por esse motivo,
   # baixei uma Fork que aparentemente corrigiu os bugs anteriores.
  
   # Repositório original (porém desatualizado): https://github.com/expersso/OECD

    
  ## Importando dados ---- 
  
   # População 
    
   search_dataset("population", data = get_datasets()) 
   str = get_data_structure("EDU_DEM")
   str[[1]] %>% View
  
   pop = get_dataset(dataset = "EDU_DEM") %>% 
           filter(AGE == "_T") %>% # Caso necessário SEX == "_T"
           select(year = Time, country = COUNTRY, sex = SEX, pop = ObsValue) %>% 
           mutate(pop = as.numeric(pop))
     
   
   # Graduados por Área de Conhecimento
   
   search_dataset("graduates", data = get_datasets())  
   str = get_data_structure("EDU_GRAD_FIELD")
   str[[1]] %>% View
  
   grad = get_dataset(dataset = "EDU_GRAD_FIELD") %>% # Argumento 'filter' bem complicadinho de usar 
               filter(MOBILITY == "_T", # Caso necessário SEX == "_T" 
                      EDUCATION_FIELD == "F042", EDUCATION_LEV == "ISCED11_6") %>% # Bachelor's or equivalent level 
               select(year = Time, country = COUNTRY, field = EDUCATION_FIELD, sex = SEX, grad = ObsValue) %>%
               mutate(grad = as.numeric(grad))
   
   
   # Combinando os dois dataframes para gerar dados per capita
   
   gradpc = grad %>% 
              left_join(pop, by = c("year", "country", "sex")) %>% 
              mutate(gradpc = (grad / pop) * 100000)
           
                 
 
   
  ## Plotando ----
  
  theme_set(theme_minimal(base_size = 15))  
  tema_base = theme(plot.title = element_markdown(size = 30, family = "WillyWonka"),
                    plot.subtitle = element_markdown(size = 15, family = "Arial Nova", lineheight = 1.2),
                    axis.text.x = element_markdown(size = 12, hjust = 1),
                    axis.text = element_markdown(size = 15, color = "black"),
                    axis.line = element_line(color = "black"),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_rect(colour = "white", fill = "white"),
                    plot.background = element_rect(colour = "white", fill = "white"),
                    plot.caption = element_markdown(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")))

   
   # Gráfico
    # https://stackoverflow.com/questions/52395478/ggsave-does-not-bolden-text-and-it-changes-font-of-all-text-instead-of-just-p
   
   gradpc %>% 
     filter(year == max(year), !is.nan(gradpc)) %>%
     mutate(iso2c = countrycode(country, origin = "iso3c", destination = "iso2c")) %>% 
   {ggplot(., aes(x = reorder(country, -gradpc, sum))) + 
     geom_col(data = . %>% filter(sex == "_T"), 
              aes(y = gradpc), fill = "#a9d297", width = 0.5) +
     geom_point(data = . %>% filter(sex != "_T"),
                aes(y = gradpc, fill = sex, shape = sex), color = "black", size = 4, show.legend = F) +
     geom_flag(aes(image = iso2c), y = -5)  +
     expand_limits(y = -5) + 
     geom_hline(yintercept = 0) +
     #annotation_custom(readPNG("./linkedin.png") %>% rasterGrob(interpolate = TRUE), x = 30, y = 78, ymax = 85, xmax = 34) +
     #annotate("text", x = 31, y = 78, label = "victorlibera") +
     coord_cartesian(clip = 'off') +
     scale_fill_manual(values = c("#ff9c9c", "#6fa8dc")) +
     scale_shape_manual(values = c(24, 23)) +
     scale_y_continuous(breaks = seq(0, 60, 10)) +
     labs(title = "A   Fantastica   Fabrica   de   Advogados -------------------------------------",
          subtitle = glue("Em 2021, o número de advogados formados no Brasil foi o maior registrado na base de dados da OCDE, tanto em <br>",
                           " números absolutos quanto por 100 mil hab. (abaixo).",
                           " Curiosamente, em termos proporcionais formam-se mais <br>", ' <span style = "color:#f08080">**mulheres**</span> ',
                            "do que", ' <span style = "color:#6fa8dc">**homens**</span> ', " em **todos** os países com dados disponíveis."),
          caption = " <br> Fonte: Elaboração própria a partir de dados do *Education at a Glance* via OCDE.Stat.",
          x = "", y = "", color = "", fill = "", shape = "") +
     tema_base +
     theme(axis.text.x = element_markdown(angle = 90),
           axis.line = element_blank(),
           panel.grid.major.y = element_line(linewidth = 0.3, linetype = "dashed", color = "#d6d6d6"),
           legend.position = c(0.6, 0.85),
           legend.direction = "horizontal",
           legend.text = element_markdown(size = 15))} %>% 
    ggsave("Imagem.png", ., width = 12, height = 7, units = "in", dpi = 300)

   
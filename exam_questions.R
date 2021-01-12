
library(tidyverse)
library(teachR)


###################### if creating question bank from scratch ######################
ff <- list.files("AnD_exam", pattern = "eq_.{2}_sit\\.[Rr]md", full.names = T)
auth <-  toupper(sub(".*_(.{2})_.*\\.[txrmd]+$", "\\1", tolower(ff)))

qb <- question.bank(ff, auth)

# qb <- question.bank("AnD_exam/and_sit_final.Rmd", "And team")
#################################### else ##########################################
# qb <- read.csv(...)




# section_weights <- "Section A is worth 66 marks (61%). Section B is worth 20 marks (19%). Section C is worth 22 marks (20%)."
diet <- list(sit = "BSc FIRST YEAR EXAMINATION May/June 2020 (A2)",
             resit = "BSc FIRST YEAR EXAMINATION August/September 2020 (A3)",
             sample = "BSc FIRST YEAR EXAMINATION 2020")

boiler <- sussex.boilerplate(module = "Analysing Data",
                   module_code = "C8891",
                   exam_diet = diet$sit,
                   time_allowed = "2 hours",
                   n_sections = 1,
                   marks_per_q = 2,
                   section_weights = NULL,
                   sample = F)


#### pick items for exam
## this needs to be figured out better

paper_qu <- qb[is.na(qb$group), ] # %>%
# group_by(diff) %>%
# filter(q_num %in% sample(q_num, floor(length(q_num)/2)))

# temporary line to pick all multi Qs and add single ones till there's 50 Qs - REWORK for cases when there's more than 50 multi Qs
set.seed(420)
pick_single <- 50 - (nrow(qb) - nrow(paper_qu))
paper_qu <- paper_qu[sample(nrow(paper_qu), pick_single), ]
paper_qu <- rbind(paper_qu, qb[!is.na(qb$group), ])

make.paper(items = paper_qu,
           code_file = "AnD_exam/and_ecode_sit.Rmd",
           output_format = "word",
           out_file_name = "AnD_mcq_2020_sit_revised",
           yaml_header = boiler$yaml,
           title_page = boiler$title_page,
           ref_file = NULL,
           randomise_qs = TRUE,
           randomise_response_opts = TRUE,
           sample = FALSE)

############################### RESIT ########################################

ff <- list.files("AnD_exam", pattern = "eq_.{2}_resit\\.[Rr]md", full.names = T)
auth <-  toupper(sub(".*_(.{2})_.*\\.[txrmd]+$", "\\1", tolower(ff)))

qb <- question.bank(ff, auth)

boiler <- sussex.boilerplate(module = "Analysing Data",
                             module_code = "C8891",
                             exam_diet = diet$resit,
                             time_allowed = "2 hours",
                             n_sections = 1,
                             marks_per_q = 2,
                             section_weights = NULL,
                             sample = F)

paper_qu <- qb[is.na(qb$group), ]

set.seed(420)
pick_single <- 50 - (nrow(qb) - nrow(paper_qu))
paper_qu <- paper_qu[sample(nrow(paper_qu), pick_single), ]
paper_qu <- rbind(paper_qu, qb[!is.na(qb$group), ])

make.paper(items = paper_qu,
           code_file = "AnD_exam/and_ecode_resit.Rmd",
           output_format = "word",
           out_file_name = "AnD_mcq_2020_resit",
           yaml_header = boiler$yaml,
           title_page = boiler$title_page,
           ref_file = NULL,
           randomise_qs = FALSE,
           randomise_response_opts = FALSE,
           sample = FALSE)


############################### SAMPLE ########################################
qb <- question.bank("AnD_exam/and_eq_samp.Rmd", "JM") # sample paper

boiler <- sussex.boilerplate(module = "Analysing Data",
                             module_code = "C8891",
                             exam_diet = diet$sample,
                             time_allowed = "2 hours",
                             n_sections = 1,
                             marks_per_q = 2,
                             section_weights = NULL,
                             sample = F)

paper_qu <- qb[is.na(qb$group), ]

pick_single <- 50 - (nrow(qb) - nrow(paper_qu))
paper_qu <- paper_qu[sample(nrow(paper_qu), pick_single), ]
paper_qu <- rbind(paper_qu, qb[!is.na(qb$group), ])

make.paper(items = paper_qu,
           code_file = "AnD_exam/and_ecode_samp.Rmd",
           output_format = "word",
           out_file_name = "AnD_mcq_2020_sample",
           yaml_header = boiler$yaml,
           title_page = boiler$title_page,
           ref_file = ref_docx,
           randomise_qs = TRUE,
           randomise_response_opts = TRUE,
           sample = TRUE)


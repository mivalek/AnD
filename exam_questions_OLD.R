
library(tidyverse)
source("AnD_exam/exam_funs.R")
source("AnD_exam/pre_processor.R")

out_file_name <- "AnD_sit.Rmd"

ff <- list.files("AnD_exam", pattern = "eq_.{2}_sit\\.[Rr]md", full.names = T)
auth <-  toupper(sub(".*_(.{2})_.*\\.[txrmd]+$", "\\1", tolower(ff)))
ref_docx <- normalizePath("analysing_data/admin/mcq_format_ref.docx")


module <- "Analysing Data"
exam_diet <- "BSc FIRST YEAR EXAMINATION May/June 2019 (A2)"

time_allowed <- "2 hours"
n_sections <- 1
marks_per_q <- 2
# section_weights <- "Section A is worth 66 marks (61%). Section B is worth 20 marks (19%). Section C is worth 22 marks (20%)."

module_code <- "C8891"
output <- "word" # or pdf


instruction <- c(
  "INSTRUCTIONS",
  "Do not write your name on this question paper.",
  "",
  "Do not tear off any part of this question paper.",
  "",
  "Do not, under any circumstances, remove this question paper, used or unused, from the examination room. It must be left on your desk when you leave the examination.",
  "",
  paste("**Time allowed:", time_allowed, "**"),
  "",
  "Answer **ALL** questions.",
  "",
  paste0("There ",
        ifelse(n_sections > 1,
               paste("are ", n_sections, " sections"),
               " is one section"),
        " in this exam paper",
        if (n_sections > 1) paste0(" (", paste(LETTERS[1:n_sections], collapse = ", "), ")"),
        ". All questions are worth ",  marks_per_q, " marks. ",
        if (n_sections > 1) section_weights),
  "", "", "",
  "Mark your answers on the answer sheet provided.",
  "",
  "**ALL THE RELEVANT TABLES THAT YOU NEED TOANSWER THE QUESTIONSARE PROVIDED IN THE HANDOUT: STATISTICAL TABLES.**"
)


### set options
# save chunk default options
default_chunk_opts <- knitr::opts_chunk$get()[c("echo", "error", "message", "warning", "eval", "comment")]
options(knitr.kable.NA = '')
knitr::opts_chunk$set(echo=F, error=T, include=T, message=F, warning=F, comment=NA)

pretty_inline_num <-  c(
  "```{r setup}",
  if (output == 'pdf') {
    c(
      "hook_inline = knitr::knit_hooks$get('inline')",
      "  knitr::knit_hooks$set(",
      "    inline = function(x) {",
      "      res = hook_inline(x)",
      "      if (is.numeric(x)) gsub('(-.*)', '$\\1$', prettyNum(format(x, scientific=FALSE), big.mark=',')) else res",
      "    }",
      "  )"
    )
  } else {
    c(
      "hook_inline = knitr::knit_hooks$get('inline')",
      "  knitr::knit_hooks$set(",
      "    inline = function(x) {",
      "      res = hook_inline(x)",
      "      if (is.numeric(x)) gsub('-', '&minus;', prettyNum(format(x, scientific=FALSE), big.mark=',')) else res",
      "    }",
      "  )"
    )
  },
  "```", ""
)



all_questions <- all_groups <- list()
group_counter <- first_gr_couter <-  0
for (i in seq_along(ff)) {
  first_gr_couter <- first_gr_couter + 1
  questions <- readLines(ff[i])
  questions <- questions[grep("^\\s*$", questions, invert = T)]
  
  
  group_ind <-  cbind(
    grep("^\\s*\\[multi\\]", questions),
    grep("^\\}", questions)
  )
  
  if (length(group_ind) > 0) {
    group_ind <- split(
      group_ind,
      rep(1:nrow(group_ind), 2))
    
    groups <- lapply(group_ind, function(ind) questions[seq(ind[1], ind[2])])
    group_qus <- lapply(groups, function(x) x[(grep("\\{\\s*$", groups[[1]]) + 1):(length(x) - 1)])
    
    questions <- questions[-unname(
      unlist(
        lapply(group_ind, function(x) seq(x[1], x[2]))
      )
    )]
  }
  
  
  
  temp <- list()
  if (length(questions) > 0)
    temp <-get_questions(questions, author = auth[i])
  
  if (length(group_ind) > 0) {
    for (j in 1:length(groups)) {
      group_counter <- group_counter + 1
      temp <- append(temp, get_questions(groups[[j]], group_counter, auth[i]))
    }
  }
  all_questions <- append(all_questions, temp)
  all_groups <- append(all_groups, groups)
}
names(all_questions) <- seq_along(all_questions)
names(all_groups) <- seq_along(all_groups)

group_stems <- unname(unlist(lapply(all_groups, function(x) paste(x[grep("^\\s*\\[multi\\]", x):grep("\\{\\s*$", x)], collapse="\\n\\n"))))
group_stems <- gsub("^\\s*\\[multi\\]\\s*|\\s*\\{\\s*$", "", group_stems)

# unname(unlist(lapply(all_groups, function(x) x[1][grep("^\\s*\\[multi\\]", x[1]):grep("\\{$", x[1])])))
# group_stems <- gsub("\\n", "\\n\\n", group_stems, fixed = T)

all_q_tib <- tibble(
  q_num = unlist(lapply(all_questions, function(x) x$q_num)),
  week = unlist(lapply(all_questions, function(x) x$week)),
  diff = factor(unlist(lapply(all_questions, function(x) x$diff)),
                levels = c("S", "E", "M", "D"),
                labels = c("Simple", "Easy", "Medium", "Difficult")),
  author = unlist(lapply(all_questions, function(x) x$author)),
  stem = unlist(lapply(all_questions, function(x) x$stem)),
  opts = unlist(lapply(all_questions, function(x) x$opts), recursive = F),
  correct = unlist(lapply(all_questions, function(x) x$correct)),
  group =  unlist(lapply(all_questions, function(x) x$group)),
  group_stem = group_stems[group]
)

all_q_tib %>%
  ggplot(aes(x = week)) +
  geom_bar()

all_q_tib %>%
  ggplot(aes(x = diff)) +
  geom_bar()

all_q_tib %>%
  ggplot(aes(x = week, fill = diff)) +
  geom_bar(position = "dodge2")



all_q_tib$paper <- 0
paper_qu <- all_q_tib %>%
  filter(is.na(group)) # %>%
  # group_by(diff) %>%
  # filter(q_num %in% sample(q_num, floor(length(q_num)/2)))

# temporary line to pick all multi Qs and add single ones till there's 50 Qs - REWORK for cases when there's more than 50 multi Qs
pick_single <- 50 - (nrow(all_q_tib) - nrow(paper_qu))
paper_qu <- paper_qu[sample(nrow(paper_qu), pick_single), ]

all_q_tib$paper[all_q_tib$q_num %in% paper_qu$q_num] <- 1

# randomise
paper_qu <- paper_qu[sample(nrow(paper_qu)), ]




paper <- c(
  "---",
  "title: 'Candidate Number'",
  "subtitle: '\\ '",
  "author: '\\ '",
  "date: '`r module_code`'",
  "---",
  "")

paper <- c(paper, pretty_inline_num)

for (i in seq_along(ff)) {
  code_chunks_file <- sub("q", "code", ff[i])
  if (file.exists(code_chunks_file))
    paper <- c(paper, paste0("<!-- ", auth[i], "'s code starts here -->"),
               readLines(code_chunks_file), "")
}

for (i in 1:nrow(paper_qu)) {
  paper <- c(paper, "", format_q(paper_qu[i, ], i, T))
}

for (g in seq_along(group_stems)) {
  gr_stem <- unlist(strsplit(group_stems[g], "\\n", fixed = T))
  chunk_lims <- grep("```", gr_stem) + c(-1, 1)
  # insert empty lines outside of code chunks
  if (length(chunk_lims) > 0) {
    ind <- c(2:chunk_lims[1], chunk_lims[2]:length(gr_stem))
    gr_stem[ind] <- sub("^$", "\\\\n\\\\n\\\\ \\\\n\\\\n", gr_stem[ind])
    gr_stem <- unlist(strsplit(gr_stem, "\\n", fixed = T))
  } else {
    gr_stem <- sub("^$", "\\\\n\\\\n\\\\ \\\\n\\\\n", gr_stem)
    gr_stem <- unlist(strsplit(gr_stem, "\\n", fixed = T))
  }
  paper <- c(paper, "", "\\ ", "", gr_stem, "", "\\ ", "")
  temp <- all_q_tib %>% filter(group == g)
  for (q in 1:nrow(temp)) {
    i <- i + 1
    paper <- c(paper, format_q(temp[q, ], i, T))
  }
}

out_file <- out_file_name

if (!file.exists(out_file)) file.create(out_file)

writeLines(paper, out_file)


rmarkdown::render(out_file,
                  output_format = exam_paper_document(
                    output_format =  "word",
                    reference_docx = ref_docx,
                    # df_print = "kable",
                    fig_height = 4,
                    fig_width = 5))

# restore default chunk opts
knitr::opts_chunk$set(default_chunk_opts)
